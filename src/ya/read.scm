(define-module ya.read
  (use gauche.parameter)
  (use gauche.uvector)
  (use gauche.interpolate)
  (use srfi-14)
  (use ya.port)
  (use ya.trie)
  (export ya-read ya-read-rec
    add-ya-read-after-hook
    add-each-ya-read-after-hook
    add-char-kind add-reader-macro
    ))

(select-module ya.read)

(define *orginal-read* read)

(define *char-kind-table*
  (make-parameter
    (list
      (cons char-set:whitespace 'whitespace))))

(define (add-char-kind char-spec char-type :optional (head? #f))
  (if (memq char-type '(whitespace constituent illegal))
    (if (or (char? char-spec) (char-set? char-spec))
      (*char-kind-table*
        (if head?
          (cons (cons char-spec char-type) (*char-kind-table*))
          (append (*char-kind-table*) (list (cons char-spec char-type)))))
      (errorf "char-spec require #<char> or #<char-set>"))
    (errorf "char-type require one of the 'whitespace, 'constituent or 'illegal")))

(define cons-reader-macro cons)
(define (reader-macro? obj)
  (and (pair? obj)
    (memq (get-reader-macro-type obj) '(term right-term non-term))))

(define get-reader-macro-type car)
(define get-reader-macro-fun cdr)

(define (constract-reader-context flags kind-table reader-table)
  (list flags kind-table reader-table))

(define reader-context-flags car)
(define reader-context-kind-table cadr)
(define reader-context-reader-table caddr)

(define (reader-context-add-flag ctx flag)
  (constract-reader-context
    (add-flag (reader-context-flags ctx) flag)
    (reader-context-kind-table ctx)
    (reader-context-reader-table ctx)))

;-------------------------
; reader macro functions
;-------------------------

(define (read-error msg port)
  (raise (make <read-error>
               :message msg
               :port (slot-ref port 'port)
               :line (slot-ref port 'line)
               :column (slot-ref port 'col))))

(define (eof-error literal-type port)
  (read-error #`"EOF encountered in a ,|literal-type| literal" port))

(define delim-sym (gensym))

(define (read-quote ctx port)
  (list 'quote (ya-read-rec ctx port)))

(define (read-quasiquote ctx port)
  (list 'quasiquote (ya-read-rec ctx port)))

(define (read-unquote ctx port)
  (list 'unquote (ya-read-rec ctx port)))

(define (read-unquote-splicing ctx port)
  (list 'unquote-splicing (ya-read-rec ctx port)))

(define (read-close-paren ctx port)
  (read-error "unexpected close paren ')'" port))

(define (read-close-bracket ctx port)
  (read-error "unexpected close paren ']'" port))

(define (read-list closer ctx port)
  (let loop ([pair '()])
    (let1 result (ya-read-rec-with-closer closer ctx port)
      (cond
        [(eof-object? result)
         (read-error "unexpected end-of-file while reading a list" port)]
        [(eq? result delim-sym)
         (reverse pair)]
        [(eq? result '|.|)
         (let1 last-ch (ya-read-rec-with-closer closer ctx port)
           (cond
             [(eof-object? last-ch)
              (read-error "unexpected end-of-file while reading a list" port)]
             [(eq? last-ch closer)
              (read-error "bad dot syntax" port)]
             [else
               (let1 close-ch (ya-read-rec-with-closer closer ctx port)
                 (cond
                   [(eof-object? close-ch)
                    (read-error "unexpected end-of-file while reading a list" port)]
                   [(eq? close-ch delim-sym)
                    (reverse pair last-ch)]
                   [else
                     (read-error "bad dot syntax" port)]))]))]
        [else
          (loop (cons result pair))]))))

(define (read-vector to->vector ctx port)
  (to->vector (read-list #\) ctx port)))

(define (read-string incomplete? ctx port)
  (define (eof-error str-acc)
    (write-char #\" str-acc)
    (read-error (format "EOF encountered in a string literal: ~a"
                        (get-output-string str-acc))
                port))
  (let1 str-acc (open-output-string)
    (when incomplete?
      (write-char #\# str-acc)
      (write-char #\* str-acc))
    (write-char #\" str-acc)
    (let loop ()
      (let1 ch (read-char port)
        (cond
          [(eof-object? ch)
           (eof-error str-acc)]
          [(char=? ch #\")
           (write-char #\" str-acc)]
          [(char=? ch #\\)
           (write-char #\\ str-acc)
           (let1 escape-ch (read-char port)
             (if (eof-object? escape-ch)
               (eof-error str-acc)
               (write-char escape-ch str-acc)))
           (loop)]
          [else
            (write-char ch str-acc)
            (loop)])))
    (*orginal-read* (open-input-string (get-output-string str-acc)))))

(define (read-string-interpolate legacy? ctx port)
  (let1 sexp (ya-read-rec ctx port)
    (if (eof-object? sexp)
      (eof-error "string-interpolate" port)
      (string-interpolate sexp legacy?))))

(define (read-string-interpolate-newer ctx port)
  (ungetc #\" port)
  (read-string-interpolate #f ctx port))

(define (read-character ctx port)
  (let1 first-ch (read-char port)
    (if (eof-object? first-ch)
      (eof-error "character" port)
      (let1 char-str (car (do-read-loop-no-readermacro #f
                                                       (reader-context-flags ctx)
                                                       port
                                                       (reader-context-kind-table ctx)
                                                       (reader-context-reader-table ctx)
                                                       (list first-ch) '()
                                                       #f))
        (*orginal-read* (open-input-string (string-append "#\\" char-str)))))))

(define (read-true ctx port)
  #t)

(define (read-false ctx port)
  #f)

(define (read-line-comment ctx port)
  (let loop ([ch (read-char port)])
    (cond
      [(or (eof-object? ch) (char=? ch #\newline)) ]
      [(char=? ch #\return)
       (let1 next-ch (read-char port)
         (if (not (or (eof-object? next-ch) (char=? #\newline next-ch)))
           (ungetc next-ch port)))]
      [else (loop (read-char port))]))
  ;;no value
  (values))

(define (read-sexp-comment ctx port)
  ;;read s-expression to skip
  (ya-read-rec (reader-context-add-flag ctx :skip) port)
  (values))

(define (read-block-comment opener closer ctx port)
  (define (match-test char-list)
    (let match-test-loop ([char-list char-list]
                          [pending-chars '()])
      (if (null? char-list)
        #t
        (let1 ch (read-char port)
          (if (char=? ch (car char-list))
            (match-test-loop (cdr char-list) (cons ch pending-chars))
            (begin
              (for-each (cut ungetc <> port) (cons ch pending-chars))
              #f))))))
  (let loop ([nest-level 1])
    (let1 ch (read-char port)
      (cond
        [(eof-object? ch) ]
        [(char=? ch (car opener))
         (if (match-test (cdr opener))
           (loop (+ nest-level 1))
           (loop nest-level))]
        [(char=? ch (car closer))
         (if (match-test (cdr closer))
           (unless (= nest-level 1)
             (loop (- nest-level 1)))
           (loop nest-level))]
        [else
          (loop nest-level)])))
  (values))

(define (read-charset ctx port)
  (let1 acc (open-output-string)
    (write-char #\# acc)
    (write-char #\[ acc)
    (let loop ([nest 0])
      (let1 ch (read-char port)
        (cond
          [(eof-object? ch)
           (eof-object? "char-set")]
          [(char=? #\] ch)
           (write-char #\] acc)
           (unless (zero? nest) 
             (loop (- nest 1)))]
          [(char=? #\[ ch)
           (write-char #\[ acc)
           (loop (+ nest 1))]
          [(char=? #\\ ch)
           (write-char #\\ acc)
           (let1 next-ch (read-char port)
             (if (eof-object? next-ch)
               (eof-error "char-set")
               (begin
                 (write-char next-ch acc)
                 (loop nest))))]
          [else
            (write-char ch acc)
            (loop nest)])))
    (*orginal-read* (open-input-string (get-output-string acc)))))

(define (read-regexp ctx port)
  (let1 acc (open-output-string)
    (write-char #\# acc)
    (write-char #\/ acc)
    (let loop ()
      (let1 ch (read-char port)
        (cond
          [(eof-object? ch)
           (eof-object? "regexp")]
          [(char=? #\/ ch)
           (write-char #\/ acc)
           (let1 last-ch (read-char port)
             (if (eq? #\i last-ch)
               (write-char #\i acc)
               (ungetc last-ch port)))]
          [(char=? #\\ ch)
           (write-char #\\ acc)
           (let1 next-ch (read-char port)
             (if (eof-object? next-ch)
               (eof-error "regexp")
               (begin
                 (write-char next-ch acc)
                 (loop))))]
          [else
            (write-char ch acc)
            (loop)])))
    (*orginal-read* (open-input-string (get-output-string acc)))))

(define (read-multi-escape ctx port)
  (let1 acc (open-output-string)
    (let loop ()
      (let1 ch (read-char port)
        (cond
          [(eof-object? ch)
           (eof-object? "multi-escape")]
          [(char=? #\| ch) ]
          [(char=? #\\ ch)
           (let1 next-ch (read-char port)
             (if (eof-object? next-ch)
               (eof-error "multi-escape")
               (begin
                 (write-char next-ch acc)
                 (loop))))]
          [else
            (write-char ch acc)
            (loop)])))
    (string->symbol (get-output-string acc))))

(define (read-word ctx port)
  (let ([acc (open-output-string)]
        [kind-table (*char-kind-table*)])
    (let loop ()
      (let1 ch (read-char port)
        (unless (eof-object? ch)
          (case (char-kind ch kind-table)
            [(constituent)
             (write-char ch acc)
             (loop)]
            [(whitespace) ]
            [(illegal)
             (error "todo illegal")]))))
    (get-output-string acc)))

(define (read-hash-bang ctx port)
  (if (and
        (= (slot-ref port 'line) 1)
        (= (slot-ref port 'col) 2)
        (let1 ch (read-char port)
          (or 
            (char=? ch #\/)
            (char=? ch #\space)
            (begin
              (ungetc ch port)
              #f))))
    (read-line-comment ctx port)
    (begin
      ;;TODO handling reader directive
      (read-word ctx port)
      (undefined))))

(define (read-reader-constractor ctx port)
  (let1 list (read-list #\) ctx port)
    (if (null? list)
      (read-error (format "bad #,-form: ~a" list) port)
      (if-let1 ctor (%get-reader-ctor (car list))
        (apply (car ctor) (cdr list))
        (read-error (format "unknown #,-key: ~a" (car list)) port)))))

(define *reader-table*
  (make-parameter
    (rlet1 trie (make-trie)
      (trie-put! trie "'" (cons-reader-macro 'term read-quote))
      (trie-put! trie "`" (cons-reader-macro 'term read-quasiquote))
      (trie-put! trie "," (cons-reader-macro 'term read-unquote))
      (trie-put! trie ",@" (cons-reader-macro 'term read-unquote-splicing))
      (trie-put! trie "(" (cons-reader-macro 'term (pa$ read-list #\))))
      (trie-put! trie ")" (cons-reader-macro 'term read-close-paren))
      (trie-put! trie "[" (cons-reader-macro 'term (pa$ read-list #\])))
      (trie-put! trie "]" (cons-reader-macro 'term read-close-bracket))
      (trie-put! trie "\"" (cons-reader-macro 'term (pa$ read-string #f)))
      (trie-put! trie "#*\"" (cons-reader-macro 'right-term (pa$ read-string #t)))
      (trie-put! trie "#`" (cons-reader-macro 'right-term (pa$ read-string-interpolate #t)))
      (trie-put! trie "#\"" (cons-reader-macro 'right-term read-string-interpolate-newer))
      (trie-put! trie "#t" (cons-reader-macro 'non-term read-true))
      (trie-put! trie "#f" (cons-reader-macro 'non-term read-false))
      (trie-put! trie ";" (cons-reader-macro 'term read-line-comment))
      (trie-put! trie "#;" (cons-reader-macro 'right-term read-sexp-comment))
      (trie-put! trie "#|" (cons-reader-macro 'right-term (pa$ read-block-comment '(#\# #\|) '(#\| #\#))))
      (trie-put! trie "#\\" (cons-reader-macro 'right-term read-character))
      (trie-put! trie "#(" (cons-reader-macro 'right-term (pa$ read-vector list->vector)))
      (trie-put! trie "#u8(" (cons-reader-macro 'right-term (pa$ read-vector list->u8vector)))
      (trie-put! trie "#u16(" (cons-reader-macro 'right-term (pa$ read-vector list->u16vector)))
      (trie-put! trie "#u32(" (cons-reader-macro 'right-term (pa$ read-vector list->u32vector)))
      (trie-put! trie "#u64(" (cons-reader-macro 'right-term (pa$ read-vector list->u64vector)))
      (trie-put! trie "#s16(" (cons-reader-macro 'right-term (pa$ read-vector list->s16vector)))
      (trie-put! trie "#s32(" (cons-reader-macro 'right-term (pa$ read-vector list->s32vector)))
      (trie-put! trie "#s64(" (cons-reader-macro 'right-term (pa$ read-vector list->s64vector)))
      (trie-put! trie "#f16(" (cons-reader-macro 'right-term (pa$ read-vector list->f16vector)))
      (trie-put! trie "#f32(" (cons-reader-macro 'right-term (pa$ read-vector list->f32vector)))
      (trie-put! trie "#f64(" (cons-reader-macro 'right-term (pa$ read-vector list->f64vector)))
      (trie-put! trie "#[" (cons-reader-macro 'right-term read-charset))
      (trie-put! trie "#/" (cons-reader-macro 'right-term read-regexp))
      (trie-put! trie "|" (cons-reader-macro 'term read-multi-escape))
      (trie-put! trie "#!" (cons-reader-macro 'right-term read-hash-bang))
      (trie-put! trie "#,(" (cons-reader-macro 'right-term read-reader-constractor))
      )))

(define (add-reader-macro dispatch-string macro-type reader-macro)
  (if (memq macro-type '(term right-term non-term))
    (*reader-table* (apply trie '()
                           (cons
                             (cons dispatch-string (cons-reader-macro macro-type reader-macro))
                             (trie->list (*reader-table*)))))
    (errorf "macro-type require one of the 'term, 'right-term or 'non-term")))

;-------------------------
; reader entry point
;-------------------------

(define empty-flags [])

(define (add-flag flags flag)
  (if (memq flag flags)
    flag
    (cons flag flags)))

(define (has-flag flags flag)
  (memq flags flags))

(define *read-after-hook* '())
(define *each-read-after-hook* '())

(define (add-ya-read-after-hook hook-proc)
  (set! *read-after-hook* (cons hook-proc *read-after-hook*)))

(define (add-each-ya-read-after-hook hook-proc)
  (set! *each-read-after-hook* (cons hook-proc *each-read-after-hook*)))

(define (ya-read :optional (port (current-input-port)))
  (if (ya-wraped-port? port)
    (let1 exp (do-read #f empty-flags port (*char-kind-table*) (*reader-table*))
      (if (eof-object? exp)
        exp
        (fold
          (lambda (hook-proc exp) (hook-proc exp)) 
          exp
          *read-after-hook*)))
    (*orginal-read* port)))

(define (do-read-after sexp src-info)
  (let1 sexp 
    (fold
      (lambda (hook sexp) (hook sexp src-info))
      (if (and (pair? sexp) src-info)
        (rlet1 ex-pair ((with-module gauche.internal extended-cons) (car sexp) (cdr sexp))
          ((with-module gauche.internal pair-attribute-set!) ex-pair 'source-info src-info))
        sexp)
      *each-read-after-hook*)))

(define (ya-read-rec ctx port)
  (ya-read-rec-with-closer #f ctx port))

(define (ya-read-rec-with-closer closer ctx port)
  (do-read closer
           (reader-context-flags ctx)
           port
           (reader-context-kind-table ctx)
           (reader-context-reader-table ctx)
           ))

(define (do-read delim flags port kind-table reader-table)
  (let1 result (do-read-first delim flags port  kind-table reader-table)
    (cond
      [(eq? result :eof)
       (eof-object)]
      [(eq? result :delim)
       delim-sym]
      [(string? (car result))
       (do-read-after
         (read-symbol-or-number (car result))
         (cdr result))]
      [(reader-macro? (car result))
       (let1 macro-result (values->list ((get-reader-macro-fun (car result))
                                         (constract-reader-context flags kind-table reader-table)
                                         port))
         (if (null? macro-result)
           (do-read delim flags port kind-table reader-table)
           (do-read-after
             (car macro-result)
             (cdr result))))])))

(define (do-read-first delim flags port kind-table reader-table)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       :eof]
      [(eq? ch delim)
       :delim]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-constituent delim flags port ch kind-table reader-table reader-table
                                '() '() '()
                                (source-info port))]
          [(whitespace)
           (do-read-first delim flags port kind-table reader-table)]
          [(illegal)
           (error "todo illegal")])])))

(define (do-read-loop delim flags port kind-table reader-table reader-table-cont
                      buffer term-macro-candidates
                      pending-chars candidate-reader-macro
                      src-info)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       (do-read-end flags port (append buffer pending-chars) '() candidate-reader-macro term-macro-candidates src-info)]
      [(eq? ch delim)
       (do-read-end flags port (append buffer pending-chars) (list ch) candidate-reader-macro term-macro-candidates src-info)]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-constituent delim flags port ch kind-table reader-table reader-table-cont
                                buffer term-macro-candidates
                                pending-chars
                                src-info
                                )]
          [(whitespace)
           (do-read-end flags port (append buffer pending-chars) (list ch) candidate-reader-macro term-macro-candidates src-info)]
          [(illegal)
           (error "todo illegal")])])))

(define (do-read-loop-no-readermacro delim flags port kind-table reader-table buffer term-macro-candidates
                                     src-info)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       (do-read-end flags port buffer [] #f term-macro-candidates src-info)]
      [(eq? ch delim)
       (ungetc ch port)
       (do-read-end flags port buffer [] #f term-macro-candidates src-info)]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-term-candidate
             :no-readermacro
             delim flags port ch kind-table reader-table #f
             (cons ch buffer) term-macro-candidates
             '() #f
             src-info)]
          [(whitespace)
           (do-read-end flags port buffer [] #f term-macro-candidates src-info)]
          [(illegal)
           ])])))

(define (do-read-constituent delim flags port ch kind-table reader-table reader-table-cont
                             buffer term-macro-candidates
                             pending-chars
                             src-info)
  (let1 result (trie-common-prefix-continuation reader-table-cont ch)
    (cond
      [(pair? result)
       ;;match key and will probably match
       (let ([cont (car result)]
             [reader-macro (cdr result)])
         (case (get-reader-macro-type reader-macro)
           [(term right-term)
            (receive (result-reader-macro ungotten-chars)
              (find-longest-term-macro delim port kind-table cont reader-macro '())
              (do-read-end flags port '() ungotten-chars result-reader-macro '() src-info))]
           [else
            (do-read-term-candidate
              :normal
              delim flags port ch kind-table reader-table cont
              (append (cons ch pending-chars) buffer)
              term-macro-candidates
              '() reader-macro
              src-info)]))]
      [(is-a? result <trie>)
       ;;will probably match
       (do-read-term-candidate
         :normal
         delim flags port ch kind-table reader-table result
         buffer
         term-macro-candidates
         (cons ch pending-chars) #f
         src-info)]
      [else
        ;; no match
        (do-read-term-candidate
          :no-readermacro
          delim flags port ch kind-table reader-table #f
          (append (cons ch pending-chars) buffer)
          term-macro-candidates
          '() #f
          src-info)])))

(define (find-longest-term-macro delim port kind-table reader-table-cont reader-macro pending-chars)
  (let loop ([ch (read-char port)]
             [reader-macro reader-macro]
             [reader-table-cont reader-table-cont]
             [pending-chars pending-chars])
    (cond
      [(eof-object? ch)
       (values reader-macro pending-chars)]
      [(eq? ch delim)
       (values reader-macro (cons ch pending-chars))]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (let1 result (trie-common-prefix-continuation reader-table-cont ch)
             (cond
               [(pair? result)
                (let ([cont (car result)]
                      [macro (cdr result)])
                  (case (get-reader-macro-type macro)
                    [(term right-term)
                     (loop (read-char port) macro cont '())]
                    [else
                     (loop (read-char port) reader-macro cont (cons ch pending-chars))]))]
               [(is-a? result <trie>)
                (loop (read-char port) reader-macro result (cons ch pending-chars))]
               [else
                 (values reader-macro (cons ch pending-chars))]))]
          [(whitespace)
           (values reader-macro (cons ch pending-chars))]
          [(illegal)
           (error "todo illegal")])])))

(define (do-read-term-candidate next-type delim flags port ch kind-table reader-table reader-table-cont buffer term-macro-candidates
                                    pending-chars candidate-reader-macro
                                    src-info)
  (if (and (null? buffer) (null? pending-chars))
    (if (eq? next-type :normal)
      (do-read-loop delim flags port kind-table reader-table reader-table-cont
                    '() term-macro-candidates
                    '() candidate-reader-macro
                    src-info)
      (do-read-loop-no-readermacro delim flags port kind-table reader-table
                                   '() term-macro-candidates
                                   src-info))
    ;;;;
    (let1 new-term-macro-candidates
      (filter-map
        (lambda (candidate)
          (if (integer? candidate)
            (+ candidate 1 )
            (let1 result (trie-common-prefix-continuation (car candidate) ch)
              (cond
                [(pair? result)
                 (let ([cont (car result)]
                       [macro (cdr result)])
                   (case (get-reader-macro-type macro)
                     [(right-term non-term) (cons cont (+ (cdr candidate) 1))]
                     [else (cdr candidate)]))] ;;return char-count
                [(is-a? result <trie>)
                 (cons result (+ (cdr candidate) 1))]
                [else
                  #f]))))
        (append term-macro-candidates (list (cons reader-table 1))))
      (cond
        [(and (not (null? new-term-macro-candidates)) (every integer? new-term-macro-candidates))
         (do-read-end flags port buffer pending-chars #f new-term-macro-candidates src-info)]
        [(eq? next-type :normal)
         (do-read-loop delim flags port kind-table reader-table reader-table-cont
                       buffer new-term-macro-candidates
                       pending-chars candidate-reader-macro
                       src-info)]
        [else
          (do-read-loop-no-readermacro delim flags port kind-table reader-table 
                                       buffer new-term-macro-candidates
                                       src-info)]))))

(define (do-read-end flags port buffer pending-chars reader-macro term-macro-candidates src-info)
  (receive (buffer pending-chars reader-macro)
    (let1 split-pos (fold
                      (lambda (char-count max-char-count)
                        (if (integer? char-count)
                          (max char-count max-char-count)
                          max-char-count))
                      0
                      term-macro-candidates)
      (if (zero? split-pos)
        (values buffer pending-chars reader-macro)
        (receive (ungetc-chars result-chars) (split-at buffer split-pos)
          (values result-chars (append ungetc-chars pending-chars) #f))))
    ;; ungetc all pending-chars
    (for-each (cut ungetc <> port) pending-chars)
    (cons
      (if reader-macro
        reader-macro
        (list->string (reverse buffer)))
      src-info)))

(define (char-kind ch kind-table)
  (or
    (any
      (lambda (kind-spec)
        (let1 spec (car kind-spec)
          (if (char-set? spec)
            (if (char-set-contains? spec ch)
              (cdr kind-spec)
              #f)
            (if (char=? spec ch)
              (cdr kind-spec)
              #f))))
      kind-table)
    (if (char<? ch #\space)
      'illegal
      'constituent)))

(define (read-symbol-or-number str)
  (or
    (string->number str)
    (if (or (zero? (string-length str))
          (not (char=? #\: (string-ref str 0))))
      (string->symbol str)
      (make-keyword (substring str 1 (string-length str))))))
