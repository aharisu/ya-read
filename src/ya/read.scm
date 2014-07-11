(define-module ya.read
  (use gauche.parameter)
  (use gauche.uvector)
  (use gauche.interpolate)
  (use ya.port)
  (use ya.trie)
  (export ya-read
    add-ya-read-after-hook
    add-each-ya-read-after-hook
    ))

(select-module ya.read)

(define *orginal-read* read)

(define *char-kind-table*
  (make-parameter
    '((#\tab . whitespace)
      (#\newline . whitespace)
      (#\x0b . whitespace) ;0x0b is VT
      (#\page . whitespace)
      (#\return . whitespace)
      (#\space . whitespace)
      )))

(define cons-reader-macro cons)
(define (reader-macro? obj)
  (and (pair? obj)
    (let1 type (get-reader-macro-type obj)
      (or (eq? type :term) (eq? type :non-term)))))

(define get-reader-macro-type car)
(define get-reader-macro-fun cdr)

(define (term-reader-macro? reader-macro)
  (eq? (get-reader-macro-type reader-macro) :term))

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
  (list 'quote (do-read #f ctx port)))

(define (read-quasiquote ctx port)
  (list 'quasiquote (do-read #f ctx port)))

(define (read-unquote ctx port)
  (list 'unquote (do-read #f ctx port)))

(define (read-unquote-splicing ctx port)
  (list 'unquote-splicing (do-read #f ctx port)))

(define (read-close-paren ctx port)
  (read-error "unexpected close paren ')'" port))

(define (read-close-bracket ctx port)
  (read-error "unexpected close paren ']'" port))

(define (read-list closer ctx port)
  (let loop ([pair '()])
    (let1 result (do-read closer ctx port)
      (cond
        [(eof-object? result)
         (read-error "unexpected end-of-file while reading a list" port)]
        [(eq? result delim-sym)
         (reverse pair)]
        [(eq? result '|.|)
         (let1 last-ch (do-read closer ctx port)
           (cond
             [(eof-object? last-ch)
              (read-error "unexpected end-of-file while reading a list" port)]
             [(eq? last-ch closer)
              (read-error "bad dot syntax" port)]
             [else
               (let1 close-ch (do-read closer ctx port)
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
  (let1 sexp (do-read #f ctx port)
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
      (let1 char-str (car (do-read-loop-no-readermacro #f ctx port (*char-kind-table*) (*reader-table*) 
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
  (do-read #f (add-context ctx :skip) port)
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

(define *reader-table*
  (make-parameter
    (rlet1 trie (make-trie
                  list
                  (cut assoc-ref <> <> #f char=?)
                  (lambda (t k v)
                    (if v
                      (assoc-set! t k v char=?)
                      (alist-delete! k t char=?)))
                  (lambda (t f s) 
                    (fold (lambda (node acc) (f (car node) (cdr node) acc)) s t))
                  )
      (trie-put! trie "'" (cons-reader-macro :term read-quote))
      (trie-put! trie "`" (cons-reader-macro :term read-quasiquote))
      (trie-put! trie "," (cons-reader-macro :term read-unquote))
      (trie-put! trie ",@" (cons-reader-macro :term read-unquote-splicing))
      (trie-put! trie "(" (cons-reader-macro :term (pa$ read-list #\))))
      (trie-put! trie ")" (cons-reader-macro :term read-close-paren))
      (trie-put! trie "[" (cons-reader-macro :term (pa$ read-list #\])))
      (trie-put! trie "]" (cons-reader-macro :term read-close-bracket))
      (trie-put! trie "\"" (cons-reader-macro :term (pa$ read-string #f)))
      (trie-put! trie "#*\"" (cons-reader-macro :term (pa$ read-string #t)))
      (trie-put! trie "#`" (cons-reader-macro :term (pa$ read-string-interpolate #t)))
      (trie-put! trie "#\"" (cons-reader-macro :term read-string-interpolate-newer))
      (trie-put! trie "#t" (cons-reader-macro :non-term read-true))
      (trie-put! trie "#f" (cons-reader-macro :non-term read-false))
      (trie-put! trie ";" (cons-reader-macro :term read-line-comment))
      (trie-put! trie "#;" (cons-reader-macro :term read-sexp-comment))
      (trie-put! trie "#|" (cons-reader-macro :term (pa$ read-block-comment '(#\# #\|) '(#\| #\#))))
      (trie-put! trie "#\\" (cons-reader-macro :term read-character))
      (trie-put! trie "#(" (cons-reader-macro :term (pa$ read-vector list->vector)))
      (trie-put! trie "#u8(" (cons-reader-macro :term (pa$ read-vector list->u8vector)))
      (trie-put! trie "#u16(" (cons-reader-macro :term (pa$ read-vector list->u16vector)))
      (trie-put! trie "#u32(" (cons-reader-macro :term (pa$ read-vector list->u32vector)))
      (trie-put! trie "#u64(" (cons-reader-macro :term (pa$ read-vector list->u64vector)))
      (trie-put! trie "#s16(" (cons-reader-macro :term (pa$ read-vector list->s16vector)))
      (trie-put! trie "#s32(" (cons-reader-macro :term (pa$ read-vector list->s32vector)))
      (trie-put! trie "#s64(" (cons-reader-macro :term (pa$ read-vector list->s64vector)))
      (trie-put! trie "#f16(" (cons-reader-macro :term (pa$ read-vector list->f16vector)))
      (trie-put! trie "#f32(" (cons-reader-macro :term (pa$ read-vector list->f32vector)))
      (trie-put! trie "#f64(" (cons-reader-macro :term (pa$ read-vector list->f64vector)))
      (trie-put! trie "#[" (cons-reader-macro :term read-charset))
      (trie-put! trie "#/" (cons-reader-macro :term read-regexp))
      (trie-put! trie "|" (cons-reader-macro :term read-multi-escape))
      (trie-put! trie "#!" (cons-reader-macro :term read-hash-bang))
      )))

;-------------------------
; reader entry point
;-------------------------

(define empty-context [])

(define (add-context ctx flag)
  (if (memq flag ctx)
    flag
    (cons flag ctx)))

(define (has-context ctx flag)
  (memq flag ctx))

(define *read-after-hook* '())
(define *each-read-after-hook* '())

(define (add-ya-read-after-hook hook-proc)
  (set! *read-after-hook* (cons hook-proc *read-after-hook*)))

(define (add-each-ya-read-after-hook hook-proc)
  (set! *each-read-after-hook* (cons hook-proc *each-read-after-hook*)))

(define (ya-read :optional (port (current-input-port)))
  (if (ya-wraped-port? port)
    (let1 exp (do-read #f empty-context port)
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

(define (do-read delim ctx port)
  (let1 result (do-read-first delim ctx port (*char-kind-table*) (*reader-table*))
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
       (let1 macro-result (values->list ((get-reader-macro-fun (car result)) ctx port))
         (if (null? macro-result)
           (do-read delim ctx port)
           (do-read-after
             (car macro-result)
             (cdr result))))])))

(define (do-read-first delim ctx port kind-table reader-table)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       :eof]
      [(eq? ch delim)
       :delim]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-constituent delim ctx port ch kind-table reader-table reader-table
                                '() '() '()
                                (source-info port))]
          [(whitespace)
           (do-read-first delim ctx port kind-table reader-table)]
          [(illegal)
           (error "todo illegal")])])))

(define (do-read-loop delim ctx port kind-table reader-table reader-table-cont
                      buffer term-macro-candidates
                      pending-chars candidate-reader-macro
                      src-info)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       (do-read-end ctx port buffer pending-chars candidate-reader-macro term-macro-candidates src-info)]
      [(eq? ch delim)
       (do-read-end ctx port buffer (cons ch pending-chars) candidate-reader-macro term-macro-candidates src-info)]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-constituent delim ctx port ch kind-table reader-table reader-table-cont
                                buffer term-macro-candidates
                                pending-chars
                                src-info
                                )]
          [(whitespace)
           (do-read-end ctx port buffer (cons ch pending-chars) candidate-reader-macro term-macro-candidates src-info)]
          [(illegal)
           (error "todo illegal")])])))

(define (do-read-loop-no-readermacro delim ctx port kind-table reader-table buffer term-macro-candidates
                                     src-info)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       (do-read-end ctx port buffer [] #f term-macro-candidates src-info)]
      [(eq? ch delim)
       (ungetc ch port)
       (do-read-end ctx port buffer [] #f term-macro-candidates src-info)]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-term-candidate
             :no-readermacro
             delim ctx port ch kind-table reader-table #f
             (cons ch buffer) term-macro-candidates
             '() #f
             src-info)]
          [(whitespace)
           (do-read-end ctx port buffer [] #f term-macro-candidates src-info)]
          [(illegal)
           ])])))

(define (do-read-constituent delim ctx port ch kind-table reader-table reader-table-cont
                             buffer term-macro-candidates
                             pending-chars
                             src-info)
  (let1 result (trie-common-prefix-continuation reader-table-cont ch)
    (cond
      [(pair? result)
       ;;match key and will probably match
       (let ([cont (car result)]
             [reader-macro (cdr result)])
         (if (term-reader-macro? reader-macro)
           (receive (result-reader-macro ungotten-chars)
             (find-longest-term-macro delim port kind-table cont reader-macro '())
             (do-read-end ctx port '() ungotten-chars result-reader-macro term-macro-candidates src-info))
           (do-read-term-candidate
             :normal
             delim ctx port ch kind-table reader-table cont
             (append (cons ch pending-chars) buffer)
             term-macro-candidates
             '() reader-macro
             src-info)))]
      [(is-a? result <trie>)
       ;;will probably match
       (do-read-term-candidate
         :normal
         delim ctx port ch kind-table reader-table result
         buffer
         term-macro-candidates
         (cons ch pending-chars) #f
         src-info)]
      [else
        ;; no match
        (do-read-term-candidate
          :no-readermacro
          delim ctx port ch kind-table reader-table #f
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
                  (if (term-reader-macro? macro)
                    (loop (read-char port) macro cont '())
                    (loop (read-char port) reader-macro cont (cons ch pending-chars))))]
               [(is-a? result <trie>)
                (loop (read-char port) reader-macro result (cons ch pending-chars))]
               [else
                 (values reader-macro (cons ch pending-chars))]))]
          [(whitespace)
           (values reader-macro (cons ch pending-chars))]
          [(illegal)
           (error "todo illegal")])])))

(define (do-read-term-candidate next-type delim ctx port ch kind-table reader-table reader-table-cont buffer term-macro-candidates
                                    pending-chars candidate-reader-macro
                                    src-info)
  (if (and (null? buffer) (null? pending-chars))
    (if (eq? next-type :normal)
      (do-read-loop delim ctx port kind-table reader-table reader-table-cont
                    '() term-macro-candidates
                    '() candidate-reader-macro
                    src-info)
      (do-read-loop-no-readermacro delim ctx port kind-table reader-table
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
                   (if (term-reader-macro? macro)
                     (cdr candidate) ;;return char-count
                     (cons cont (+ (cdr candidate) 1))))]
                [(is-a? result <trie>)
                 (cons result (+ (cdr candidate) 1))]
                [else
                  #f]))))
        (append term-macro-candidates (list (cons reader-table 1))))
      (cond
        [(and (not (null? new-term-macro-candidates)) (every integer? new-term-macro-candidates))
         (do-read-end ctx port buffer pending-chars #f new-term-macro-candidates src-info)]
        [(eq? next-type :normal)
         (do-read-loop delim ctx port kind-table reader-table reader-table-cont
                       buffer new-term-macro-candidates
                       pending-chars candidate-reader-macro
                       src-info)]
        [else
          (do-read-loop-no-readermacro delim ctx port kind-table reader-table 
                                       buffer new-term-macro-candidates
                                       src-info)]))))

(define (do-read-end ctx port buffer pending-chars reader-macro term-macro-candidates src-info)
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

(define (extra-whitespace ch)
  (cond
    [(char<? #\x3000 ch) #f]
    [(char<? ch #\x2000)
     (if (or (char=? ch  #\x00A0)
           (char=? ch #\x1680)
           (char=? ch #\x180E))
       'whitespace
       #f)]
    [(char<=? ch #\x200A) ; 16#2000 ... 16#200A are all Zs's
     'whitespace]
    [else
      (if (or (char=? ch #\x3000)  ; Zs NO-BREAK SPACE
            (char=? ch #\x202F)    ; Zs OGHAM SPACE MARK
            (char=? ch #\x205F))   ; Zs MONGOLIAN VOWEL SEPARATOR
        'whitespace
        #f)]))

(define (char-kind ch kind-table)
  (or
    (assq-ref kind-table ch)
    (extra-whitespace ch)
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
