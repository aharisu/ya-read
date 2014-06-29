(define-module ex-read
  (use gauche.parameter)
  (use ex-port)
  (use ex-trie)
  (export ex-read))

(select-module ex-read)

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

(define delim-sym (gensym))

(define (read-quote ctx port)
  (list 'quote (do-read #f ctx port)))

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
             [(eq? last-ch delim-sym)
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

(define (read-vector ctx port)
  (list->vector (read-list #\) ctx port)))

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

(define (read-character ctx port)
  (let1 first-ch (read-char port)
    (if (eof-object? first-ch)
      (read-error "EOF encountered in a character literal" port)
      (let1 char-str (do-read-loop-no-readermacro #f ctx port (*char-kind-table*) (*reader-table*) 
                                                  (list first-ch) '())
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
         (if (not (or (eof-object? ch) (char=? #\newline ch)))
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
              (for-each (cut ungetc <> port) pending-chars)
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
      (trie-put! trie "(" (cons-reader-macro :term (pa$ read-list #\))))
      (trie-put! trie ")" (cons-reader-macro :term read-close-paren))
      (trie-put! trie "[" (cons-reader-macro :term (pa$ read-list #\])))
      (trie-put! trie "]" (cons-reader-macro :term read-close-bracket))
      (trie-put! trie "\"" (cons-reader-macro :term (pa$ read-string #f)))
      (trie-put! trie "#*\"" (cons-reader-macro :term (pa$ read-string #t)))
      (trie-put! trie "#t" (cons-reader-macro :non-term read-true))
      (trie-put! trie "#f" (cons-reader-macro :non-term read-false))
      (trie-put! trie ";" (cons-reader-macro :term read-line-comment))
      (trie-put! trie "#;" (cons-reader-macro :term read-sexp-comment))
      (trie-put! trie "#|" (cons-reader-macro :term (pa$ read-block-comment '(#\# #\|) '(#\| #\#))))
      (trie-put! trie "#\\" (cons-reader-macro :term read-character))
      (trie-put! trie "#(" (cons-reader-macro :term read-vector))
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

(define (ex-read :optional port)
  (do-read #f empty-context port))

(define (do-read delim ctx port)
  (let1 result (do-read-first delim ctx port (*char-kind-table*) (*reader-table*))
    (cond
      [(eq? result :eof)
       (eof-object)]
      [(eq? result :delim)
       delim-sym]
      [(string? result)
       (read-symbol-or-number result)]
      [(reader-macro? result)
       (let1 macro-result (values->list ((get-reader-macro-fun result) ctx port))
         (if (null? macro-result)
           (do-read delim ctx port)
           (car macro-result)))]
      [else
        ;;error result format '(error-msg)
        (read-error (car result) port)])))

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
                                '() '() '())]
          [(whitespace)
           (do-read-first delim ctx port kind-table reader-table)]
          [(illegal)
           (error "todo illegal")])])))

(define (do-read-loop delim ctx port kind-table reader-table reader-table-cont
                      buffer term-macro-candidates
                      pending-chars candidate-reader-macro)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       (do-read-end ctx port buffer pending-chars candidate-reader-macro term-macro-candidates)]
      [(eq? ch delim)
       (do-read-end ctx port buffer (cons ch pending-chars) candidate-reader-macro term-macro-candidates)]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-constituent delim ctx port ch kind-table reader-table reader-table-cont
                                buffer term-macro-candidates
                                pending-chars
                                )]
          [(whitespace)
           (do-read-end ctx port buffer pending-chars candidate-reader-macro term-macro-candidates)]
          [(illegal)
           (error "todo illegal")])])))

(define (do-read-loop-no-readermacro delim ctx port kind-table reader-table buffer term-macro-candidates)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       (do-read-end ctx port buffer [] #f term-macro-candidates)]
      [(eq? ch delim)
       (ungetc ch port)
       (do-read-end ctx port buffer [] #f term-macro-candidates)]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-term-candidate
             :no-readermacro
             delim ctx port ch kind-table reader-table #f
             (cons ch buffer) term-macro-candidates
             '() #f
             )]
          [(whitespace)
           (do-read-end ctx port buffer [] #f term-macro-candidates)]
          [(illegal)
           ])])))

(define (do-read-constituent delim ctx port ch kind-table reader-table reader-table-cont
                             buffer term-macro-candidates
                             pending-chars)
  (let1 result (trie-common-prefix-continuation reader-table-cont ch)
    (cond
      [(pair? result)
       ;;match key and will probably match
       (let ([cont (car result)]
             [reader-macro (cdr result)])
         (if (term-reader-macro? reader-macro)
           (receive (result-reader-macro ungotten-chars)
             (find-longest-term-macro delim port kind-table cont reader-macro '())
             (do-read-end ctx port '() ungotten-chars result-reader-macro term-macro-candidates))
           (do-read-term-candidate
             :normal
             delim ctx port ch kind-table reader-table cont
             (append (cons ch pending-chars) buffer)
             term-macro-candidates
             '() reader-macro
             )))]
      [(is-a? result <trie>)
       ;;will probably match
       (do-read-term-candidate
         :normal
         delim ctx port ch kind-table reader-table result
         buffer
         term-macro-candidates
         (cons ch pending-chars) #f
         )]
      [else
        ;; no match
        (do-read-term-candidate
          :no-readermacro
          delim ctx port ch kind-table reader-table #f
          (append (cons ch pending-chars) buffer)
          term-macro-candidates
          '() #f
          )])))

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
           (values reader-macro pending-chars)]
          [(illegal)
           (error "todo illegal")])])))

(define (do-read-term-candidate next-type delim ctx port ch kind-table reader-table reader-table-cont buffer term-macro-candidates
                                    pending-chars candidate-reader-macro
                                    )
  (if (and (null? buffer) (null? pending-chars))
    (if (eq? next-type :normal)
      (do-read-loop delim ctx port kind-table reader-table reader-table-cont
                    '() term-macro-candidates
                    '() candidate-reader-macro
                    )
      (do-read-loop-no-readermacro delim ctx port kind-table reader-table
                                   '() term-macro-candidates
                                   ))
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
         (do-read-end ctx port buffer pending-chars #f new-term-macro-candidates)]
        [(eq? next-type :normal)
         (do-read-loop delim ctx port kind-table reader-table reader-table-cont
                       buffer new-term-macro-candidates
                       pending-chars candidate-reader-macro
                       )]
        [else
          (do-read-loop-no-readermacro delim ctx port kind-table reader-table 
                                       buffer new-term-macro-candidates
                                       )]))))

(define (do-read-end ctx port buffer pending-chars reader-macro term-macro-candidates)
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
    (if reader-macro
      reader-macro
      (list->string (reverse buffer)))))

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
    (string->symbol str)))
