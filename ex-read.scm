(define-module ex-read
  (use gauche.parameter)
  (use ex-port)
  (use ex-trie)
  (export ex-read))

(select-module ex-read)

(define *char-kind-table*
  (make-parameter
    '((#\tab . whitespace)
      (#\newline . whitespace)
      (#\x0b . whitespace) ;0x0b is VT
      (#\page . whitespace)
      (#\return . whitespace)
      (#\space . whitespace)
      (#\| . multi-escape)
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

(define (read-single-quote port)
  (list 'quote (do-read #f port)))


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
      (trie-put! trie "'" (cons-reader-macro :term read-single-quote))
      )))

;-------------------------
; reader entry point
;-------------------------

(define (ex-read :optional port)
  (do-read #f port))

(define delim-sym (gensym))

(define (do-read delim port)
  (let1 result (do-read-first delim port (*char-kind-table*) (*reader-table*))
    (cond
      [(eq? result :eof)
       (eof-object)]
      [(eq? result :dlim)
       elim-sym]
      [(string? result)
       (read-symbol-or-number result)]
      [(reader-macro? result)
       (let1 macro-result (values->list ((get-reader-macro-fun result) port))
         (if (null? macro-result)
           (do-read delim port)
           (car macro-result)))]
      [else
        ;;error result format '(error-msg)
        (raise (make <read-error>
                     :message (car result)
                     :port (slot-ref port 'port)
                     :line (slot-ref port 'line)
                     :column (slot-ref port 'col)))])))

(define (do-read-first delim port kind-table reader-table)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       :eof]
      [(eq? ch delim)
       :delim]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-constituent delim port ch kind-table reader-table reader-table
                                '() '() '())]
          [(whitespace)
           (do-read-first delim port kind-table reader-table)]
          [(multi-escape)
           (error "todo multi-escape")]
          [(illegal)
           (error "todo illegal")])])))

(define (do-read-loop delim port kind-table reader-table reader-table-cont
                      buffer non-term-macro-candidates
                      pending-chars candidate-reader-macro)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       (do-read-end port buffer pending-chars candidate-reader-macro)]
      [(eq? ch delim)
       (do-read-end port buffer (cons ch pending-chars) candidate-reader-macro)]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-constituent delim port ch kind-table reader-macro reader-table-cont
                                buffer non-term-macro-candidates
                                pending-chars
                                )]
          [(whitespace)
           (do-read-end port buffer pending-chars candidate-reader-macro)]
          [(multi-escape)
           (error "todo multi-escape")]
          [(illegal)
           (error "todo illegal")])])))

(define (do-read-loop-no-readermacro delim port kind-table reader-table buffer non-term-macro-candidates)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       (list->string (reverse buffer))]
      [(eq? ch delim)
       (ungetc ch port)
       (list->string (reverse buffer))]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-non-term-candidate
             :no-readermacro
             delim port ch kind-table reader-table #f
             (cons ch buffer) non-term-macro-candidates
             '() #f
             )]
          [(whitespace)
           (list->string (reverse buffer))]
          [(multi-escape)
           (error "todo multi-escape")]
          [(illegal)
           ])])))

(define (do-read-constituent delim port ch kind-table reader-table reader-table-cont
                             buffer non-term-macro-candidates
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
             (do-read-end port '() ungotten-chars result-reader-macro))
           (do-read-non-term-candidate
             :normal
             delim port ch kind-table reader-table cont
             (append (cons ch pending-chars) buffer)
             non-term-macro-candidates
             '() reader-macro
             )))]
      [(is-a? result <trie>)
       ;;will probably match
       (do-read-non-term-candidate
         :normal
         delim port ch kind-table reader-table result
         buffer
         non-term-macro-candidates
         (cons ch pending-chars) #f
         )]
      [else
        ;; no match
        (do-read-non-term-candidate
          :no-readermacro
          delim port ch kind-table reader-table #f
          (append (cons ch pending-chars) buffer)
          non-term-macro-candidates
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
       (values reader-table (cons ch pending-chars))]
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
          [(multi-escape)
           (values reader-macro (cons ch pending-chars))]
          [(illegal)
           (error "todo illegal")])])))

(define (do-read-non-term-candidate next-type delim port ch kind-table reader-table reader-table-cont buffer non-term-macro-candidates
                                    pending-chars candidate-reader-macro
                                    )
  (if (and (null? buffer) (null? pending-chars))
    (if (eq? next-type :normal)
      (do-read-loop delim port kind-table reader-table reader-table-cont
                    '() non-term-macro-candidates
                    '() candidate-reader-macro
                    )
      (do-read-loop-no-readermacro delim port kind-table reader-table
                                   '() non-term-macro-candidates
                                   ))
    ;;;;
    (let1 new-non-term-macro-candidates
      (let loop ([candidates (append non-term-macro-candidates (list (cons reader-table 1)))]
                 [acc '()])
        (if (null? candidates)
          acc
          (let1 ret (loop (cdr candidates) acc)
            (if (integer? ret)
              ret
              (let1 result (trie-common-prefix-continuation (caar candidates) ch)
                (cond
                  [(pair? result)
                   (let ([cont (car result)]
                         [macro (cdr result)])
                     (if (term-reader-macro? macro)
                       (cdar candidates) ;;return char-count
                       (cons (cons cont (+ (cdar candidates) 1)) ret)))]
                  [(is-a? result <trie>)
                   (cons (cons result (+ (cdar candidates) 1)) ret)]
                  [else
                    ret]))))))
      (cond
        [(integer? new-non-term-macro-candidates)
         (receive (ungetc-chars result-chars) (split-at buffer new-non-term-macro-candidates)
           (do-read-end port result-chars (append ungetc-chars pending-chars) candidate-reader-macro))]
        [(eq? next-type :normal)
         (do-read-loop delim port kind-table reader-table reader-table-cont
                       buffer new-non-term-macro-candidates
                       pending-chars candidate-reader-macro
                       )]
        [else
          (do-read-loop-no-readermacro delim port kind-table reader-table 
                                       buffer new-non-term-macro-candidates
                                       )]))))

(define (do-read-end port buffer pending-chars reader-macro)
  ;; ungetc all pending-chars
  (for-each (cut ungetc <> port) pending-chars)
  (if reader-macro
    reader-macro
    (list->string (reverse buffer))))

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
