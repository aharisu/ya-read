;;;
;;; read.scm
;;;
;;; MIT License
;;; Copyright 2014 aharisu
;;; All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;
;;;
;;; aharisu
;;; foo.yobina@gmail.com
;;;

(define-module ya.read
  (use gauche.parameter)
  (use gauche.uvector)
  (use gauche.interpolate)
  (use gauche.vport)
  (use srfi-14)
  (use ya.trie)
  (export ya-read ya-read-rec
    add-ya-read-after-hook
    add-each-ya-read-after-hook
    add-char-kind
    copy-reader-table add-reader-macro set-reader-table!
    wrap-ya-port ungetc source-info
    ))

(select-module ya.read)

(define *orginal-read* read)

(define *char-kind-table*
  (make-parameter
    (list
      (cons char-set:whitespace 'whitespace))))

(define (add-char-kind char-spec char-type :optional (head? #f))
  (if (memq char-type '(whitespace constituent skip illegal))
    (if (or (char? char-spec) (char-set? char-spec))
      (*char-kind-table*
        (if head?
          (cons (cons char-spec char-type) (*char-kind-table*))
          (append (*char-kind-table*) (list (cons char-spec char-type)))))
      (errorf "char-spec require #<char> or #<char-set>"))
    (errorf "char-type require one of the 'whitespace, 'constituent, 'skip' or 'illegal")))

(define cons-reader-macro cons)
(define (reader-macro? obj)
  (and (pair? obj)
    (memq (get-reader-macro-type obj) '(term right-term non-term))))

(define get-reader-macro-type car)
(define get-reader-macro-fun cdr)

(define-macro (with-ctx port ctx . body)
  `(unwind-protect
     (begin
       (add-ctx! ,port ,ctx)
       ,@body)
     (clear-ctx! ,port ,ctx)))

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

(define (set-source-info port exp src-info)
  (if (and (pair? exp) src-info)
    (rlet1 ex-pair ((with-module gauche.internal extended-cons) (car exp) (cdr exp))
      ((with-module gauche.internal pair-attribute-set!) ex-pair 'source-info src-info))
    exp))

(define delim-sym (gensym))

(define (read-quote port)
  (let* ([src-info (get-start-source-info port)]
         [item (ya-read-rec port)])
    (if (eof-object? item)
      (eof-error "quote" port)
      (rlet1 ret (set-source-info port (list 'quote item) src-info)
        (when (read-reference? item)
          (read-reference-push port (cdr ret) #f))))))

(define (read-quasiquote port)
  (list 'quasiquote (ya-read-rec port)))

(define (read-unquote port)
  (list 'unquote (ya-read-rec port)))

(define (read-unquote-splicing port)
  (list 'unquote-splicing (ya-read-rec port)))

(define (read-close-paren port)
  (read-error "unexpected close paren ')'" port))

(define (read-close-bracket port)
  (read-error "unexpected close paren ']'" port))

(define (read-list closer port)
  (let1 src-info (get-start-source-info port)
    (receive (ret has-ref?) (read-list-internal closer port) 
      (rlet1 ret (set-source-info port ret src-info)
        (if has-ref?
          (read-reference-push port ret #f))
        ret))))

(define (read-list-eof-error port src-info)
  (if src-info
    (read-error #`"unexpected end-of-file while reading a list (starting from line:,(cadr src-info) col:,(caddr src-info))" port)
    (read-error "unexpected end-of-file while reading a list" port)))

(define (read-list-internal closer port)
  (let1 src-info (get-start-source-info port)
    (let loop ([pair '()]
               [ref-seen #f])
      (let1 result (ya-read-rec-with-closer closer port)
        (cond
          [(eof-object? result)
           (read-list-eof-error port src-info)]
          [(eq? result delim-sym)
           (values (reverse pair) ref-seen)]
          [(eq? result '|.|)
           (let1 last-ch (ya-read-rec-with-closer closer port)
             (cond
               [(eof-object? last-ch)
                (read-list-eof-error port src-info)]
               [(eq? last-ch closer)
                (read-error "bad dot syntax" port)]
               [else
                 (let1 close-ch (ya-read-rec-with-closer closer port)
                   (cond
                     [(eof-object? close-ch)
                      (read-list-eof-error port src-info)]
                     [(eq? close-ch delim-sym)
                      (values (reverse pair last-ch)
                              (if (read-reference? last-ch)
                                #t
                                ref-seen))]
                     [else
                       (read-error "bad dot syntax" port)]))]))]
          [else
            (loop (cons result pair)
                  (if (read-reference? result)
                    #t
                    ref-seen))])))))

(define (read-vector to->vector port)
  (receive (ret has-ref?) (read-list-internal #\) port)
    (rlet1 ret (to->vector ret)
      (if has-ref? 
        (read-reference-push port ret #f)))))

(define (read-string incomplete? port)
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

(define (read-string-interpolate legacy? port)
  (let1 sexp (ya-read-rec port)
    (if (eof-object? sexp)
      (eof-error "string-interpolate" port)
      (string-interpolate sexp legacy?))))

(define (read-string-interpolate-newer port)
  (ungetc #\" port)
  (read-string-interpolate #f port))

(define (read-character port)
  (let1 first-ch (read-char port)
    (if (eof-object? first-ch)
      (eof-error "character" port)
      (let1 char-str (car (do-read-loop-no-readermacro #f
                                                       port
                                                       (slot-ref port 'kind-table)
                                                       (slot-ref port 'reader-table)
                                                       (list first-ch) '()))
        (*orginal-read* (open-input-string (string-append "#\\" char-str)))))))

(define (read-true port)
  #t)

(define (read-false port)
  #f)

(define (read-line-comment port)
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

(define (read-sexp-comment port)
  ;;read s-expression to skip
  (with-ctx
    port :skip
    (ya-read-rec port))
  (values))

(define (read-block-comment opener closer port)
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

(define (read-charset port)
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

(define (read-regexp port)
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

(define (read-multi-escape port)
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

(define (read-word port)
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
            [(skip)
             (loop)]
            [(illegal)
             (raise-illegal-char port ch)]))))
    (get-output-string acc)))

(define (read-hash-bang port)
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
    (read-line-comment port)
    (begin
      ;;TODO handling reader directive
      (read-word port)
      (undefined))))

(define (read-reader-constractor port)
  (receive (list has-ref?) (read-list-internal #\) port)
    (cond
      [(has-ctx? port :skip)
       #f]
      [(null? list)
       (read-error (format "bad #,-form: ~a" list) port)]
      [(%get-reader-ctor (car list))
       => (lambda (ctor) 
            (rlet1 ret (apply (car ctor) (cdr list))
              (if has-ref?
                (read-reference-push port ret (cdr ctor)))))]
      [else 
        (read-error (format "unknown #,-key: ~a" (car list)) port)])))

(define (read-debug-print port)
  `(debug-print ,(ya-read-rec port)))

;;
;; Read reference
;;

(define unbound-sym (gensym "unbound"))
(define reference-sym (gensym "reference"))

(define make-read-reference
  (pa$ list reference-sym unbound-sym))

(define (read-reference? ref)
  (and (pair? ref) (eq? (car ref) reference-sym)))

(define (read-reference-realized? ref)
  (not (eq? (cadr ref) unbound-sym)))

(define read-reference-value-get cadr)

(define (read-reference-value-set! ref value)
  (set-car! (cdr ref) value))

(define (read-reference-push port obj finisher)
  (slot-set! port 'ref-pending (acons obj finisher (slot-ref port 'ref-pending))))

(define (read-reference-flush port)
  (for-each
    (lambda (entry)
      (let ([obj (car entry)]
            [finisher (cdr entry)])
        (cond
          [finisher
            (finisher obj)]
          [(pair? obj)
           (let loop ([ep obj])
             (if (read-reference? (car ep))
               (set-car! ep (read-reference-value-get (car ep))))
             (cond
               [(null? (cdr ep))
                ;;do nothing
                ]
               ;;in case we have (... . #N#)
               [(read-reference? (cdr ep))
                (set-cdr! ep (read-reference-value-get (cdr ep)))]
               [else
                 (loop (cdr ep))]))]
          [(vector? obj)
           (let1 size (vector-length obj)
             (let loop ([i 0])
               (when (< i len)
                 (let1 ep (vector-ref obj i)
                   (if (read-reference? ep)
                     (vector-set! obj i (read-reference-value-get ep))))
                 (loop (+ i 1)))))]
          [else
            (read-error "read-context-flush: recursive reference only supported with vector and lists" port) ])))
    (slot-ref port 'ref-pending)))

(define (read-reference refnum port)
  (receive (refnum ch) (let loop ([refnum refnum]
                                  [ch (read-char port)])
                         (cond
                           [(eof-object? ch)
                            (eof-error "reference" port)]
                           [(char-numeric? ch)
                            (loop
                              (+ (* refnum 10) (digit->integer ch))
                              (read-char port))]
                           [(or (char=? ch #\#) (char=? ch #\=))
                            (values refnum ch)]
                           [else
                             (read-error
                               (format
                                 "invalid reference form (must be either #digits# or #digits=) : #~a~a" 
                                 refnum ch)
                               port)]))
    (if (char=? #\# ch)
      ;;#N.*# pattern
      (let1 e (if-let1 table (slot-ref port 'ref-table)
                (hash-table-get table refnum unbound-sym)
                unbound-sym)
        (when (eq? unbound-sym e)
          (read-error
            (format "invalid reference number in #~a=" refnum)
            port))
        (if (and (read-reference? e) (read-reference-realized? e))
          (read-reference-value-get e)
          e))
      ;;#N.*= pattern
      (let ([table (if-let1 ref-table (slot-ref port 'ref-table)
                     ref-table
                     (rlet1 table (make-hash-table 'eqv?)
                       (slot-set! port 'ref-table table)))]
            [ref (make-read-reference)])
        (unless (eq? (hash-table-get table refnum unbound-sym) unbound-sym)
          (read-error
            (format "duplicate back-reference number in #~a=" refnum)
            port))
        ;;ref-register
        (hash-table-put! table refnum ref)
        (let1 val (ya-read-rec port)
          ;;an edge case: #0=#0#
          (when (eq? ref val)
            (read-error 
              (format "interminate read reference: #~a=#~a#"
                      refnum refnum)
              port))
          (read-reference-value-set! ref val)
          val)))))

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
      (trie-put! trie "#?=" (cons-reader-macro 'right-term read-debug-print))
      ;;read reference macro
      (trie-put! trie "#0" (cons-reader-macro 'right-term (pa$ read-reference 0)))
      (trie-put! trie "#1" (cons-reader-macro 'right-term (pa$ read-reference 1)))
      (trie-put! trie "#2" (cons-reader-macro 'right-term (pa$ read-reference 2)))
      (trie-put! trie "#3" (cons-reader-macro 'right-term (pa$ read-reference 3)))
      (trie-put! trie "#4" (cons-reader-macro 'right-term (pa$ read-reference 4)))
      (trie-put! trie "#5" (cons-reader-macro 'right-term (pa$ read-reference 5)))
      (trie-put! trie "#6" (cons-reader-macro 'right-term (pa$ read-reference 6)))
      (trie-put! trie "#7" (cons-reader-macro 'right-term (pa$ read-reference 7)))
      (trie-put! trie "#8" (cons-reader-macro 'right-term (pa$ read-reference 8)))
      (trie-put! trie "#9" (cons-reader-macro 'right-term (pa$ read-reference 9)))
      )))

(define (copy-reader-table :optional reader-table)
  (apply trie '() (trie->list (if (undefined? reader-table) (*reader-table*) reader-table))))

(define (add-reader-macro dispatch-string macro-type reader-macro
                          :optional reader-table)
  (if (memq macro-type '(term right-term non-term))
    (let1 table (if (undefined? reader-table)
                  (copy-reader-table) 
                  reader-table)
      (trie-put! table dispatch-string (cons-reader-macro macro-type reader-macro))
      (if (undefined? reader-table)
        (*reader-table* table)))
    (errorf "macro-type require one of the 'term, 'right-term or 'non-term")))

(define (set-reader-table! reader-table)
  (*reader-table* (copy-reader-table reader-table)))

;-------------------------
; reader entry point
;-------------------------

(define *read-after-hook* '())
(define *each-read-after-hook* '())

(define (add-ya-read-after-hook hook-proc)
  (set! *read-after-hook* (cons hook-proc *read-after-hook*)))

(define (add-each-ya-read-after-hook hook-proc)
  (set! *each-read-after-hook* (cons hook-proc *each-read-after-hook*)))

(define (ya-read :optional (port (current-input-port)))
  (if (ya-wraped-port? port)
    (begin
      (init-ctx! port (*char-kind-table*) (*reader-table*))
      (let1 exp (do-read #f port)
        (read-reference-flush port)
        (if (null? *read-after-hook*)
          exp
          (if (eof-object? exp)
            exp
            (fold
              (lambda (hook-proc exp) (hook-proc exp)) 
              exp
              *read-after-hook*)))))
    (*orginal-read* port)))

(define (do-read-after sexp src-info)
  (if (null? *each-read-after-hook*)
    sexp
    (fold
      (lambda (hook sexp) (hook sexp src-info))
      sexp
      *each-read-after-hook*)))

(define (ya-read-rec port)
  (ya-read-rec-with-closer #f port))

(define (ya-read-rec-with-closer closer port)
  (do-read closer port))

(define (do-read delim port)
  (let1 result (do-read-first
                 delim
                 (and (has-ctx? port :middle) (begin (clear-ctx! port :middle) #t))
                 port (slot-ref port 'kind-table) (slot-ref port 'reader-table))
    (cond
      [(eq? result :eof)
       (eof-object)]
      [(eq? result :delim)
       delim-sym]
      [(string? (car result))
       (when (cdr result)
         (add-ctx! port :middle))
       (do-read-after
         (read-symbol-or-number (car result))
         (get-start-source-info port))]
      [(reader-macro? (car result))
       (when (cdr result)
         (add-ctx! port :middle))
       (let* ([src-info (get-start-source-info port)]
              [macro-result (values->list ((get-reader-macro-fun (car result)) port))])
         (if (null? macro-result)
           (do-read delim port)
           (do-read-after (car macro-result) src-info)))])))

(define (do-read-first delim middle? port kind-table reader-table)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       :eof]
      [(eq? ch delim)
       :delim]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (save-start-source-info port)
           (do-read-constituent delim middle? port ch kind-table reader-table reader-table
                                '() '() '())]
          [(whitespace skip)
           (do-read-first delim middle? port kind-table reader-table)]
          [(illegal)
           (raise-illegal-char port ch)])])))

(define (do-read-loop delim middle? port kind-table reader-table reader-table-cont
                      buffer term-macro-candidates
                      pending-chars candidate-reader-macro)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       (do-read-end #f port (append buffer pending-chars) '() candidate-reader-macro term-macro-candidates)]
      [(eq? ch delim)
       (do-read-end #f port (append buffer pending-chars) (list ch) candidate-reader-macro term-macro-candidates)]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-constituent delim middle? port ch kind-table reader-table reader-table-cont
                                buffer term-macro-candidates
                                pending-chars)]
          [(whitespace)
           (do-read-end #f port (append buffer pending-chars) (list ch) candidate-reader-macro term-macro-candidates)]
          [(skip)
           (do-read-loop delim middle? port kind-table reader-table reader-table-cont
                         buffer term-macro-candidates
                         pending-chars candidate-reader-macro)]
          [(illegal)
           (raise-illegal-char port ch)])])))

(define (do-read-loop-no-readermacro delim port kind-table reader-table buffer term-macro-candidates)
  (let1 ch (read-char port)
    (cond
      [(eof-object? ch)
       (do-read-end #f port buffer [] #f term-macro-candidates)]
      [(eq? ch delim)
       (ungetc ch port)
       (do-read-end #f port buffer [] #f term-macro-candidates)]
      [else
        (case (char-kind ch kind-table)
          [(constituent)
           (do-read-term-candidate
             :no-readermacro
             delim #f port ch kind-table reader-table #f
             (cons ch buffer) term-macro-candidates
             '() #f)]
          [(whitespace)
           (do-read-end #f port buffer [] #f term-macro-candidates)]
          [(skip)
           (do-read-loop-no-readermacro delim port kind-table reader-table buffer term-macro-candidates)]
          [(illegal)
           (raise-illegal-char port ch)])])))

(define (do-read-constituent delim middle? port ch kind-table reader-table reader-table-cont
                             buffer term-macro-candidates
                             pending-chars)
  (let1 result (trie-common-prefix-continuation reader-table-cont ch)
    (cond
      [(pair? result)
       ;;match key and will probably match
       (let ([cont (car result)]
             [reader-macro (cdr result)])
         (if (memq (get-reader-macro-type reader-macro)
                   (if middle? '(term) '(term right-term)))
           (receive (result-reader-macro ungotten-chars)
             (find-longest-term-macro delim middle? port kind-table cont reader-macro '())
             (do-read-end #f port '() ungotten-chars result-reader-macro '()))
           (do-read-term-candidate
             :normal
             delim middle? port ch kind-table reader-table cont
             (append (cons ch pending-chars) buffer)
             term-macro-candidates
             '() reader-macro)))]
      [(is-a? result <trie>)
       ;;will probably match
       (do-read-term-candidate
         :normal
         delim middle? port ch kind-table reader-table result
         buffer
         term-macro-candidates
         (cons ch pending-chars) #f)]
      [else
        ;; no match
        (do-read-term-candidate
          :no-readermacro
          delim middle? port ch kind-table reader-table #f
          (append (cons ch pending-chars) buffer)
          term-macro-candidates
          '() #f)])))

(define (find-longest-term-macro delim middle? port kind-table reader-table-cont reader-macro pending-chars)
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
                  (if (memq (get-reader-macro-type macro)
                            (if middle? '(term) '(term right-term)))
                    (loop (read-char port) macro cont '())
                    (loop (read-char port) reader-macro cont (cons ch pending-chars))))]
               [(is-a? result <trie>)
                (loop (read-char port) reader-macro result (cons ch pending-chars))]
               [else
                 (values reader-macro (cons ch pending-chars))]))]
          [(whitespace)
           (values reader-macro (cons ch pending-chars))]
          [(skip)
           (loop (read-char port) reader-macro reader-table-cont pending-chars)]
          [(illegal)
           (raise-illegal-char port ch)])])))

(define (do-read-term-candidate next-type delim middle? port ch kind-table reader-table reader-table-cont buffer term-macro-candidates
                                    pending-chars candidate-reader-macro)
  (if (and (null? buffer) (null? pending-chars))
    (if (eq? next-type :normal)
      (do-read-loop delim middle? port kind-table reader-table reader-table-cont
                    '() term-macro-candidates
                    '() candidate-reader-macro)
      (do-read-loop-no-readermacro delim port kind-table reader-table
                                   '() term-macro-candidates))
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
         (do-read-end #t port buffer pending-chars #f new-term-macro-candidates)]
        [(eq? next-type :normal)
         (do-read-loop delim middle? port kind-table reader-table reader-table-cont
                       buffer new-term-macro-candidates
                       pending-chars candidate-reader-macro)]
        [else
          (do-read-loop-no-readermacro delim port kind-table reader-table 
                                       buffer new-term-macro-candidates)]))))

(define (do-read-end middle? port buffer pending-chars reader-macro term-macro-candidates)
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
      middle?)))

(define (raise-illegal-char port ch)
  (read-error (format "read illegal character: ~a" ch)
              port))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <ya-virtual-input-port>
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <ya-virtual-input-port> (<virtual-input-port>)
  (
   (line :init-value 1)
   (col :init-value 0)
   (ungotten :init-value '())
   (prev-ch :init-value 0)
   (col-list :init-value '())
   (port :init-keyword :port)
   (ctx :init-value '())
   (kind-table)
   (reader-table)
   (source-info :init-value #f)
   (ref-table)
   (ref-pending)
   ))

(define (init-ctx! port kind-table reader-table)
  (slot-set! port 'kind-table kind-table)
  (slot-set! port 'reader-table reader-table)
  (slot-set! port 'ref-table #f)
  (slot-set! port 'ref-pending '())
  )

(define (add-ctx! port ctx)
  (unless (memq ctx (slot-ref port 'ctx))
    (slot-set! port 'ctx (cons ctx (slot-ref port 'ctx)))))

(define (clear-ctx! port ctx)
  (slot-set! port 'ctx (delete ctx (slot-ref port 'ctx) eq?)))

(define (has-ctx? port ctx)
  (memq ctx (slot-ref port 'ctx)))

(define *original-port-name* port-name)

(set! port-name
  (lambda (port)
    (*original-port-name* 
      (if (is-a? port <ya-virtual-input-port>)
        (slot-ref port 'port)
        port))))

(define (wrap-ya-port port)
  (letrec ([ya-port (make <ya-virtual-input-port>
                          :getc (lambda () (ya-getc ya-port))
                          :port port
                          )])
    ya-port))

(define (ya-wraped-port? port)
  (is-a? port <ya-virtual-input-port>))

(define (ungetc ch port)
  (unless (eof-object? ch)
    (slot-set! port 'ungotten (cons ch (slot-ref port 'ungotten)))
    (let1 prev-ch (slot-ref port 'prev-ch)
      (cond
        [(and (eq? ch #\return) (eq? prev-ch (- (char->integer #\newline)))) ]
        [(or (eq? ch #\return) (eq? ch #\newline))
         (slot-set! port 'line (- (slot-ref port 'line) 1))
         (slot-set! port 'col (car (slot-ref port 'col-list)))
         (slot-set! port 'col-list (cdr (slot-ref port 'col-list)))]
        [else
          (slot-set! port 'col (- (slot-ref port 'col) 1))])
      (slot-set! port 'prev-ch (- (char->integer ch))))))

(define (ya-getc port)
  (let1 ungotten (slot-ref port 'ungotten)
    (rlet1 ch (if (null? ungotten)
                (read-char (slot-ref port 'port))
                (let ([ch (car ungotten)]
                      [remain (cdr ungotten)])
                  (slot-set! port 'ungotten remain)
                  ch))
      (let1 prev-ch (slot-ref port 'prev-ch)
        (cond
          [(and (eq? ch #\newline) (eq? prev-ch (char->integer #\return))) ]
          [(or (eq? ch #\newline) (eq? ch #\return))
           (slot-set! port 'line (+ (slot-ref port 'line) 1))
           (slot-set! port 'col-list (cons (slot-ref port 'col) (slot-ref port 'col-list)))
           (slot-set! port 'col 0)]
          [(not (eof-object? ch))
           (slot-set! port 'col (+ (slot-ref port ' col) 1))]))
      (if (eof-object? ch)
        (slot-set! port 'prev-ch ch)
        (slot-set! port 'prev-ch (char->integer ch))))))

(define (save-start-source-info port)
  (slot-set! port 'source-info (source-info port)))

(define (get-start-source-info port)
  (slot-ref port 'source-info))

(define (source-info port)
  (list
    (port-name (slot-ref port 'port))
    (slot-ref port 'line)
    (slot-ref port 'col)))
