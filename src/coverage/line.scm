(define-module coverage.line
  (use util.match)
  (use gauche.vm.insn)
  (use gauche.sequence)
  (use srfi-13)

  (use ya.port)
  (use ya.read)
  (use coverage.util)
  (export coverage-setup coverage-finish test-output line-out))

(select-module coverage.line)

(define (coverage-setup)
  (set! load-from-port ya-load-from-port)
  (set! read ya-read)
  (add-ya-read-after-hook read-after)
  (add-each-ya-read-after-hook each-read-after)
  #t)

(define (coverage-finish)
  (let1 directory (get-coverage-directory)
    (hash-table-for-each
      file-table
      (lambda (filename coverage-table)
        (print filename)
        (output-coverage-file directory filename (coverage-body filename coverage-table))))))

(define-constant indent-width 8)

(define (coverage-body filename coverage-table)
  (string-join
    (map-with-index
      (lambda (idx line)
        (let1 c (hash-table-get coverage-table (+ idx 1) #f)
          (cond
            [(not c)
             (string-append (make-string indent-width #\space) "| " line)]
            [(zero? c)
             (string-append "<font color=red>" (make-string (- indent-width 3) #\space) "0..| " line "</font>")]
            [else
              (string-append (string-pad (number->string c) (- indent-width 2)) "..| " line)])))
      (call-with-input-file filename port->string-list))
    "\n"))

(define ya-load-from-port
  (let1 *original-load-from-port* load-from-port
    (lambda (port . args)
      (apply *original-load-from-port* (cons (wrap-ya-port port) args)))))

(define srcinfo-table (make-hash-table 'eq?))

(define (individual-object? sexp)
  (not (or
         (fixnum? sexp)
         (symbol? sexp)
         (keyword? sexp)
         (boolean? sexp)
         (eof-object? sexp)
         (undefined? sexp)
         )))

(define (each-read-after sexp srcinfo)
  (when (and srcinfo (individual-object? sexp))
    (hash-table-put! srcinfo-table sexp srcinfo))
  sexp)

(define-macro (call/gi func . args)
  `((with-module gauche.internal ,func) ,@args))

(define file-table (make-hash-table 'string=?))

(define (line results filename line)
  (hash-table-update! (hash-table-get file-table filename) line (pa$ + 1))
  (apply values results))

(define line. ((with-module gauche.internal make-identifier) 'line (current-module) '()))

(define (read-after toplevel-sexp)
  (scan-expression
    toplevel-sexp
    (lambda (sexp src)
      (if-let1 srcinfo (hash-table-get srcinfo-table src #f)
        (let ([filename (car srcinfo)]
              [line (cadr srcinfo)])
          (let1 coverage-table (or (hash-table-get file-table filename #f)
                                 (rlet1 table (make-hash-table)
                                   (hash-table-put! file-table filename table)))
            (hash-table-put! coverage-table line 0))
          `(,line. (values->list ,sexp) ,filename ,line))
        sexp))))

