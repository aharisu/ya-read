(define-module coverage.line
  (use util.match)
  (use gauche.vm.insn)
  (use gauche.sequence)
  (use srfi-13)
  (use file.util)

  (use ya.port)
  (use ya.read)
  (use coverage.util)
  (use debug.traverse)
  (export coverage-setup coverage-finish
    coverage-repot-output coverage-report-load 
    coverage-test-execution))

(select-module coverage.line)

(define (coverage-setup)
  (set! load-from-port ya-load-from-port)
  (set! read ya-read)
  (add-ya-read-after-hook read-after)
  (add-each-ya-read-after-hook each-read-after)
  #t)

(define (coverage-finish)
  ;;output all coverage file
  (hash-table-for-each
    file-table
    (lambda (filename coverage-table)
      (when (file-is-readable? filename)
        (output-coverage-file filename (coverage-body filename coverage-table)))))
  ;;output coverage summary file
  (let1 summary-list (hash-table-fold
                       file-table
                       (lambda (filename coverage-table acc)
                         (if (file-is-readable? filename)
                           (cons 
                             (list filename
                                   (length coverage-table) ; total
                                   (length (filter (.$ not zero?) (map cdr coverage-table)))) ;cover
                             acc)
                           acc))
                       '())
    (output-coverage-summary (sort
                               summary-list
                               (lambda (a b) (string<? (car a) (car b)))))))

(define (coverage-repot-output port)
  (display "(" port)
  (hash-table-for-each
    file-table
    (lambda (filename coverage-table)
      (when (file-is-readable? filename)
        (display "(" port)
        (write filename port)
        (for-each
          (lambda (line.count)
            (display "(" port)
            (write (car line.count) port)
            (display " " port)
            (write (cdr line.count) port)
            (display ")" port))
          coverage-table)
        (display ")" port))))
  (display ")" port))

(define (coverage-report-load report)
  (for-each
    (lambda (file-report)
      (hash-table-update!
        file-table
        (car file-report)
        (lambda (coverage-table)
          (fold
            (lambda (line/count coverage-table)
              (assq-set! coverage-table (car line/count)
                         (+ (cadr line/count)
                            (assq-ref coverage-table (car line/count) 0))))
            coverage-table
            (cdr file-report)))
        '()))
    report))

(define-constant indent-width 8)

(define (coverage-body filename coverage-table)
  (string-join
    (map-with-index
      (lambda (idx line)
        (let1 c (assq-ref coverage-table (+ idx 1) #f)
          (cond
            [(not c)
             (string-append (make-string indent-width #\space) "| " line)]
            [(zero? c)
             (string-append "<font color=red>" (make-string (- indent-width 3) #\space) "0..| " line "</font>")]
            [else
              (string-append (string-pad (number->string c) (- indent-width 2)) "..| " line)])))
      (call-with-input-file filename port->string-list))
    "\n"))

(define *coverage-ignore-file-list* '())

(define (coverage-test-execution load-filename)
  (if-let1 r ((with-module gauche.internal find-load-file)
              load-filename *load-path* *load-suffixes*)
    (set! *coverage-ignore-file-list*
      (cons (to-absolute-path (car r)) *coverage-ignore-file-list*)))
  (load load-filename))

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

(define (line results cell)
  (set-cdr! cell (+ (cdr cell) 1))
  (apply values results))

(define line. ((with-module gauche.internal make-identifier) 'line (current-module) '()))

(define (read-after toplevel-sexp)
  (scan-expression
    toplevel-sexp
    (lambda (sexp src)
      (if-let1 srcinfo (hash-table-get srcinfo-table src #f)
        (let ([filename (car srcinfo)]
              [line (cadr srcinfo)])
          (if (member (to-absolute-path filename) *coverage-ignore-file-list*)
            sexp
            (let1 cell (cons line 0)
              (hash-table-update!
                file-table
                filename
                (lambda (v) (cons cell v))
                '())
              `(,line. (values->list ,sexp) (quote ,cell)))))
        sexp))
    (lambda (sexp iform)
      sexp)
    ))

