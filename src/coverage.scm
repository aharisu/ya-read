(use gauche.process)
(use gauche.parseopt)
(use srfi-13)

(use coverage.line)

(define (usage)
  (exit 1 "Usage:\
          \n  gosh coverage.scm  --tests=TESTS\
          \n  gosh coverage.scm  test-file1 test-file2 ...\
          \n  options:\
          \n    --tests<path>   Testing load file list.\
          \n    --basedir<path> Testing working directory path.\
          \n    --show-detail   Show test detail message. "
  ))

(define (main args)
  (let-args (cdr args)
    ([test-file "test-file=s"]
     [test-basedir "basedir=s"]
     [show-detail "show-detail"]
     [tests "tests=s"]
     [else (_ . _) (usage)]
     . rest)
    (let1 test-basedir (or test-basedir (sys-getcwd))
      (cond
        [tests
          (execution-all-tests (car args) (load-tests-file tests) test-basedir show-detail)]
        [test-file
          (test-exec test-file test-basedir show-detail)]
        [(not (null? rest))
          (execution-all-tests (car args) rest test-basedir show-detail)]
        [else (usage)]))))

(define (load-tests-file tests)
  (filter-map
    (lambda (line)
      (let1 line (string-trim line)
        (if (string-prefix? ";" line)
          #f
          (string-trim-right
            (if-let1 index (string-index line #\;)
              (string-take line index)
              line)))))
    (call-with-input-file tests port->string-list)))

(define (execution-all-tests self-filename test-files test-basedir show-detail)
  (let loop ([test-files test-files])
    (cond
      [(null? test-files)
       (coverage-finish)]
      [(fork-test-process self-filename (car test-files) *load-path* test-basedir show-detail)
       => (lambda (ret)
            (loop (cdr test-files)))]
      [else #f])))

(define (fork-test-process self-filename file load-path test-basedir show-detail)
  (let* ([p (run-process `(gosh
                            ,@(fold-right
                                (lambda (path acc) (cons "-I" (cons path acc)))
                                '()
                                load-path)
                            ,self-filename
                            "--test-file" ,file 
                            "--basedir" ,test-basedir
                            ,@(if show-detail '("--show-detail") '())
                            )
                         :output :pipe
                         :wait #t
                         )]
         [out-port (process-output p)]
         [report (read out-port)])
    (coverage-report-load report)
    (let1 result-line (read-line out-port)
      (print result-line)
      (display (port->string out-port))
      #t)))

(define (test-exec filename test-basedir show-detail)
  (sys-chdir test-basedir)
  (coverage-setup)
  (let1 msg
    (let ([null-port (open-output-string)]
          [msg-port (open-output-string)])
      (with-ports
        (current-input-port)
        (if show-detail (current-error-port) null-port)
        msg-port
        (lambda ()
          (coverage-test-execution filename)
          (get-output-string msg-port))))
    (coverage-repot-output (current-output-port))
    (display msg)))

