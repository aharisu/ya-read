(use gauche.parseopt)
(use gauche.parameter)
(use gauche.threads)
(use gauche.vport)
(use srfi-13)
(use util.queue)

(use ya.port)
(use ya.read)
(use coverage.util)
(use debug.message-queue)


(define (usage)
  (exit 1 "Usage:\
          \n  gosh debug.scm gauche-script-file"
  ))

(define ya-load-from-port
  (let1 *original-load-from-port* load-from-port
    (lambda (port . args)
      (apply *original-load-from-port* (cons (wrap-ya-port port) args)))))

(define (setup)
  (set! load-from-port ya-load-from-port)
  (set! read ya-read)
  (add-ya-read-after-hook read-after)
  (add-each-ya-read-after-hook each-read-after)
  #t)

(define default-script-file #f)

(define (main args)
  (let-args (cdr args)
    ([else (_ . _) (usage)]
     . rest)
    (unless (null? rest)
      (set! default-script-file (car rest)))
    (setup)
    (user-interface-entry-point)
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug function interpolate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define break. (global-id 'break (current-module)))
(define begin. (global-id 'begin))
(define if. (global-id 'if))
(define quote. (global-id 'quote))

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

(define (read-after toplevel-sexp)
  (scan-expression
    toplevel-sexp
    (lambda (sexp src)
      (if (and (pair? sexp) script-loading?)
        (let* ([srcinfo (hash-table-get srcinfo-table src '(#f #f #f))]
               [cell (if (and (car srcinfo) (cadr srcinfo))
                       (get-break-point-cell (car srcinfo) (cadr srcinfo))
                       (cons #f (cons #f #f)))])
          `(,begin.
             (,break. (,quote. ,cell))
             ,sexp))
        sexp))))

;;;;;;;;;;;;;;;;;;;;
;; UI entry point
;;;;;;;;;;;;;;;;;;;;


(define (user-interface-entry-point)
  (let* ([msg-queue (make-message-queue)]
         [notify-queue (make-mtqueue)]
         [debug-thread (make-thread (pa$ debug-entry-point msg-queue notify-queue))])
    (thread-start! debug-thread)
    (let command-loop ()
      (display "gdb> ") (flush)
      (let1 line/or/notify (read-line/or/receive-notification notify-queue)
        (cond
          [(string? line/or/notify)
           (let* ([raw-line line/or/notify]
                  [line (string-trim-both raw-line)])
             (unless (string=? line "exit")
               (unless (string-null? line)
                 (exec-line raw-line msg-queue))
               (command-loop)))]
          [(not (eof-object? line/or/notify))
            (case (car line/or/notify)
              [(stop)
               (print "stop: " (cadr line/or/notify) ":" (caddr line/or/notify))]
              [(error)
               (print (cadr line/or/notify))]
              )
            (command-loop)])))))

(define (read-line/or/receive-notification notify-queue)
  (cond
    [(char-ready? (current-input-port))
     (read-line (current-input-port))]
    [(dequeue/wait! notify-queue 0.1 #f)
     => identity]
    [else (read-line/or/receive-notification notify-queue)]))

(define (exec-line raw-line msg-queue)
  (let1 tokens (string-split (string-trim-both raw-line) #[\s])
    (if-let1 cmd (get-ui-command (car tokens))
      (if ((ui-cmd-valid-arg cmd) (length (cdr tokens)))
        (apply (ui-cmd-exec cmd) (cons msg-queue (cdr tokens)))
        (print-err "Error: Invalid argument."))
      (print-err (format #f "Error: Unknown command [~a]." raw-line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Command definition
;;;;;;;;;;;;;;;;;;;;;;;;;

(define *ui-commands* '())

(define-macro (define-ui-cmd cmd valid-arg exec)
  `(set! *ui-commands* (acons (symbol->string (quote ,cmd))
                           (cons ,valid-arg ,exec)
                           *ui-commands*)))

(define (get-ui-command cmd) (assoc-ref *ui-commands* cmd #f))
(define ui-cmd-valid-arg car)
(define ui-cmd-exec cdr)
(define (get-ui-all-command)
  (cons "exit" (map car *ui-commands*)))

(define default-cmd-target-filename #f)

(define (break/clear-cmd set? msg-queue . args)
  (receive (filename line raw-line/function)
    (if (null? (cdr args))
      (let1 line/function (car args)
        (values default-cmd-target-filename
                (string->number line/function)
                line/function))
      (let ([filename (car args)]
            [line/function (cadr args)])
        (values filename
                (string->number line/function)
                line/function)))
    (if filename
      (set/unset-break-point filename line set?)
      (print-err (format #f "Error: Please specify a target file")))))

(define-ui-cmd
  break
  (^n (or (= n 1) (= n 2)))
  (pa$ break/clear-cmd #t))

(define-ui-cmd
  clear
  (^n (or (= n 1) (= n 2)))
  (pa$ break/clear-cmd #f))

(define-ui-cmd
  run
  (^n (or (zero? n) (= n 1)))
  (lambda (msg-queue . args)
    (if-let1 script-file (if (pair? args)
                           (car args)
                           default-script-file)
      (begin
        (set! default-cmd-target-filename script-file)
        (post msg-queue (list 'load script-file)))
      (print-err "Error: Specify a script file"))))

(define-ui-cmd
  exp
  (pa$ <= 1)
  (lambda (msg-queue . args)
    (let* ([line (string-append (string-join args " ") " ")]
           [index 0]
           [inport (make <virtual-input-port>
                         :getc (lambda ()
                                 (if (< index (string-length line))
                                   (begin0
                                     (string-ref line index)
                                     (inc! index))
                                   (read-char))))])
      (let1 e (read inport)
        (post msg-queue (list 'exp e))))))

(define-ui-cmd
  step
  zero?
  (lambda (msg-queue . args)
    (post msg-queue '(step))))

(define-ui-cmd
  next
  zero?
  (lambda (msg-queue . args)
    (post msg-queue '(next))))

(define-ui-cmd
  continue
  zero?
  (lambda (msg-queue . args)
    (post msg-queue '(continue))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Debug entry point
;;;;;;;;;;;;;;;;;;;;;;;

(define script-loading? #f)
(define break-escape-point #f)

(define stop-cond #f)

(define break-point-table (make-hash-table 'string=?))

(define (get-break-point-cell filename line)
  (let1 cells (hash-table-get break-point-table filename '())
    (if-let1 cell (find
                    (lambda (cell) (= (cadr cell) line))
                    cells)
      cell
      (rlet1 cell (cons #f (cons line filename))
        (hash-table-put! break-point-table filename (cons cell cells))))))

(define (set/unset-break-point filename line enable?)
  (set-car! (get-break-point-cell filename line) enable?))

(define (debug-entry-point msg-queue notify-queue)
  (let1 break-point-info (call/cc
                           (lambda (escape)
                             (set! break-escape-point escape)
                             #f))
    (when break-point-info
      (enqueue! notify-queue
                `(stop
                   ,(cadr break-point-info)   ;filename
                   ,(caddr break-point-info)  ;line
                   )))
    (let loop ()
      (let* ([msg (get/wait msg-queue)]
             [cmd (get-debug-command (car msg))])
        (if (debug-cmd-stopped? cmd)
          (if break-point-info
            ;;exec stopped debug command
            (apply (debug-cmd-exec cmd)
                   (append (list loop
                                 (car break-point-info)
                                 (cadr break-point-info)
                                 (caddr break-point-info))
                           (cdr msg)))
            (begin
              (enqueue! notify-queue '(error "The program is not stop"))
              (loop)))
          ;;exec normal debug command
          (apply (debug-cmd-exec cmd) (cons loop (cdr msg))))))))

(define (break cell)
  (when (let1 break? (car cell)
            (cond
              [(eq? break? #t) #t]
              [(not stop-cond) #f]
               ;equal file?
              [(string? stop-cond) ;next command
               (equal? stop-cond (cddr cell))]
              [else #t]))
    (call/cc
      (lambda (return-point)
        (break-escape-point (list
                              return-point
                              (cddr cell) ;filename
                              (cadr cell) ;line
                              ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug Command definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *debug-commands* '())

(define-macro (define-debug-cmd cmd stopped? exec)
  `(set! *debug-commands* (acons (quote ,cmd)
                           (cons ,stopped? ,exec)
                           *debug-commands*)))

(define (get-debug-command cmd) (assoc-ref *debug-commands* cmd))
(define debug-cmd-stopped? car)
(define debug-cmd-exec cdr)

(define-debug-cmd
  load
  #f
  (lambda (loop script-file)
    (dynamic-wind
      (lambda () (set! script-loading? #t))
      (lambda () (load script-file))
      (lambda () (set! script-loading? #f)))
    (loop)))

(define-debug-cmd
  exp
  #f
  (lambda (loop e)
    (eval e (current-module))
    (set! stop-cond #f)
    (loop)))

(define-debug-cmd
  step
  #t
  (lambda (loop break-continuation filename line)
    (set! stop-cond #t)
    (break-continuation #t)))

(define-debug-cmd
  next
  #t
  (lambda (loop break-continuation filename line)
    (set! stop-cond filename)
    (break-continuation #t)))

(define-debug-cmd
  continue
  #t
  (lambda (loop break-continuation filename line)
    (set! stop-cond #f)
    (break-continuation #t)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Util
;;;;;;;;;;;;;;;;;;;;;;;

(define (print-port x port)
  (for-each (lambda (s) (display (x->string s) port)) x)
  (newline port)
  (flush port))
(define (print-err . x) (print-port x (current-error-port)))

