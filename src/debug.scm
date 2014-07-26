(use gauche.parseopt)
(use gauche.parameter)
(use gauche.threads)
(use gauche.vport)
(use srfi-13)
(use util.queue)

(use ya.port)
(use ya.read)
(use debug.traverse)
(use debug.message-queue)

(define (usage)
  (exit 1 "Usage:\
          \n  gosh debug.scm gauche-script-file"
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug function interpolate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ya-load-from-port
  (let1 *original-load-from-port* load-from-port
    (lambda (port . args)
      (apply *original-load-from-port* (cons (wrap-ya-port port) args)))))

(include "debug/traverse-struct.scm")

(define break. (global-id 'break (current-module)))
(define begin. (global-id 'begin))
(define if. (global-id 'if))
(define quote. (global-id 'quote))
(define let. (global-id 'let))
(define list. (global-id 'list))
(define cons. (global-id 'cons))
(define append. (global-id 'append))
(define dynamic-wind. (global-id 'dynamic-wind))

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

(define frame-sym (gensym "frame"))
(define-macro (in-null-module-empty-list-definition sym)
  `(define-in-module null ,(eval sym (current-module)) '()))
(in-null-module-empty-list-definition frame-sym)

(define (read-after toplevel-sexp)
  (define (make-frame-let lambda? name lvars args body)
    (let1 frame-obj (cons `(,cons. ,lambda? (,quote. ,name))
                          (let loop ([lvars lvars]
                                     [args (if (pair? args) args (cons args '()))]
                                     [acc '()])
                            (if (null? lvars)
                              (reverse acc)
                              (let1 args (if (pair? args) args (cons args '()))
                                (loop (cdr lvars)
                                      (cdr args)
                                      (cons `(,cons. (,quote. ,(car lvars))
                                                     ,(car args))
                                            acc))))))
      `(,let. ([,frame-sym (,append. (,list. ,@frame-obj) ,frame-sym)])
              ,@body)))

  (scan-expression
    toplevel-sexp
    ;;each expression hook
    (lambda (sexp src)
      (if (pair? sexp)
        (let* ([srcinfo (hash-table-get srcinfo-table src '(#f #f #f))]
               [cell (if (and (car srcinfo) (cadr srcinfo))
                       (get-break-point-cell (car srcinfo) (cadr srcinfo))
                       (cons #f (cons #f #f)))])
          `(,begin.
             (,break. (,quote. ,cell) ,frame-sym)
             ,sexp))
        sexp))
    ;;iform hook
    (lambda (sexp iform)
      (let1 tag (iform-tag iform)
        (cond
          [(= $LAMBDA tag)
           `(,(car sexp)
              ,(cadr sexp)
              ,(make-frame-let #t ($lambda-name iform)
                               (map
                                 (lambda (lvar) (lvar-name lvar))
                                 ($lambda-lvars iform))
                               (cadr sexp)
                               (cddr sexp)))]
          [(= $RECEIVE tag)
           `(,(car sexp)
              ,(cadr sexp)
              ,(caddr sexp)
              ,(make-frame-let #f 'receive
                               (map
                                 (lambda (lvar) (lvar-name lvar))
                                 ($receive-lvars iform))
                               (cadr sexp)
                               (cdddr sexp)))]
          [(and (= $LET tag)
             (not (equal? (car sexp) dynamic-wind.)))
           `(,(car sexp)
              ,(cadr sexp)
              ,(make-frame-let #f (identifier->symbol (car sexp))
                               (map
                                 (lambda (lvar) (lvar-name lvar))
                                 ($let-lvars iform))
                               (map car (cadr sexp))
                               (cddr sexp)))]
          [else sexp])))
    ))

(define (reload-used-module)
  (let1 modules (make-hash-table)
    (for-each
      (lambda (pattern)
        (library-for-each
          pattern
          (lambda (name path)
            (hash-table-put! modules name path))))
      '(* *.* *.*.* ))
    (for-each
      (lambda (m)
        (let1 name (module-name m)
          (unless (member (car (string-split (symbol->string name) #\.))
                          '("ya" "debug")
                          string=?)
            (if-let1 path (hash-table-get modules name #f)
              (load path)))))
      (all-modules))))

(define (setup)
  (set! load-from-port ya-load-from-port)
  (set! read ya-read)
  (add-ya-read-after-hook read-after)
  (add-each-ya-read-after-hook each-read-after)
  (reload-used-module)
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
              [(show)
               (print (cadr line/or/notify))]
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

(define (read-expression args)
  (let* ([line (string-append (string-join args " ") " ")]
         [index 0]
         [inport (make <virtual-input-port>
                       :getc (lambda ()
                               (if (< index (string-length line))
                                 (begin0
                                   (string-ref line index)
                                   (inc! index))
                                 (read-char))))])
    (read inport)))

(define-ui-cmd
  exec
  (pa$ <= 1)
  (lambda (msg-queue . args)
    (let1 e (read-expression args)
      (post msg-queue (list 'exec e)))))

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

(define (cmd-write/display post-tag msg-queue . args)
  (let1 e (read-expression args)
    (post msg-queue (list post-tag e))))

(define-ui-cmd
  print
  (pa$ <= 1)
  (pa$ cmd-write/display 'print))

(define-ui-cmd
  write
  (pa$ <= 1)
  (pa$ cmd-write/display 'write))

(define-ui-cmd
  backtrace
  (^n (or (zero? n) (= n 1)))
  (lambda (msg-queue :optional mode)
    (cond
      [(undefined? mode)
       (post msg-queue (list 'backtrace 'function))]
      [(string=? mode "full")
       (post msg-queue (list 'backtrace 'full))]
      [else
        (print-err "Error: Specify the function or full.")])))

;;;;;;;;;;;;;;;;;;;;;;;
;; Debug entry point
;;;;;;;;;;;;;;;;;;;;;;;

(define break-escape-point #f)

(define stop-cond (make-parameter #f))
(define stop-cond-temp #f)

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
                                 notify-queue
                                 (car break-point-info)
                                 (cadr break-point-info)
                                 (caddr break-point-info)
                                 (cadddr break-point-info)
                                 )
                           (cdr msg)))
            (begin
              (enqueue! notify-queue '(error "The program is not stop"))
              (loop)))
          ;;exec normal debug command
          (apply (debug-cmd-exec cmd) (append (list loop notify-queue) (cdr msg))))))))

(define (check-break-point-cell cell frame)
  (let1 break? (car cell)
    (cond
      [(eq? break? #t) #t]
      [else #f])))

(define (current-functionname frame)
  (assq-ref frame #t 'top-level))

(define (break cell frame)
  (when (if-let1 stop-cond (stop-cond)
          ;;position has not changed
          (if (and
                (= (cadr cell) (caddr stop-cond)) ;in the same line?
                (string=? (cddr cell) (cadr stop-cond))) ;and, in the same file?
            #f ; continue to run.
            (or
              (case (car stop-cond)
                [(step)
                 ;;always stop if the position has changed.
                 #t]
                [(next)
                 ;;stop when it if both have the same function.
                 (eq? (current-functionname frame) (cadddr stop-cond))]
                [(continue) 
                 ;;always continue if the position has changed.
                 #f])
              ;;if not the case in the stop condition, check current break point cell.
              (check-break-point-cell cell frame)))
          ;;if no stop condition, check current break point cell.
          (check-break-point-cell cell frame))
    (call/cc
      (lambda (return-point)
        (break-escape-point (list
                              return-point
                              (cddr cell) ;filename
                              (cadr cell) ;line
                              frame
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
  (lambda (loop notify-queue script-file)
    (load script-file)
    (loop)))

(define-debug-cmd
  exec
  #f
  (lambda (loop notify-queue e)
      (dynamic-wind
        (lambda ()
          (stop-cond stop-cond-temp)
          (set! stop-cond-temp #f))
        (lambda ()
          (eval e (current-module)))
        (lambda ()
          (stop-cond #f)))
      (loop)))

(define-debug-cmd
  step
  #t
  (lambda (loop notify-queue break-continuation filename line frame)
    (set! stop-cond-temp (list 'step filename line))
    (break-continuation #t)))

(define-debug-cmd
  next
  #t
  (lambda (loop notify-queue break-continuation filename line frame)
    (set! stop-cond-temp (list 'next filename line (current-functionname frame)))
    (break-continuation #t)))

(define-debug-cmd
  continue
  #t
  (lambda (loop notify-queue break-continuation filename line frame)
    (set! stop-cond-temp (list 'continue filename line))
    (break-continuation #t)))

(define (frame-map proc frame)
  (let loop ([frame frame]
             [result-acc '()])
    (if (null? frame) 
      (reverse result-acc)
      (receive (frame acc) (let frame-constract ([frame (cdr frame)]
                                                 [acc (list (car frame))])
                             (if (or (null? frame) (boolean? (caar frame)))
                               (values frame (reverse acc))
                               (frame-constract (cdr frame)
                                                (cons (car frame) acc))))
        (loop frame
              (cons (proc acc) result-acc))))))

(define-debug-cmd
  backtrace
  #t
  (lambda (loop notify-queue break-continuation filename line frame mode)
    (define (frame->string frame)
      (with-output-to-string
        (lambda ()
          (display (or (cdar frame) "???"))
          (display " (")
          (display (string-join
                     (map 
                       (lambda (binding)
                         (string-append
                           (symbol->string (car binding))
                           "="
                           (with-output-to-string
                             (pa$ write (cdr binding)))))
                       (cdr frame))
                     " "))
          (display ")"))))
    (let1 frame-str-list (filter
                           identity
                           (let1 all? (eq? mode 'full)
                             (frame-map
                               (lambda (f)
                                 (if (or all? (caar f))
                                   (frame->string f)
                                   #f))
                               frame)))
      (enqueue! notify-queue (list 'show (string-join frame-str-list "\n"))))
    (loop)))

(define resolve-notfound-sym (gensym))
(define (exec-watch-expression notify-queue frame show-func e)
  (let1 e (let resolve-local-var ([e e])
            (cond
              [(pair? e)
               (cons (resolve-local-var (car e)) (resolve-local-var (cdr e)))]
              [(symbol? e)
               (let1 val (assq-ref frame e resolve-notfound-sym) 
                 (if (eq? val resolve-notfound-sym)
                   e
                   (list quote. val)))]
              [else e]))
    (receive (type msg)
      (guard (err 
               [(<message-condition> err)
                (values 'error (slot-ref err 'message))]
               [else
                 (values 'error (with-output-to-string (pa$ display err)))])
        (let1 result (eval e (current-module))
          (values 'show (with-output-to-string (pa$ show-func result)))))
      (enqueue! notify-queue (list type msg)))))

(define-debug-cmd
  print
  #t
  (lambda (loop notify-queue break-continuation filename line frame e)
    (exec-watch-expression notify-queue frame display e)
    (loop)))

(define-debug-cmd
  write
  #t
  (lambda (loop notify-queue break-continuation filename line frame e)
    (exec-watch-expression notify-queue frame write e)
    (loop)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Util
;;;;;;;;;;;;;;;;;;;;;;;

(define (print-port x port)
  (for-each (lambda (s) (display (x->string s) port)) x)
  (newline port)
  (flush port))
(define (print-err . x) (print-port x (current-error-port)))

