(define-module ya.port
  (use gauche.vport)
  (export wrap-ya-port ya-wraped-port? ungetc source-info))

(select-module ya.port)

(define-class <ya-virtual-input-port> (<virtual-input-port>)
  (
   (line :init-value 1)
   (col :init-value 0)
   (ungotten :init-value '())
   (prev-ch :init-value 0)
   (col-list :init-value '())
   (port :init-keyword :port)
   ))

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

(define (source-info port)
  (list
    (port-name (slot-ref port 'port))
    (slot-ref port 'line)
    (slot-ref port 'col)))

