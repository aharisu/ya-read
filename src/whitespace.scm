(use ya.read)
(use file.util)
(use srfi-13)

(define (usage)
  (exit 1 "Usage:\
          \n  gosh whitespace.scm filepath"
  ))

(define (main args)
  (if (null? (cdr args))
    (usage)
    (load-whitespace-lang (cadr args))))

(define (load-whitespace-lang filepath)
  (setup-whitespace-reader)
  (for-each
    (cut eval <> (current-module))
    (call-with-input-file
      (if (relative-path? filepath)
        (build-path (sys-getcwd) filepath)
        filepath)
      (lambda (port)
        (let1 ya-port (wrap-ya-port port)
          (let loop ([exp '()])
            (let1 e (ya-read ya-port)
              (if (eof-object? e)
                (reverse exp)
                (loop (cons e exp))))))))))

(define (setup-whitespace-reader)
  (add-char-kind #[\x0a;\x09;\x20;] 'constituent #t)
  (add-char-kind #[^\x0a;\x09;\x20;] 'skip #t)
  ;;stack operation
  (add-reader-macro "  " 'right-term read-push)
  (add-reader-macro " \n " 'right-term read-clone-top)
  (add-reader-macro " \t " 'right-term read-copy-nth-item)
  (add-reader-macro " \n\t" 'right-term read-swap-first-second)
  (add-reader-macro " \n\n" 'right-term read-discard-top)
  (add-reader-macro " \t\n" 'right-term read-slide-items)
  ;;numerical operation
  (add-reader-macro "\t   " 'right-term (pa$ read-arithmetic +))
  (add-reader-macro "\t  \t" 'right-term (pa$ read-arithmetic -))
  (add-reader-macro "\t  \n" 'right-term (pa$ read-arithmetic *))
  (add-reader-macro "\t \t " 'right-term (pa$ read-arithmetic /))
  (add-reader-macro "\t \t\t" 'right-term (pa$ read-arithmetic modulo))
  ;;heap operation
  (add-reader-macro "\t\t " 'right-term read-address-store)
  (add-reader-macro "\t\t\t" 'right-term read-address-retrieve)
  ;;flow operation
  (add-reader-macro "\n  " 'right-term read-define-label)
  (add-reader-macro "\n \t" 'right-term read-call-routine)
  (add-reader-macro "\n \n" 'right-term read-goto)
  (add-reader-macro "\n\t " 'right-term (pa$ read-goto-pred zero?))
  (add-reader-macro "\n\t\t" 'right-term (pa$ read-goto-pred negative?))
  (add-reader-macro "\n\t\n" 'right-term read-return)
  (add-reader-macro "\n\n\n" 'right-term read-halt)
  ;;io operation
  (add-reader-macro "\t\n  " 'right-term read-write-char)
  (add-reader-macro "\t\n \t" 'right-term read-write-number)
  (add-reader-macro "\t\n\t " 'right-term read-store-char)
  (add-reader-macro "\t\n\t\t" 'right-term read-store-number)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translate whitespace -> Gauche
;;;;;;;;;;;;;;;;;;;;;;;;;

(define stack '())
(define heap (make-hash-table 'equal?))
(define label-table (make-hash-table))
(define call-stack '())

(define (read-to-newline port)
  (let1 acc (open-output-string)
    (let loop ([ch (read-char port)])
      (cond
        [(char=? ch #\space)
         (write-char #\space acc)
         (loop (read-char port))]
        [(char=? ch #\tab)
         (write-char #\tab acc)
         (loop (read-char port))]
        [(char=? ch #\newline)
         (get-output-string acc)]
        [else (loop (read-char port))]))))

(define (read-number port)
  (let1 str (read-to-newline port)
    (*
      (if (char=? (string-ref str 0) #\space) 1 -1)
      (string->number 
        (string-map
          (lambda (ch) (if (char=? #\space ch) #\0 #\1))
          (substring str 1 (string-length str)))
        2))))

(define (read-push port)
  `(set! stack (cons ,(read-number port) stack)))

(define (read-clone-top port)
  '(set! stack (cons (car stack) stack)))

(define (read-copy-nth-item port)
  `(set! stack (cons (list-ref stack ,(read-number port)) stack)))

(define (read-swap-first-second port)
  '(set! stack (cons (cadr stack) (cons (car stack) (cddr stack)))))

(define (read-discard-top port)
  '(set! stack (cdr stack)))

(define (read-slide-items port)
  '(error "read-slide-items"))

(define (read-arithmetic op port)
  `(set! stack (cons (,op (cadr stack) (car stack)) (cddr stack))))

(define (read-address-store port)
  '(let ([value (car stack)]
         [address (cadr stack)])
     (set! stack (cddr stack))
     (hash-table-put! heap address value)))

(define (read-address-retrieve port)
  '(set! stack (cons (hash-table-get heap (car stack))
                     (cdr stack))))

(define (read-label port)
  (string->symbol
    (string-map
      (lambda (ch) (if (eq? #\space ch) #\s #\t))
      (read-to-newline port))))

(define (read-define-label port)
  (let ([label (read-label port)]
        [body (let loop ([body '()])
                (let1 e (ya-read-rec port)
                  (if (eof-object? e)
                    (reverse body)
                    (loop (cons e body)))))])
    (hash-table-put! label-table label (eval `(lambda () ,@body) (current-module)))
    `(begin ,@body)))

(define (read-call-routine port)
  (let1 label (read-label port)
    `(call/cc (lambda (cont)
                (push! call-stack cont)
                ((hash-table-get label-table (quote ,label)))))))

(define (read-goto port)
  (let1 label (read-label port)
    `((hash-table-get label-table (quote ,label)))))

(define (read-goto-pred pred port)
  (let1 label (read-label port)
    `(if (,pred (pop! stack))
       ((hash-table-get label-table (quote ,label))))))

(define (read-return port)
  '((pop! call-stack)))

(define (read-halt port)
  '(exit))

(define (read-write-char port)
  '(begin
     (display (integer->char (car stack)))
     (flush)
     (set! stack (cdr stack))))

(define (read-write-number port)
  '(begin
     (display (car stack))
     (flush)
     (set! stack (cdr stack))))

(define (read-store-char port)
  `(hash-table-put! heap (car stack) (char->integer (read-char))))

(define (read-store-number port)
  `(hash-table-put! heap (car stack) (string->number (x->string (ya-read)))))

