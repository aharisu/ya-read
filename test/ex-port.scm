(use gauche.test)
(test-start "extend-trie")

(use ex-port)

(let1 port (wrap-ex-port (open-input-string "hoge"))
  (test* "getc" #\h (read-char port))
  (test* "getc" #\o (read-char port))
  (test* "getc" #\g (read-char port))
  (test* "getc" #\e (read-char port))
  (test* "getc" (eof-object) (read-char port))
  (test* "getc" (eof-object) (read-char port)))

(let1 port (wrap-ex-port (open-input-string "hoge"))
  (read-char port)
  (read-char port)
  (ungetc #\o port)
  (ungetc #\h port)
  (test* "ungetc" #\h (read-char port))
  (test* "ungetc" #\o (read-char port))
  (test* "ungetc" #\g (read-char port))
  (ungetc #\g port)
  (test* "ungetc" #\g (read-char port))
  (test* "ungetc" #\e (read-char port))
  (test* "ungetc" (eof-object) (read-char port))
  (ungetc #\e port)
  (test* "ungetc" #\e (read-char port))
  (test* "ungetc" (eof-object) (read-char port)))
  

(define (get-line-col port)
  (let1 info (source-info port)
    (cons (cadr info) (caddr info))))

(define (read-num-char port num)
  (unless (zero? num)
    (read-char port)
    (read-num-char port (- num 1))))

(let1 port (wrap-ex-port (open-input-string "hoge\nfoo\r\nbar\rbaz"))
  (read-num-char port 2) ;;read ho
  (test* "source info" '(1 . 2) (get-line-col port))
  (ungetc #\o port) ;unget o
  (test* "source info" '(1 . 1) (get-line-col port))
  (read-num-char port 3) ;;read oge
  (test* "source info" '(1 . 4) (get-line-col port))
  (read-num-char port 1) ;; read \n
  (test* "source info" '(2 . 0) (get-line-col port))
  (ungetc #\newline port) ;; unget \n
  (test* "source info" '(1 . 4) (get-line-col port))
  (read-num-char port 4) ;; read \nfoo
  (test* "source info" '(2 . 3) (get-line-col port))
  (read-num-char port 3) ;; read \r\nb
  (test* "source info" '(3 . 1) (get-line-col port))
  (ungetc #\b port) ;; ungetc b
  (ungetc #\newline port) ;; ungetc \n
  (test* "source info" '(2 . 3) (get-line-col port))
  (ungetc #\return port) ;; ungetc \r
  (test* "source info" '(2 . 3) (get-line-col port))
  (read-num-char port 7) ;; read \r\nbar\nb
  (test* "source info" '(4 . 1) (get-line-col port))
  (read-num-char port 5) ;; read az
  (test* "source info" '(4 . 3) (get-line-col port)))
  
(test-end)


