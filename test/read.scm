(use gauche.test)
(test-start "ya-read")

(use ya.read)

(test-section "ya-port")

(let1 port (wrap-ya-port (open-input-string "hoge"))
  (test* "getc" #\h (read-char port))
  (test* "getc" #\o (read-char port))
  (test* "getc" #\g (read-char port))
  (test* "getc" #\e (read-char port))
  (test* "getc" (eof-object) (read-char port))
  (test* "getc" (eof-object) (read-char port)))

(let1 port (wrap-ya-port (open-input-string "hoge"))
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

(let1 port (wrap-ya-port (open-input-string "hoge\nfoo\r\nbar\rbaz"))
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
  (test* "source info" '(4 . 1) (get-line-col port)))
  
(test-section "ya-read")

(let1 port (wrap-ya-port (open-input-string "hoge   huga\nbar\r\nfoo :keyword :"))
  (test* "symbol read" 'hoge (ya-read port))
  (test* "symbol read" 'huga (ya-read port))
  (test* "symbol read" 'bar (ya-read port))
  (test* "symbol read" 'foo (ya-read port))
  (test* "keyword read" :keyword (ya-read port))
  (test* "keyword read" : (ya-read port))
  (test* "symbol read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "1 102 +2 -3 3.14 123/7 81/169 2.0e20 2-i #xFF"))
  (test* "number read" 1 (ya-read port))
  (test* "number read" 102  (ya-read port))
  (test* "number read" 2 (ya-read port))
  (test* "number read" -3 (ya-read port))
  (test* "number read" 3.14 (ya-read port))
  (test* "number read" 123/7 (ya-read port))
  (test* "number read" 81/169 (ya-read port))
  (test* "number read" 2.0e20 (ya-read port))
  (test* "number read" 2-i (ya-read port))
  (test* "number read" #xFF (ya-read port))
  (test* "number read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "'abc d'ef"))
  (test* "quote read" ''abc (ya-read port))
  (test* "quote read" 'd (ya-read port))
  (test* "quote read" ''ef (ya-read port))
  (test* "quote read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "`(a b c) `a(b c) `(a b ,c) `(a b ,c ,@d) `[a b ,@c]"))
  (test* "quasiquote read" '`(a b c) (ya-read port))
  (test* "quasiquote read" '`a (ya-read port))
  (test* "quasiquote read" '(b c) (ya-read port))
  (test* "quasiquote read" '`(a b ,c) (ya-read port))
  (test* "quasiquote read" '`(a b ,c ,@d) (ya-read port))
  (test* "quasiquote read" '`[a b ,@c] (ya-read port))
  (test* "quasiquote read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "(a b c) a(b c)d (a b . c) (a b . c d) [a b c] ]"))
  (test* "list read" '(a b c) (ya-read port))
  (test* "list read" 'a (ya-read port))
  (test* "list read" '(b c) (ya-read port))
  (test* "list read" 'd (ya-read port))
  (test* "list read" '(a b . c) (ya-read port))
  (test* "list read" (test-error) (ya-read port)) ;;bad dot syntax of (a b . c d)
  (test* "list read" (test-error) (ya-read port)) ;;read close paren of (a b . c d)
  (test* "list read" '(a b c) (ya-read port))
  (test* "list read" (test-error) (ya-read port)) ;;read close bracket
  (test* "list read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "#(a b c) #(b c)d #(a b . c) #u8(1 2) #u16(1 2) #f32(1 2 3.14)"))
  (test* "vector read" #(a b c) (ya-read port))
  (test* "vector read" #(b c) (ya-read port))
  (test* "vector read" 'd (ya-read port))
  (test* "vector read" (test-error) (ya-read port))
  (test* "u8vector read" #u8(1 2) (ya-read port))
  (test* "u16vector read" #u16(1 2) (ya-read port))
  (test* "f32vector read" #f32(1 2 3.14) (ya-read port))
  (test* "vector read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "\"abc\" \"a\n\r\" \"\x40\u3042\""))
  (test* "string read" "abc" (ya-read port))
  (test* "string read" "a\n\r" (ya-read port))
  (test* "string read" "@あ" (ya-read port))
  (test* "string read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "#*\"abc\" #*\"a\n\r\" #*\"\x40\u3042\""))
  (test* "string read" #*"abc" (ya-read port))
  (test* "string read" #*"a\n\r" (ya-read port))
  (test* "string read" #*"@あ" (ya-read port))
  (test* "string read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "#`\"abc\" #`\"ab ,(+ 1 2)\" #`\a"))
  (test* "string interpolate read" '#`"abc" (ya-read port))
  (test* "string interpolate read" '#`"ab ,(+ 1 2)" (ya-read port))
  (test* "string interpolate read" (test-error) (ya-read port))
  (test* "string interpolate read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "#\\a #\\; #\\newline #\\tab #\\x0A #\\u3042"))
  (test* "char read" #\a (ya-read port))
  (test* "char read" #\; (ya-read port))
  (test* "char read" #\newline (ya-read port))
  (test* "char read" #\tab (ya-read port))
  (test* "char read" #\x0A (ya-read port))
  (test* "char read" #\u3042 (ya-read port))
  (test* "char read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "#f #t aiu#t#f"))
  (test* "bool read" #f (ya-read port))
  (test* "bool read" #t (ya-read port))
  (test* "bool read" '|aiu#t#f| (ya-read port))
  (test* "bool read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "a;bcd\n a ;bc\r a;\r\n a;\na"))
  (test* "line comment read" 'a (ya-read port))
  (test* "line comment read" 'a (ya-read port))
  (test* "line comment read" 'a (ya-read port))
  (test* "line comment read" 'a (ya-read port))
  (test* "line comment read" 'a (ya-read port))
  (test* "line comment read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "a #;bc\n #;(a ;)\r b)c"))
  (test* "s-exp comment read" 'a (ya-read port))
  (test* "s-exp comment read" 'c (ya-read port))
  (test* "s-exp comment read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "a #|iue|#o #|ai#|ue|#o|#k"))
  (test* "block-exp comment read" 'a (ya-read port))
  (test* "block-exp comment read" 'o (ya-read port))
  (test* "block-exp comment read" 'k (ya-read port))
  (test* "block-exp comment read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "#[abc] #[[:alnum:]] #[] #[\\]]"))
  (test* "char-set read" #[abc] (ya-read port))
  (test* "char-set read" #[[:alnum:]] (ya-read port))
  (test* "char-set read" #[] (ya-read port))
  (test* "char-set read" #[\]] (ya-read port))
  (test* "char-est read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "#/a+b/ #/a.b/i #/a|b|c/ #/\\\//"))
  (test* "regexp read" #/a+b/ (ya-read port))
  (test* "regexp read" #/a.b/i (ya-read port))
  (test* "regexp read" #/a|b|c/ (ya-read port))
  (test* "regexp read" #/\// (ya-read port))
  (test* "regexp read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "|aiue| |123| |:keyword| |\\#\\||"))
  (test* "multi-escape symbol read" '|aiue| (ya-read port))
  (test* "multi-escape symbol read" '|123| (ya-read port))
  (test* "multi-escape symbol read" '|:keyword| (ya-read port))
  (test* "multi-escape symbol read" '|\#\|| (ya-read port))
  (test* "symbol read" (eof-object) (ya-read port)))

(test* "hash-bang read" :shebang
  (ya-read (wrap-ya-port (open-input-string "#!/shebang\n:shebang"))))
(test* "hash-bang read" :space-shebang
  (ya-read (wrap-ya-port (open-input-string "#! shebang\n:space-shebang"))))
(test* "hash-bang read" (undefined)
  (ya-read (wrap-ya-port (open-input-string "#! shebang\n#!hash-bang"))))

(let1 port (wrap-ya-port (open-input-string "a#t #t #ta"))
  (test* "s-exp non-term macro" '|a#t| (ya-read port))
  (test* "s-exp non-term macro" #t (ya-read port))
  (test* "s-exp non-term macro" '|#ta| (ya-read port))
  (test* "s-exp non-term macro" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "#(1) a#(1)"))
  (test* "s-exp righte-term macro" #(1) (ya-read port))
  (test* "s-exp righte-term macro" '|a#| (ya-read port))
  (test* "s-exp righte-term macro" '(1) (ya-read port))
  (test* "s-exp righte-term macro" (eof-object) (ya-read port)))

(define-reader-ctor 'pi (lambda () (* (atan 1) 4)))
(define-reader-ctor 'hash
  (lambda (type . pairs)
    (rlet1 tab (make-hash-table type)
      (for-each
        (lambda (pair) (hash-table-put! tab (car pair) (cdr pair)))
        pairs))))

(let1 port (wrap-ya-port (open-input-string "#,(pi) '(#,(pi)) #,(hash eq? (foo . bar) (hoge . piyo))"))
  (test* "reader constractor" 3.141592653589793 (ya-read port))
  (test* "reader constractor" ''(3.141592653589793) (ya-read port))
  (let1 table (ya-read port)
    (test* "reader constractor" 'bar (hash-table-get table 'foo))
    (test* "reader constractor" 'piyo (hash-table-get table 'hoge)))
  (test* "s-exp righte-term macro" (eof-object) (ya-read port)))

(define *counter* 0)
(define-reader-ctor 'countup (lambda () (inc! *counter*) #f))
(let1 port (wrap-ya-port (open-input-string "#,(countup) #;#,(countup) #,(countup)"))
  (ya-read port)
  (ya-read port)
  (ya-read port)
  (test* "reader constractor" 2 *counter*))

(add-reader-macro "a" 'term (lambda (port) 1))
(add-reader-macro "ab" 'right-term (lambda (port) 2))
(add-reader-macro "bc" 'non-term (lambda (port) 3))

(let1 port (wrap-ya-port (open-input-string "zabc"))
  (test* "term macro" 'z (ya-read port))
  (test* "term macro" 1 (ya-read port))
  (test* "term macro" 3 (ya-read port))
  (test* "term macro" (eof-object) (ya-read port)))

(test-end)

