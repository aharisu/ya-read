(use gauche.test)
(test-start "extend-port")

(use ya.port)
(use ya.read)

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

(let1 port (wrap-ya-port (open-input-string "#(a b c) a#(b c)d #(a b . c) #u8(1 2) #u16(1 2) #f32(1 2 3.14)"))
  (test* "vector read" #(a b c) (ya-read port))
  (test* "vector read" 'a (ya-read port))
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

(let1 port (wrap-ya-port (open-input-string "a;bcd\n a ;bc\r a;\r\n a;"))
  (test* "line comment read" 'a (ya-read port))
  (test* "line comment read" 'a (ya-read port))
  (test* "line comment read" 'a (ya-read port))
  (test* "line comment read" 'a (ya-read port))
  (test* "line comment read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "a#;bc\n #;(a ;)\r b)c"))
  (test* "s-exp comment read" 'a (ya-read port))
  (test* "s-exp comment read" 'c (ya-read port))
  (test* "s-exp comment read" (eof-object) (ya-read port)))

(let1 port (wrap-ya-port (open-input-string "a#|iue|#o #|ai#|ue|#o|#k"))
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

