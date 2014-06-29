(use gauche.test)
(test-start "extend-port")

(use ex-port)
(use ex-read)

(let1 port (wrap-ex-port (open-input-string "hoge   huga\nbar\r\nfoo"))
  (test* "symbol read" 'hoge (ex-read port))
  (test* "symbol read" 'huga (ex-read port))
  (test* "symbol read" 'bar (ex-read port))
  (test* "symbol read" 'foo (ex-read port))
  (test* "symbol read" (eof-object) (ex-read port)))

(let1 port (wrap-ex-port (open-input-string "1 102 +2 -3 3.14 123/7 81/169 2.0e20 2-i #xFF"))
  (test* "number read" 1 (ex-read port))
  (test* "number read" 102  (ex-read port))
  (test* "number read" 2 (ex-read port))
  (test* "number read" -3 (ex-read port))
  (test* "number read" 3.14 (ex-read port))
  (test* "number read" 123/7 (ex-read port))
  (test* "number read" 81/169 (ex-read port))
  (test* "number read" 2.0e20 (ex-read port))
  (test* "number read" 2-i (ex-read port))
  (test* "number read" #xFF (ex-read port))
  (test* "number read" (eof-object) (ex-read port)))

(let1 port (wrap-ex-port (open-input-string "'abc d'ef"))
  (test* "quote read" ''abc (ex-read port))
  (test* "quote read" 'd (ex-read port))
  (test* "quote read" ''ef (ex-read port))
  (test* "quote read" (eof-object) (ex-read port)))

(let1 port (wrap-ex-port (open-input-string "(a b c) a(b c)d (a b . c) (a b . c d) [a b c] ]"))
  (test* "list read" '(a b c) (ex-read port))
  (test* "list read" 'a (ex-read port))
  (test* "list read" '(b c) (ex-read port))
  (test* "list read" 'd (ex-read port))
  (test* "list read" '(a b . c) (ex-read port))
  (test* "list read" (test-error) (ex-read port)) ;;bad dot syntax of (a b . c d)
  (test* "list read" (test-error) (ex-read port)) ;;read close paren of (a b . c d)
  (test* "list read" '(a b c) (ex-read port))
  (test* "list read" (test-error) (ex-read port)) ;;read close bracket
  (test* "list read" (eof-object) (ex-read port)))

(let1 port (wrap-ex-port (open-input-string "\"abc\" \"a\n\r\" \"\x40\u3042\""))
  (test* "string read" "abc" (ex-read port))
  (test* "string read" "a\n\r" (ex-read port))
  (test* "string read" "@あ" (ex-read port))
  (test* "string read" (eof-object) (ex-read port)))

(let1 port (wrap-ex-port (open-input-string "#*\"abc\" #*\"a\n\r\" #*\"\x40\u3042\""))
  (test* "string read" #*"abc" (ex-read port))
  (test* "string read" #*"a\n\r" (ex-read port))
  (test* "string read" #*"@あ" (ex-read port))
  (test* "string read" (eof-object) (ex-read port)))

(let1 port (wrap-ex-port (open-input-string "#\\a #\\; #\\newline #\\tab #\\x0A #\\u3042"))
  (test* "char read" #\a (ex-read port))
  (test* "char read" #\; (ex-read port))
  (test* "char read" #\newline (ex-read port))
  (test* "char read" #\tab (ex-read port))
  (test* "char read" #\x0A (ex-read port))
  (test* "char read" #\u3042 (ex-read port))
  (test* "char read" (eof-object) (ex-read port)))

(let1 port (wrap-ex-port (open-input-string "#f #t aiu#t#f"))
  (test* "bool read" #f (ex-read port))
  (test* "bool read" #t (ex-read port))
  (test* "bool read" '|aiu#t#f| (ex-read port))
  (test* "bool read" (eof-object) (ex-read port)))

(let1 port (wrap-ex-port (open-input-string "a;bcd\n a ;bc\r a;\r\n a;"))
  (test* "line comment read" 'a (ex-read port))
  (test* "line comment read" 'a (ex-read port))
  (test* "line comment read" 'a (ex-read port))
  (test* "line comment read" 'a (ex-read port))
  (test* "line comment read" (eof-object) (ex-read port)))

(let1 port (wrap-ex-port (open-input-string "a#;bc\n #;(a ;)\r b)c"))
  (test* "s-exp comment read" 'a (ex-read port))
  (test* "s-exp comment read" 'c (ex-read port))
  (test* "s-exp comment read" (eof-object) (ex-read port)))

(let1 port (wrap-ex-port (open-input-string "a#|iue|#o #|ai#|ue|#o|#k"))
  (test* "block-exp comment read" 'a (ex-read port))
  (test* "block-exp comment read" 'o (ex-read port))
  (test* "block-exp comment read" 'k (ex-read port))
  (test* "block-exp comment read" (eof-object) (ex-read port)))

