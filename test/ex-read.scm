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

