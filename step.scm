(define c 3)

(define (add a)
  (print "before")
  (print (string-split "hoge huga" #\space))
  (let1 b 5
    (print (+ (mul a b) c)))
  (print "after"))

(define (mul a b)
  (* a b))

