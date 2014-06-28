(use gauche.test)
(test-start "extend-trie")

(use ex-trie)

(use gauche.sequence)
(use srfi-1)

(let* ([keys '("hoge" "hoga" "hi" "hikari")]
       [trie-test
         (lambda (t)
           (for-each-with-index 
             (lambda (index key) (trie-put! t key index))
             keys)
           (test* "prefix continuation" #t
             (is-a? (trie-common-prefix-continuation t #\h) <trie>))
           (test* "prefix continuation" #t
             (is-a? 
               (trie-common-prefix-continuation
                 (trie-common-prefix-continuation t #\h)
                 #\o)
               <trie>))
           (test* "prefix continuation" #t
             (is-a? 
               (trie-common-prefix-continuation
                 (trie-common-prefix-continuation
                   (trie-common-prefix-continuation t #\h)
                   #\o)
                 #\g)
               <trie>))
           (test* "prefix continuation" (cons #t 0)
             (receive (cont v)
               (car+cdr
                 (trie-common-prefix-continuation
                   (trie-common-prefix-continuation
                     (trie-common-prefix-continuation
                       (trie-common-prefix-continuation t #\h)
                       #\o)
                     #\g)
                   #\e))
               (cons (is-a? cont <trie>) v)))
           (test* "prefix continuation" #f
             (trie-common-prefix-continuation
               (car
                 (trie-common-prefix-continuation
                   (trie-common-prefix-continuation
                     (trie-common-prefix-continuation
                       (trie-common-prefix-continuation t #\h)
                       #\o)
                     #\g)
                   #\e))
               #\a))
           (test* "prefix continuation" #f
             (trie-common-prefix-continuation t #\a))
           )])
  (trie-test (make-trie))
  (trie-test (make-trie
               list
               (cut assoc-ref <> <> #f char-ci=?)
               (lambda (t k v)
                 (if v
                   (assoc-set! t k v char-ci=?)
                   (alist-delete! k t char-ci=?)))
               (lambda (t f s) 
                 (fold (lambda (node acc) (f (car node) (cdr node) acc)) s t))
               ))
  )


(test-end)
