(define-module ex-trie
  (extend util.trie)
  (export trie-common-prefix-continuation))

(select-module ex-trie)

(define (%trie-get-node-from-elt trie elt)
  (and-let* ([table (%node-table (slot-ref trie 'root))])
    ((slot-ref trie 'tab-get)  table elt)))

(define (%trie-prefix-elt-collect trie prefix-elt getter)
  (or (and-let* ([node (%trie-get-node-from-elt trie prefix-elt )])
        (let ([sub-trie (make <trie>
                              :tab-make (slot-ref trie 'tab-make)
                              :tab-get (slot-ref trie 'tab-get)
                              :tab-put! (slot-ref trie 'tab-put!)
                              :tab-fold (slot-ref trie 'tab-fold))]
              [terminal (%node-terminals node)])
          (slot-set! sub-trie 'root node)
          (if (null? terminal)
            sub-trie
            (cons sub-trie (cdar terminal)))))
    #f))

(define (trie-common-prefix-continuation trie prefix-elt)
  (%trie-prefix-elt-collect trie prefix-elt (^ [k v] (cons k v))))

(define t (make-trie list
                     (cut assoc-ref <> <> #f char-ci=?)
                     (lambda (t k v)
                       (if v
                         (assoc-set! t k v char-ci=?)
                         (alist-delete! k t char-ci=?)))
                     (lambda (t f s) 
                       (fold (lambda (node acc) (f (car node) (cdr node) acc)) s t))))

