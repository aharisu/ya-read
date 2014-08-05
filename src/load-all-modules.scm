(use ya.read)
(use file.util)
(use srfi-13)

(define ya-load-from-port
  (let1 *original-load-from-port* load-from-port
    (lambda (port . args)
      (apply *original-load-from-port* (cons (wrap-ya-port port) args)))))

(set! load-from-port ya-load-from-port)
(set! read ya-read)

(for-each
  (lambda (pattern)
    (library-for-each
      pattern
      (lambda (name path)
        (when (absolute-path? path)
          (unless (memq name '(srfi.30 srfi.25 srfi.40 srfi.46 srfi.18 srfi.39 srfi.99 gauche.cgen.stub))
            (load path)
            ))
        )))
  '(* *.* *.*.*))

