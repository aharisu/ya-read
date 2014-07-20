(define-module coverage.util
  (use srfi-13)
  (use file.util)
  (use srfi-11) ;;let*-values
  (export to-absolute-path
    output-coverage-file output-coverage-summary
    ))

(select-module coverage.util)

(define (get-coverage-directory)
  (rlet1 path (build-path (current-directory) ".coverage")
    (make-directory* path)))

(define (to-absolute-path filename)
  (expand-path (simplify-path (if (relative-path? filename)
                                (build-path (current-directory) filename)
                                filename))))

(define (split-directory directory)
  (let loop ([directory directory]
             [acc '()])
    (receive (parent-directory cur-directory ext) (decompose-path directory)
      (if (or (string=? parent-directory ".")
            (string=? parent-directory "/"))
        (cons cur-directory acc)
        (loop parent-directory (cons cur-directory acc))))))

(define (get-coverage-filename filename)
  (string-append 
    (let1 abspath (to-absolute-path filename)
      (if-let1 load-path (find
                           (cut string-prefix? <> abspath)
                           (map to-absolute-path *load-path*))
        (string-join (split-directory (substring abspath
                                                 (string-length load-path)
                                                 (string-length abspath)))
                     ".")
        filename))
    ".COVER.html"))

(define (output-coverage-file filename content-body)
  (let1 cover-filename (get-coverage-filename filename)
    (call-with-output-file
      (build-path (get-coverage-directory) cover-filename)
      (pa$ display
           #`"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n\
           <html>\n\
           <head>\n\
           <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\
           <title>,|cover-filename|</title>\n\
           </head><body style='background-color: white; color: black'>\n\
           <pre>\n\
           \n\
           *************************************************\n\
           \n\
           ,|content-body|\n\
           </pre>\n\
           </body>\n\
           </html>")
           :if-exists :supersede
           :if-does-not-exist :create)))

(define (output-coverage-summary filename/total/cover-list)
  (let* ([total/cover (fold
                        (lambda (summary total/cover-acc)
                          (cons
                            (+ (car total/cover-acc) (cadr summary))
                            (+ (cdr total/cover-acc) (caddr summary))))
                        '(0 . 0)
                        filename/total/cover-list)]
         [body (string-join
                 (map (lambda (summary)
                        (let ([filename (get-coverage-filename (car summary))]
                              [total (cadr summary)]
                              [cover (caddr summary)])
                          #`"<tr><td><a href=',|filename|'>,(string-drop-right filename (string-length \".COVER.html\"))</a></td><td>,(floor->exact (* (/ cover total) 100))%</td>"))
                      filename/total/cover-list)
                 "\n")])
    (call-with-output-file
      (build-path (get-coverage-directory) "index.html")
      (pa$ display
           #`"<!DOCTYPE HTML><html>\n\
           <head><meta charset=\"utf-8\"><title>Coverage Summary</title></head>\n\
           <body>\n\
           <h1>Coverage Summary</h1>\n\
           <h3>Total: ,(floor->exact (* (/ (cdr total/cover) (car total/cover)) 100))%</h3>\n\
           <table><tr><th>File</th><th>Coverage %</th></tr>\n\
           ,|body|\n\
           </table>\n\
           </body></html>")
           :if-exists :supersede
           :if-does-not-exist :create)))
    
