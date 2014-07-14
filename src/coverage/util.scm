(define-module coverage.util
  (use srfi-13)
  (use util.match)
  (use file.util)
  (use gauche.vm.insn)
  (use srfi-11) ;;let*-values
  (export to-absolute-path
    output-coverage-file output-coverage-summary
    scan-expression))

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
    
(define-macro (define-enum name . syms)
  (do ((i 0 (+ i 1))
       (rest syms (cdr rest))
       (exprs '() (cons `(define-constant ,(car rest) ,i) exprs)))
      ((null? rest) `(begin ,@exprs))))

(define-enum .intermediate-tags.
  $DEFINE
  $LREF
  $LSET
  $GREF
  $GSET
  $CONST
  $IF
  $LET
  $RECEIVE
  $LAMBDA
  $LABEL
  $PROMISE
  $SEQ
  $CALL
  $ASM
  $CONS
  $APPEND
  $VECTOR
  $LIST->VECTOR
  $LIST
  $LIST*
  $MEMV
  $EQ?
  $EQV?
  $IT
  )

(define-macro (define-simple-struct name tag constructor :optional (slot-defs '()))
  (define (take l n) ; we can't use srfi-1 take, so here it is.
    (if (zero? n) '() (cons (car l) (take (cdr l) (- n 1)))))
  (define (make-constructor)
    (let ([args (gensym)]
          [num-slots  (length slot-defs)]
          [slot-names (map (^[s] (if (symbol? s) s (car s))) slot-defs)]
          [init-vals  (map (^[s] (if (symbol? s) #f (cadr s))) slot-defs)])
      `(define-macro (,constructor . ,args)
         (match ,args
           ,@(let loop ((n 0)
                        (r '()))
               (if (> n num-slots)
                 r
                 (let1 carg (take slot-names n)
                   (loop (+ n 1)
                         (cons
                          `(,carg
                            (list 'vector
                                  ,@(if tag `(',tag) '())
                                  ,@carg
                                  ,@(map (cut list 'quote <>)
                                         (list-tail init-vals n))))
                          r)))
                 ))))
      ))
  `(begin
     ,@(if constructor
         `(,(make-constructor))
         '())
     ,@(let loop ((s slot-defs) (i (if tag 1 0)) (r '()))
         (if (null? s)
           (reverse r)
           (let* ([slot-name (if (pair? (car s)) (caar s) (car s))]
                  [acc (string->symbol #`",|name|-,|slot-name|")]
                  [mod (string->symbol #`",|name|-,|slot-name|-set!")])
             (loop (cdr s)
                   (+ i 1)
                   (list*
                    `(define-macro (,acc obj)
                       `(vector-ref ,obj ,,i))
                    `(define-macro (,mod obj val)
                       `(vector-set! ,obj ,,i ,val))
                    r))))))
  )

;;============================================================
;; Data structures
;;

(define-macro (iform-tag iform)
  `(vector-ref ,iform 0))

;; Local variables (lvar)
(define-simple-struct lvar 'lvar #f
  (name
   (initval (undefined))
   (ref-count 0)
   (set-count 0)))

;; Compile-time environment (cenv)
(define-simple-struct cenv #f make-cenv
  (module frames exp-name current-proc (source-path (current-load-path))))

;; $define <src> <flags> <id> <expr>
(define-simple-struct $define $DEFINE #f
  (src       ; original source for debugging
   flags     ; a list of flags.  Currently, only the following flag
             ;  is supported:
             ;      const   : the binding is constant.
   id        ; global identifier
   expr      ; expression IForm
   ))

;; $lref <lvar>
(define-simple-struct $lref $LREF #f
  (lvar      ; lvar struct.
   ))

;; $lset <lvar> <expr>
(define-simple-struct $lset $LSET #f
  (lvar      ; lvar struct
   expr      ; IForm
   ))

;; $gref <id>
(define-simple-struct $gref $GREF #f
  (id        ; identifier
   ))

;; $gset <id> <iform>
(define-simple-struct $gset $GSET #f
  (id        ; identifier
   expr      ; IForm
   ))

;; $const <value>
(define-simple-struct $const $CONST #f
  (value     ; Scheme value
   ))

;; $if <src> <test> <then> <else>
(define-simple-struct $if $IF #f
  (src       ; original source for debugging
   test      ; IForm for test expression
   then      ; IForm for then expression
   else      ; IForm for else expression
   ))

;; $let <src> <type> <lvars> <inits> <body>
(define-simple-struct $let $LET #f
  (src       ; original source for debugging
   type      ; indicates scope: 'let for normal let, 'rec[*] for letrec[*], 
   lvars     ; list of lvars
   inits     ; list of IForms to initialize lvars
   body      ; IForm for the body
   ))

;; $receive <src> <reqargs> <optarg> <lvars> <expr> <body>
(define-simple-struct $receive $RECEIVE #f
  (src       ; original source for debugging
   reqargs   ; # of required args
   optarg    ; 0 or 1, # of optional arg
   lvars     ; list of lvars
   expr      ; IForm for the expr to yield multiple values
   body      ; IForm for the body
   ))

;; $lambda <src> <reqargs> <optarg> <lvars> <body> [<flag>]
(define-simple-struct $lambda $LAMBDA #f
  (src              ; original source for debugging
   name             ; inferred name of this closure
   reqargs          ; # of required args
   optarg           ; 0 or 1, # of optional arg
   lvars            ; list of lvars
   body             ; IForm for the body
   flag             ; Marks some special state of this node.
                    ;   'dissolved: indicates that this lambda has been
                    ;               inline expanded.
                    ;   'used: indicates that this lambda has been already dealt
                    ;          with, and need to be eliminated.  This one is
                    ;          specifically used for communication between
                    ;          pass2/$CALL and pass2/$LET.
                    ;   <packed-iform>  : inlinable lambda
   ;; The following slots are used temporarily during pass2-5, and
   ;; need not be saved when packed.
   (calls '())      ; list of call sites
   (free-lvars '()) ; list of free local variables
   (lifted-var #f)  ; if this $LAMBDA is lifted to the toplevel, this slot
                    ; contains an lvar to which the toplevel closure
                    ; is to be bound.  See pass 4.
   ))

;; $label <src> <label> <body>
(define-simple-struct $label $LABEL #f
  (src       ; original source for debugging
   label     ; label.  #f in Pass 2.  Assigned in Pass 5.
   body      ; IForm for the body
   ))

;; $seq <body>
(define-simple-struct $seq $SEQ #f
  (body      ; list of IForms
   ))

;; $call <src> <proc> <args> [<flag>]
(define-simple-struct $call $CALL #f
  (src       ; original source for debugging
   proc      ; IForm for the procedure to call.
   args      ; list of IForms for arguments.
   flag      ; #f, 'local, 'embed, 'jump, 'rec or 'tail-rec.
   ;; Transient slots
   (renv '()) ; runtime env.  used in embed calls to record depth of env
              ;   in Pass 5.
   ))

;; $asm <src> <insn> <args>
(define-simple-struct $asm $ASM #f
  (src       ; original source for debugging
   insn      ; instruction (<code> [<param> ...])
   args      ; list of IForms
   ))

;; $it
;;   A special node.  See the explanation of $if above.
(define $it (let ((c `#(,$IT))) (^[] c)))

;; The followings are builtin version of standard procedures.
;;
(define-simple-struct $cons $CONS #f (src arg0 arg1))

(define-simple-struct $append $APPEND #f (src arg0 arg1))
(define-simple-struct $memv   $MEMV   #f   (src arg0 arg1))
(define-simple-struct $eq?    $EQ?    #f    (src arg0 arg1))
(define-simple-struct $eqv?   $EQV?   #f   (src arg0 arg1))
(define-simple-struct $vector $VECTOR #f (src args))
(define-simple-struct $list   $LIST   #f   (src args))
(define-simple-struct $list*  $LIST*  #f  (src args))
(define-simple-struct $list->vector $LIST->VECTOR #f (src arg0))

(define gauche-module (find-module 'gauche))

(define (global-id symbol :optional (module gauche-module))
  ((with-module gauche.internal make-identifier)
   symbol
   module
   '()))
   
(define define. (global-id 'define))
(define define-constant. (global-id 'define-constant))
(define define-in-module. (global-id 'define-in-module))
(define begin. (global-id 'begin))
(define if. (global-id 'if))
(define or. (global-id 'or))
(define and. (global-id 'and))
(define dynamic-wind. (global-id 'dynamic-wind))
(define set!. (global-id 'set!))
(define let. (global-id 'let))
(define letrec. (global-id 'letrec))
(define letrec*. (global-id 'letrec*))
(define lambda. (global-id 'lambda))
(define receive. (global-id 'receive))
(define lazy. (global-id 'lazy))
(define eager (global-id 'eager))
(define ash. (global-id 'ash))

(define asm->proc-symbol
  (let1 tbl (let ([insn-proc-alist
                    '((APPEND . append)
                      (LOGXOR . logxor) 
                      (LOGIOR .  logior)
                      (LOGAND . logand)
                      (APPLY . apply)
                      (TAIL-APPLY . apply)
                      (ASSQ . assq)
                      (ASSV . assv)
                      (CAAR . caar)
                      (CADR . cadr)
                      (CAR . car)
                      (CDAR . cdar)
                      (CDDR . cddr)
                      (CDR . cdr)
                      (CHARP . char?)
                      (CONS . cons)
                      (CONSTU . undefined)
                      (CURERR . current-error-port)
                      (CURIN . current-input-port)
                      (CUROUT . current-output-port)
                      (EOFP . eof-object?)
                      (EQ . eq?)
                      (EQV . eqv?)
                      (IDENTIFIERP . identifier?)
                      (NUMBERP . number?)
                      (REALP . real?)
                      (IS-A . is-a?)
                      (LENGTH . length)
                      (LIST . list)
                      (LIST-STAR . list*)
                      (MEMQ . memq)
                      (MEMV . memv)
                      (NEGATE . -)
                      (NOT . not)
                      (NULLP . null?)
                      (NUMADD2 . +)
                      (NUMDIV2 . /)
                      (NUMEQ2 . =)
                      (NUMGE2 . >=)
                      (NUMGT2 . >)
                      (NUMIADD2 . +.)
                      (NUMIDIV2 . /.)
                      (NUMIMUL2 . *.)
                      (NUMISUB2 . -.)
                      (NUMLE2 . <=)
                      (NUMLT2 . <)
                      (NUMMUL2 . *)
                      (NUMSUB2 . -)
                      (PAIRP . pair?)
                      (PEEK-CHAR . peek-char)
                      (READ-CHAR . read-char)
                      (REVERSE . reverse)
                      (SETTER . setter)
                      (SLOT-REF . slot-ref)
                      (SLOT-SET . slot-set!)
                      (STRINGP . string?)
                      (SYMBOLP . symbol?)
                      (VALUES . values)
                      (VEC . vector)
                      (VEC-LEN . vector-length)
                      (VEC-REF . vector-ref)
                      (VEC-SET . vector-set!)
                      (VECTORP . vector?)
                      (WRITE-CHAR . write-char))]
                  [tbl (make-hash-table)])
              (for-each
                (lambda (pair)
                  (if-let1 proc-sym (assq-ref insn-proc-alist (car pair))
                    (hash-table-put! tbl (slot-ref (cdr pair) 'code) (global-id proc-sym))))
                (class-slot-ref <vm-insn-info> 'all-insns))
              tbl)
    (lambda (insn)
      (let1 proc (hash-table-get tbl insn #f)
        (if proc
          proc
          (errorf "Unknwon $ASM ~a" insn))))))

;; IForm scanner
(define-macro (case-iform iform . pat-list)
  (let ((iform-type (gensym))
        (src (gensym)))
    `(let ((,iform-type (vector-ref ,iform 0)))
       ,(let loop ((rest pat-list))
          (cond
           ((null? rest) #f)
           (else
            `(if (eq? ,iform-type ,(caar (list-ref rest 0)))
                 (let ,(do ((i 1 (+ i 1))
                            (args (cdar (list-ref rest 0)) (cdr args))
                            (lvars '()))
                           ((null? args)
                            lvars)
                         (case (car args)
                           (($src)
                            (push! lvars `(,src (vector-ref ,iform ,i))))
                           ((_))
                           (else
                            (push! lvars `(,(car args) (vector-ref ,iform ,i))))))
                   ,(if (memq '$src (cdar (list-ref rest 0)))
                        `(with-src ,src ,@(cdr (list-ref rest 0)))
                        `(begin ,@(cdr (list-ref rest 0)))))
                 ,(loop (cdr rest)))))))))

(define-macro (case/unquote obj . clauses)
  (let1 tmp (gensym)
    (define (expand-clause clause)
      (match clause
        (((item) . body)
         `((eqv? ,tmp ,item) ,@body))
        (((item ...) . body)
         (let1 ilist (list 'quasiquote
                           (map (cut list 'unquote <>) item))
           `((memv ,tmp ,ilist) ,@body)))
        (('else . body)
         `(else ,@body))))
    `(let ((,tmp ,obj))
       (cond ,@(map expand-clause clauses)))))

(define ashi-code (slot-ref (assq-ref (class-slot-ref <vm-insn-info> 'all-insns) 'ASHI) 'code))
(define push-handlers-code (slot-ref (assq-ref (class-slot-ref <vm-insn-info> 'all-insns) 'PUSH-HANDLERS) 'code))
(define uvec-ref-code (slot-ref (assq-ref (class-slot-ref <vm-insn-info> 'all-insns) 'UVEC-REF) 'code))
(define promise-code (slot-ref (assq-ref (class-slot-ref <vm-insn-info> 'all-insns) 'PROMISE) 'code))

(define-enum .uvector-types.
             SCM_UVECTOR_S8
             SCM_UVECTOR_U8
             SCM_UVECTOR_S16
             SCM_UVECTOR_U16
             SCM_UVECTOR_S32
             SCM_UVECTOR_U32
             SCM_UVECTOR_S64
             SCM_UVECTOR_U64
             SCM_UVECTOR_F16
             SCM_UVECTOR_F32
             SCM_UVECTOR_F64
             )

(define (form-is-define-in-module? original translated)
  (and
    (pair? original)
    (eq? (car original) 'define-in-module)
    (not (null? (cdr original))) ;;has module name?
    (symbol? (cadr original)) ;;module name is symbol?
    (pair? translated)
    (eq? (car translated) define.)
    ))

(define (iform->sexp iform exp-hook)
  (define (make-local-env lvars)
    (let1 env (map
                (lambda (lvar)
                  (cons
                    lvar
                    (let1 name (lvar-name lvar) 
                      (if (symbol? name)
                        (gensym (symbol->string name))
                        name))))
                lvars)
      (values (map cdr env) env)))
  (define (resolve-local-var lvar env)
    (if-let1 sym (assq-ref env lvar)
      sym
      (errorf "unknown local var ~a" lvar)))
  (define (args->arg-declare args optarg)
    (if (zero? optarg)
      args
      (let1 len (length args)
        (if (= len 1)
          (car args)
          (receive (req rest) (split-at args (- len 1))
            (append req (car rest)))))))
  (define (dynamic-wind? iform)
    (and
      (= (iform-tag iform) $LET)
      (= (length ($let-lvars iform)) 3)
      (= (iform-tag ($let-body iform)) $SEQ)
      (= (length ($seq-body ($let-body iform))) 3)
      (= (iform-tag (cadr ($seq-body ($let-body iform)))) $ASM)
      (= (car ($asm-insn (cadr ($seq-body ($let-body iform))))) push-handlers-code)))

  (define (rec env iform)
    (case/unquote
     (iform-tag iform)
     [($DEFINE)
      (let1 translated `(,(if (memq 'const ($define-flags iform)) define-constant. define.)
                          ,(identifier->symbol ($define-id iform))
                          ,(rec env ($define-expr iform)))
        (if (form-is-define-in-module? ($define-src iform) translated)
          `(,define-in-module. ,(cadr ($define-src iform)) ,@(cdr translated))
          translated))]
     [($LREF)
        (exp-hook
          (resolve-local-var ($lref-lvar iform) env)
          (lvar-name ($lref-lvar iform)))]
     [($LSET)
      `(set!
         ,(resolve-local-var ($lset-lvar iform) env)
         ,(rec env ($lset-expr iform)))]
     [($GREF)
      (exp-hook ($gref-id iform) (identifier->symbol ($gref-id iform)))]
     [($GSET)
      `(,set!. ,($gset-id iform) ,(rec env ($gset-expr iform)))]
     [($CONST)
      (let* ([src ($const-value iform)]
             [v (cond
                  [(or (symbol? src)
                     (identifier? src)
                     (pair? src)
                     (module? src)
                     (undefined? src)
                     (eof-object? src))
                   `(quote ,src)]
                  [else src])])
        (exp-hook v src))]
     [($IF)
      (cond
        [(= (iform-tag ($if-then iform)) $IT)
         `(,or. ,(rec env ($if-test iform)) ,(rec env ($if-else iform)))]
        [(= (iform-tag ($if-else iform)) $IT)
         `(,and. ,(rec env ($if-test iform)) ,(rec env ($if-then iform)))]
        [else 
          `(,if.
             ,(rec env ($if-test iform))
             ,(rec env ($if-then iform))
             ,(rec env ($if-else iform)))])]
     [($LET)
      (if (dynamic-wind? iform)
        (let1 inits ($let-inits iform)
          `(,dynamic-wind.
             ,(rec env (cadr inits)) ;before
             ,(rec env (caddr inits));body
             ,(rec env (car inits))));after
        (let*-values ([(lvar-names new-env) (make-local-env ($let-lvars iform))]
                      [(let-name let-env) (case ($let-type iform)
                                            [(let) (values let. env)]
                                            [(rec) (values letrec. (append new-env env))]
                                            [(rec*) (values letrec*. (append new-env env))]
                                            [else (errorf "Unknown type ~a" ($let-type iform))])])
          `(,let-name
             ,(map
                (lambda (lvar-name lvar-iform) `(,lvar-name ,(rec let-env lvar-iform)))
                lvar-names
                ($let-inits iform))
             ,(rec (append new-env env) ($let-body iform)))))]
     [($RECEIVE)
      (receive (args new-env) (make-local-env ($receive-lvars iform))
        `(,receive.
           ,(args->arg-declare args ($receive-optarg iform))
           ,(rec env ($receive-expr iform))
           ,(rec (append new-env env) ($receive-body iform))))]
     [($LAMBDA)
      (receive (args new-env) (make-local-env ($lambda-lvars iform))
        `(,lambda.
           ,(args->arg-declare args ($lambda-optarg iform))
           ,(rec (append new-env env) ($lambda-body iform))))]
     [($SEQ)
      `(,begin. ,@(map (pa$ rec env) ($seq-body iform)))]
     [($CALL)
      (exp-hook `(,(rec env ($call-proc iform)) ,@(map (pa$ rec env) ($call-args iform))) ($call-src iform))]
     [($ASM)
      (let* ([insn ($asm-insn iform)]
             [code (car insn)])
        (exp-hook 
          (cond
            [(= code ashi-code)
             `(,ash.
                ,@(map (pa$ rec env) ($asm-args iform))
                ,(cadr insn))]
            [(= code promise-code)
             `(,lazy.
               ,(rec env ($lambda-body (car ($asm-args iform)))))]
            [(= code uvec-ref-code)
             (let1 type (cadr insn)
               `(,(cond 
                    [(= type SCM_UVECTOR_U8) 'u8vector-ref]
                    [(= type SCM_UVECTOR_S8) 's8vector-ref]
                    [(= type SCM_UVECTOR_U16) 'u16vector-ref]
                    [(= type SCM_UVECTOR_S16) 's16vector-ref]
                    [(= type SCM_UVECTOR_U32) 'u32vector-ref]
                    [(= type SCM_UVECTOR_S32) 's32vector-ref]
                    [(= type SCM_UVECTOR_U64) 'u64vector-ref]
                    [(= type SCM_UVECTOR_S64) 's64vector-ref]
                    [(= type SCM_UVECTOR_F16) 'f16vector-ref]
                    [(= type SCM_UVECTOR_F32) 'f32vector-ref]
                    [(= type SCM_UVECTOR_F64) 'f64vector-ref])
                  ,@(map (pa$ rec env) ($asm-args iform))))]
            [else 
              `(,(asm->proc-symbol code)
                 ,@(map (pa$ rec env) ($asm-args iform)))])
          ($asm-src iform)))]
     [($CONS)
      (exp-hook
        `(cons ,(rec env ($cons-arg0 iform)) ,(rec env ($cons-arg1 iform))) ($cons-src iform))]
     [($APPEND)
      (exp-hook
        `(append ,(rec env ($append-arg0 iform)) ,(rec env ($append-arg1 iform))) ($append-src iform))]
     [($VECTOR)
      (exp-hook
        `(vector ,@(map (pa$ rec env) ($vector-args iform))) ($vector-src iform))]
     [($LIST->VECTOR)
      (exp-hook
        `(list->vector ,(rec env ($list->vector-arg0 iform))) ($list->vector-src iform))]
     [($LIST)
      (exp-hook
        `(list ,@(map (pa$ rec env) ($list-args iform))) ($list-src iform))]
     [($LIST*)
      (exp-hook
        `(list* ,@(map (pa$ rec env) ($list*-args iform))) ($list*-src iform))]
     [($MEMV)
      (exp-hook
        `(memv ,(rec env ($memv-arg0 iform)) ,(rec env ($memv-arg1 iform))) ($memv-src iform))]
     [($EQ?)
      (exp-hook
        `(eq? ,(rec env ($eq?-arg0 iform)) ,(rec env ($eq?-arg1 iform))) ($eq?-src iform))]
     [($EQV?)
      (exp-hook
        `(eqv? ,(rec env ($eqv?-arg0 iform)) ,(rec env ($eqv?-arg1 iform))) ($eqv?-src iform))]
     (else
       (error (string-append "unknown iform:" (x->string iform)))
      )))
  (rec '() iform))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry point
;;

(define-macro (call/gi func . args)
  `((with-module gauche.internal ,func) ,@args))

(define (form-is-define-module? original translated)
  (and
    (pair? original)
    (eq? (car original) 'define-module)
    (not (null? (cdr original))) ;;has module name?
    (symbol? (cadr original)) ;;module name is symbol?
    ))

(define (scan-expression exp exp-hook)
  (let* ([iform (call/gi pass1 exp (make-cenv (call/gi vm-current-module)))]
         [translated (iform->sexp
                       iform
                       exp-hook)])
    (cond
      [(form-is-define-module? exp translated)
       `(with-module ,(cadr exp) ,translated)]
      [else translated])))

