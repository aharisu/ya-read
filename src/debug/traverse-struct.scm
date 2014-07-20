(use util.match)
(use gauche.vm.insn)

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
