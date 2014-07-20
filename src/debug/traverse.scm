(define-module debug.traverse
  (use srfi-11) ;;let*-values
  (export scan-expression global-id))

(select-module debug.traverse)

(include "traverse-struct.scm")

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

(define (form-is-define-in-module? original translated)
  (and
    (pair? original)
    (eq? (car original) 'define-in-module)
    (not (null? (cdr original))) ;;has module name?
    (symbol? (cadr original)) ;;module name is symbol?
    (pair? translated)
    (eq? (car translated) define.)
    ))

(define (iform->sexp iform exp-hook iform-hook)
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
    (iform-hook
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
          ))
      iform))
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

(define (scan-expression exp exp-hook iform-hook)
  (let* ([iform (call/gi pass1 exp (make-cenv (call/gi vm-current-module)))]
         [translated (iform->sexp
                       iform
                       exp-hook
                       iform-hook
                       )])
    (cond
      [(form-is-define-module? exp translated)
       `(with-module ,(cadr exp) ,translated)]
      [else translated])))

