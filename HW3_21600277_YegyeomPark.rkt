#lang plai

; Problem 1:
; Solved by myself: Y
; Time taken: 5 mins
; [contract] parse: sdexp -> SDFAE
; [purpose]  To convert SD-expressions into SDFAEs as abstract syntax

(define-type SDFAE
    [num     (n number?)]
    [add     (lhs SDFAE?) (rhs SDFAE?)]
    [sub     (lhs SDFAE?) (rhs SDFAE?)]
    [id        (name symbol?)]
    [fun     (sd symbol?) (param symbol?) (body SDFAE?)]
    [app     (ftn SDFAE?) (arg SDFAE?)])

(define (parse sdexp)
   (match sdexp
        [(? number?)                (num sdexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun 's i (parse e)) (parse v))]
        [(? symbol?)                (id sdexp)]
        [(list 's 'fun (list p) b)                 (fun 's p (parse b))]
        [(list 'd 'fun (list p) b)                 (fun 'd p (parse b))]
        [(list f a)                 (app (parse f) (parse a))]
        [else                       (error 'parse "bad syntax: ~a" sdexp)]))


; Problem 2:
; Solved by myself: Y
; Time taken: 1 hour
; [contract] SDFAE-Value
; [purpose]  To define SDFAE value. SDFAE can provide dynamic scope and static scope. closureV can provide static scope function, dynamicV can provide dynamic scope function.

(define-type SDFAE-Value
  [numV       (n number?)]
  [closureV   (param symbol?) (body SDFAE?) (ds DefrdSub?)]
  [dynamicV   (param symbol?) (body SDFAE?)]
  [exprV      (expr SDFAE?) (ds DefrdSub?)
                            (value (box/c (or/c false SDFAE-Value?)))])


; [contract] DefrdSub: structure of deferred substitution cache
; [purpose]  To provide deferred substitution fucntion.

(define-type DefrdSub 
  [mtSub]
  [aSub (name symbol?) (value SDFAE-Value?) (ds DefrdSub?)])


; [contract] strict: SDFAE -> SDFAE-value
; [purpose]  To execute delayed evaluation 

(define (strict v)
    (type-case SDFAE-Value v
        [exprV (expr ds v-box)
                     (if (not (unbox v-box))
                          (local [(define v (strict (interp expr ds)))]
                              (begin (set-box! v-box v)
                                           v))
                          (unbox v-box))] 
        [else v]))


; [contract] num-op: SDFAE -> SDFAE-value
; [purpose]  To get the result of arithmetic expression.

(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))


; [contract] lookup: symbol DefrdSub -> SDFAE
; [purpose]  To know the contents of the cache for that symbol(identifier)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()  (error 'lookup "free identifier")]
    [aSub  (i v saved) (if(symbol=? i name)
                               (strict v)             ;; if v is exprV (num ==> interp it
                                (lookup name saved))]))


; [contract] interp: SDFAE DefrdSub -> SDFAE
; [purpose]  To get the result of SDFAE(abstract expression) expression

(define (interp sdfae ds)
  (type-case SDFAE sdfae
     [num (n)      (numV n)]
     [add (l r)    (num+ (interp l ds) (interp r ds))]
     [sub (l r)    (num- (interp l ds) (interp r ds))]
     [id  (s)      (lookup s ds)]
     [fun (s p b)  (if (symbol=? s 's)
                       (closureV p b ds)
                       (dynamicV p b))]
     [app (f a)   (local [(define f-val (strict (interp f ds)))
                          (define a-val (exprV a ds (box #f)))]
                    
                   (if (closureV? f-val)
                       (interp (closureV-body f-val)
                           (aSub (closureV-param f-val)
                                 a-val
                                 (closureV-ds f-val)))
                       (interp (dynamicV-body f-val)
                               (aSub (dynamicV-param f-val)
                                     a-val
                                     ds))))]))


(test (parse '{with {x 3} {with {f {d fun {y} {+ x y}}} {with {x 5} {f 4}}}})(app (fun 's 'x (app (fun 's 'f (app (fun 's 'x (app (id 'f) (num 4))) (num 5))) (fun 'd 'y (add (id 'x) (id 'y))))) (num 3))
)

(interp (parse '{with {x 3} {with {f {d fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub))
