#lang plai

; Problem 1:
; Solved by myself: Y
; Time taken: 1 hour
; [contract] parse: sexp -> BFAE
; [purpose]  To convert expressions into BFAEs as abstract syntax
; Type definition for abstract syntax tree of BFAE
(define-type BFAE
    [num    (n number?)]
    [add     (lhs BFAE?) (rhs BFAE?)]
    [sub     (lhs BFAE?) (rhs BFAE?)]
    [id      (name symbol?)]
    [fun      (param symbol?) (body BFAE?)]
    [newbox  (v BFAE?)]
    [setbox  (bn BFAE?) (v BFAE?)]
    [openbox  (v BFAE?)]
    [seqn  (ex1 BFAE?) (ex2 BFAE?)]
    [app     (ftn BFAE?) (arg BFAE?)]
  )

(define-type BFAE-Value
    [numV (n number?)]
    [closureV (param symbol?) (body BFAE?) (ds DefrdSub?)]
    [boxV (address integer?)]
    [exprV (expr BFAE?) (ds DefrdSub?) (st Store?) (value (box/c (or/c false BFAE-Value?))) ])
         
; parse: sexp -> BFAE
; purpose: to convert sexp to BFAE
(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'fun (list p) b)                 (fun p (parse b))]
        [(list 'newbox v)           (newbox (parse v))]
        [(list 'setbox bn v)        (setbox (parse bn)(parse v))]
        [(list 'openbox v)          (openbox (parse v))]
        [(list 'seqn ex1 ex2)       (seqn (parse ex1)(parse ex2))]
        [(list f a)                 (app (parse f) (parse a))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))

; Problem 2:
; Solved by myself: Y
; Time taken: 6 hours


; [contract] DefrdSub: structure of deferred substitution cache
; [purpose]  To provide deferred substitution fucntion.
(define-type DefrdSub
  [mtSub]
   [aSub (name symbol?) (address integer?) (ds DefrdSub?)])

; [contract] Store: structure of store
; [purpose]  To provide Store(memory space) fucntion.
(define-type Store
            [mtSto]
            [aSto  (address integer?) (value BFAE-Value?)
                       (rest Store?)])

; [contract] Value*Store : structure of Value*Store
; [purpose]  Type definition for Value*Store
(define-type Value*Store
    [v*s (value BFAE-Value?) (store Store?)])

; [contract] malloc : Store -> Integer
; [purpose]  return new address
(define (malloc st)
    (+ 1 (max-address st)))

; [contract] max-address: Store -> Integer
; [purpose] return max address of used Store 
(define (max-address st)
    (type-case Store st
        [mtSto () 0]
        [aSto (n v st)
                  (max n (max-address st))]))

; [contract] num-op: BFAE -> BFAE-value
; [purpose]  To get the result of arithmetic expression.
(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

; [contrac] lookup: symbol DefrdSub -> BFAE-Value
; [purpose] To get a value for the given identifier (symbol)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()                  (error 'lookup "free identifier")]
    [aSub  (i adr saved) (if(symbol=? i name)
                                            adr
                                            (lookup name saved))]))

; [contrac] store-lookup: address Store -> BFAE-Value
; [purpose] To get a value for the given address(integer)
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto ()           (error 'store-lookup "No value at address")]
    [aSto   (location value rest-store)
                               (if(= location address)
                                  (strict value)
                                  (store-lookup address rest-store))]))


; [contract] strict: BFAE -> BFAE-value
; [purpose]  To execute delayed evaluation 
(define (strict v)
    (type-case BFAE-Value v
        [exprV (expr ds st v-box)
                     (if (not (unbox v-box))
                          (local [(define v (interp expr ds st))]
                              (begin
                                 (set-box! v-box (v*s-value v))
                                           (v*s-value v)))
                          (unbox v-box))] 
        [else v]))


; [contract] run: sexp -> Value*Store
; [purpose] to run parse and interp in one queue.
(define (run sexp ds st)
     (interp (parse sexp) ds st))


; [contract] interp-two: BFAE BFAE DefrdSub Store LamdaFuction -> Value*Store
; [purpose]  to get Value*Store value from BFAE expression
(define (interp-two expr1 expr2 ds st handle)
    (type-case Value*Store (interp expr1 ds st)
        [v*s (val1 st2)
            (type-case Value*Store (interp expr2 ds st2)
                [v*s (val2 st3)
                    (handle val1 val2 st3)])]))

; [contract]interp: BFAE DefrdSub Store -> Value*Store
; [purpose] to get Value*Store from BFAE
(define (interp bfae ds st)
  (type-case BFAE bfae
     [num (n)     (v*s (numV n) st)]
     [add (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]
     [sub (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]
     [id      (s)     (v*s (store-lookup (lookup s ds) st) st)]
     [fun (p b)   (v*s (closureV p b ds) st)]
     [newbox (val) 
                  (type-case Value*Store (interp val ds st)
                     [v*s   (vl st1)
                         (local [(define a (malloc st1))]
                                (v*s (boxV a) (aSto a (exprV val ds st (box #f)) st1)))])]
     [openbox (bx-expr)
                  (type-case Value*Store (interp bx-expr ds st)
                     [v*s (bx-val st1)
                         (v*s (store-lookup (boxV-address bx-val) 
                               st1)
                                st1)])]
     [setbox (bx-expr val-expr)
                            (interp-two bx-expr val-expr ds st
                                    (lambda (bx-val val st1)
                                            (v*s val
                                                    (aSto (boxV-address bx-val)
                                                                val
                                                                st1))))]
     [seqn (a b) (interp-two a b ds st (lambda (v1 v2 st1) (v*s v2 st1)))]
     [app (f a)
          (type-case Value*Store  (interp f ds st)
                    [v*s (f-value f-store)
                         (type-case BFAE a
                           [newbox (val)
                                   (local [(define vs_a (interp a ds f-store))
                                           (define new-address (malloc (v*s-store vs_a)))]
                                    (interp (closureV-body f-value)
                                        (aSub (closureV-param f-value)
                                            new-address
                                            (closureV-ds f-value))
                                        (aSto new-address
                                            (v*s-value vs_a)
                                            (v*s-store vs_a))))]
                           [openbox (bx-expr)
                                    (local [(define vs_a (interp a ds f-store))
                                           (define new-address (malloc (v*s-store vs_a)))]
                                    (interp (closureV-body f-value)
                                        (aSub (closureV-param f-value)
                                            new-address
                                            (closureV-ds f-value))
                                        (aSto new-address
                                            (v*s-value vs_a)
                                            (v*s-store vs_a))))]
                           [setbox (bx-expr val-expr)
                                   (local [(define vs_a (interp a ds f-store))
                                           (define new-address (malloc (v*s-store vs_a)))]
                                    (interp (closureV-body f-value)
                                        (aSub (closureV-param f-value)
                                            new-address
                                            (closureV-ds f-value))
                                        (aSto new-address
                                            (v*s-value vs_a)
                                            (v*s-store vs_a))))]
                           [seqn (a b)
                                 (local [(define vs_a (interp a ds f-store))
                                           (define new-address (malloc (v*s-store vs_a)))]
                                    (interp (closureV-body f-value)
                                        (aSub (closureV-param f-value)
                                            new-address
                                            (closureV-ds f-value))
                                        (aSto new-address
                                            (v*s-value vs_a)
                                            (v*s-store vs_a))))]
                           [else 
                                (local [(define new-address (malloc f-store))
                                        (define a-val (exprV a ds f-store (box #f)))]
                                    (interp (closureV-body f-value)
                                        (aSub (closureV-param f-value)
                                            new-address
                                            (closureV-ds f-value))
                                        (aSto new-address
                                            a-val
                                            f-store)))])])]))