#lang plai

; Problem 1:
; Solved by myself: Y
; Time taken: 2 hours 30 mins
; [contract] parse: exp -> PWAE
; [purpose]  To convert expressions into PWAEs as abstract syntax

;<PWAE> ::= <num>                            
;	| <op>
;	| <id>
;	| <keyword>
;	| {<PWAE> <PWAE> <op>}
;    	| {{<id> <PWAE>} <PWAE> <keyword>}

(define-type PWAE
  [num (n number?)]
  [op (sign symbol?)]
  [id (name symbol?)]
  [keyword (key symbol?)]
  [postfix (lhs PWAE?) (rhs PWAE?) (OP op?)]
  [substitute (name symbol?) (var PWAE?) (body PWAE?) (key keyword?)])

(define (parse exp)
  (match exp
    [(? number?) (num exp)]
    [(? symbol?) (id exp)]
    [(list l r '+) (postfix (parse l) (parse r) (op 'add))]
    [(list l r '-) (postfix (parse l) (parse r) (op 'sub))]
    [(list (list i v) e 'with) (substitute i (parse v) (parse e) (keyword 'with))]
    ))



; Problem 3:
; Solved by myself: Y
; Time taken: 1 hour
; [contract] binding-ids: PWAE -> list of symbol
; [purpose] To find binding identifiers in the given expression(abstract syntax)

(define (binding-ids exp)
  (type-case PWAE exp
    [num (n) empty]
    [op (sign) empty]
    [id (name) empty]
    [keyword (key) empty]
    [postfix (l r op) (binding-ids l) (binding-ids r)]
    [substitute (name var body key) (remove-duplicates (sort (append (list name) (binding-ids var) (binding-ids body)) symbol<?))]
    ))


; Problem 4:
; Solved by myself: N  My friend gives me a idea where bound identifier is located in substitute body place.
; Time taken: 2 hours 30 mins
; [contract] bound-ids: PWAE -> list of symbol
; [purpose] To find bound identifiers in the given expression(abstract syntax)

(define (bound-ids exp)
  (type-case PWAE exp
    [num (n) empty]
    [op (sign) empty]
    [id (name) empty]
    [keyword (key) empty]
    [postfix (l r op) (sort (append (bound-ids l) (bound-ids r)) symbol<?)] 
    [substitute (name var body key) (remove-duplicates (sort (append (bound-ids var) (bound-ids body) (cond
                                      [(eq? #f (member (first (binding-ids exp)) (id_find body))) empty] 
                                      [else (list (first (binding-ids exp)))] 
                )) symbol<?))]))

; [contract] id_find: PWAE -> list of symbol
; [purpose] To find all of 'id' symbol in the given expression

(define (id_find expr)
    (type-case PWAE expr
    [num (n) empty]
    [op (sign) empty]
    [id (name) (list name)]
    [keyword (key) empty]
    [postfix (l r op) (append (id_find l) (id_find r))]
    [substitute (name var body key) (append (id_find var) (id_find body))]
    ))

;;NOTICE Problem 2 was placed at the bottom of the code by using the function declared in Problem 3.
; Problem 2:
; Solved by myself: Y
; Time taken: 2 hours 30 mins
; [contract] free-ids: exp -> PWAE
; [purpose]  To find free identifiers in the given expression(abstract syntax)

(define (free-ids exp)
  (remove-duplicates (sort (free_find (purebound exp) (id_find exp)) symbol<?)))

; [contract] detect: symbol list of symbol -> list of symbol
; [purpose]  To make a list of symbol by selecting only target symbol from the input list

(define (detect var lst)
  (cond
    [(eq? (member var lst) #f) empty]
    [else(append (list (first (member var lst))) (detect var (rest (member var lst))))]))

; [contract] purebound : PWAE -> list of symbol
; [purpose]  This functin makes a list of symbol which is consist of bound identifier and allow duplicates in the PWAE expression 

(define (purebound exp)
  (type-case PWAE exp
    [num (n) empty]
    [op (sign) empty]
    [id (name) empty]
    [keyword (key) empty]
    [postfix (l r op) (sort (append (purebound l) (purebound r)) symbol<?)] 
    [substitute (name var body key) (sort (append (purebound var) (purebound body) (cond
                                      [(eq? #f (member (first (binding-ids exp)) (id_find body))) empty] 
                                      [else (detect (first (binding-ids exp)) (id_find body))] 
                )) symbol<?)]))

; [contract] myfirst : list of symbol -> symbol
; [purpose]  To get first symbol in input list of symbol. If input list is empty, return empty list  

(define (myfirst lst)
  (cond
   [(eq? lst empty) empty]
   [else (first lst)]))

; [contract] free_find : list of symbol_1 list of symbol_2 -> list of symbol
; [purpose]  In the contents of list2, erase the symbol that overlaps with list1 and if there is no symbol that overlaps, return list2.

(define (free_find var lst)
  (cond
   [(eq? var empty) lst]
   [else (free_find  (rest var) (remove (myfirst var) lst))]))  


