#lang plai

;Problem 1:
;Solved by myself : Y
;Time taken : About 2 mins
; [contract] get-average: number number -> number
; [purpose] To get average of two number
; [tests] (test (get-average 10 12) 11)
;           (test (get-average 10 24) 17)
(define (get-average a b)
  (/(+ a b) 2))
(test (get-average 10 12) 11)
(test (get-average 10 24) 17)


;Problem 2:
;Solved by myself : Y 
;Time taken : About 5 mins
; [contract] inchworm-travel: number -> number
; [purpose] To get distance of inchworm traveled
; [tests] (test (inchworm-travel 2) 5.08)
;           (test (get-average 10) 25.4)
(define (inchworm-travel a)
  (* a 2.54))
(test (inchworm-travel 2) 5.08)
(test (inchworm-travel 10) 25.4)


;Problem 3:
;Solved by myself : Y 
;Time taken : About 3 mins
; [contract] volume-cube: number -> number
; [purpose] To get volume of the cube
; [tests] (test (volume-cubel 2) 8)
;           (test (volume-cube 4) 64)
(define (volume-cube a)
  (* a a a))
(test (volume-cube 2) 8)
(test (volume-cube 4) 64)


;Problem 4:
;Solved by myself : Y 
;Time taken : About 3 mins
; [contract] my-BMI: number number -> number
; [purpose] To get BMI
; [tests] (test (my-BMI 60 1.7) 21.0)
;         (test (my-BMI 90 1.83) 27.0)

(define (my-BMI w h)
  (round (/ w (* h h))))
(test (my-BMI 60 1.7) 21.0)
(test (my-BMI 90 1.83) 27.0)


;Problem 5:
;Solved by myself : Y 
;Time taken : About 2 hours 30 mins
; [contract] fib: number -> list of number
; [purpose] To get fibonacci numbers
; [tests] (test (fib 6) '(1 1 2 3 5 8))
;         (test (fib 10) '(1 1 2 3 5 8 13 21 34 55))
(define (m-fib n)
  (cond
    ((<= n 1) '1)
    ((<= n 2) '1)
    (else (+ (m-fib (- n 1)) (m-fib (- n 2))))))
(define (fib n)
   (cond
     ((>= n 2)(append (fib(- n 1)) (list(m-fib n))))
     (else '(1))))

(test (fib 6) '(1 1 2 3 5 8))
(test (fib 10) '(1 1 2 3 5 8 13 21 34 55))


(define tax-wh 10)
(define tax-win 20)
(define tax-eng 50)

;Problem 6 - a:
;Solved by myself : Y 
;Time taken : About 10 mins
; [contract] Vehicle: 
; [purpose] To define the type Vehicle
; [tests] (test (Bicycle? king-bicycle) #t)
;         (test (Car? king-bicycle) #f)
;         (test (Airplane? king-bicycle) #f)

(define-type Vehicle
  (Bicycle (wheels number?))
  (Car (wheels number?)
       (windows number?))
  (Airplane (wheels number?)
            (windows number?)
            (engines number?))
)
(define king-bicycle (Bicycle 2))
(test (Bicycle? king-bicycle) #t)
(test (Car? king-bicycle) #f)
(test (Airplane? king-bicycle) #f)

;Problem 6 - b:
;Solved by myself : Y 
;Time taken : About 20 mins
; [contract] vehicle-tax: Vehicle -> number
; [purpose] To get the calculated tax
; [tests] (test (vehicle-tax red-bicycle) 50)
;         (test (vehicle-tax blue-car) 120)
;         (test (vehicle-tax green-airplane) 530)

(define (vehicle-tax V)
  (type-case Vehicle V
    (Bicycle (wh) (* tax-wh wh))
    (Car (wh win) (+ (* tax-wh wh) (* tax-win win)))
    (Airplane (wh win eng) (+ (* tax-wh wh) (* tax-win win) (* tax-eng eng)))
    )
  )
(define red-bicycle (Bicycle 5))
(define blue-car (Car 4 4))
(define green-airplane (Airplane 3 20 2))
(test (vehicle-tax red-bicycle) 50)
(test (vehicle-tax blue-car) 120)
(test (vehicle-tax green-airplane) 530)

;Problem 6 - c:
;Solved by myself : Y 
;Time taken : About 18 mins
; [contract] is-vehicle-safe: Vehicle -> String
; [purpose] To find out if the vehicle is safe
; [tests] (test (is-vehicle-safe  wow-bicycle) "safe")
;         (test (is-vehicle-safe cool-car) "safe")
;         (test (is-vehicle-safe beautiful-airplane) "unsafe")

(define (is-vehicle-safe V)
  (type-case Vehicle V
    (Bicycle (wh)
             (cond
               ((<= wh 4) "safe")
               (else "unsafe")))
    (Car (wh win)
             (cond
               ((and (>= wh 3) (>= win 2)) "safe")
               (else "unsafe")))
    (Airplane (wh win eng)
             (cond
               ((and (>= wh 3) (>= win 10) (>= eng 1)) "safe")
               (else "unsafe")))))

(define wow-bicycle (Bicycle 3))
(define cool-car (Car 3 4))
(define beautiful-airplane (Airplane 3 9 1))

(test (is-vehicle-safe  wow-bicycle) "safe")
(test (is-vehicle-safe cool-car) "safe")
(test (is-vehicle-safe beautiful-airplane) "unsafe")


;Problem 7:
;Solved by myself : Y
;Time taken : About 1 hour
; [contract] update-name: string string list of string -> list of string
; [purpose] To add a source string to the back of the target string located in list of string
; [tests] (test (update-name "claire" " is nice" '("jc" "claire" "kate")) '("jc" "claire is nice" "kate"))
;         (test (update-name "claire" " is nice" strlist2) '("jc" "claire is nice" "kate"))
;         (test (update-name "Hi" " HA HA" strlist1) '("Hi HA HA" "everyone" "nice" "to" "meet" "you"))

(define strlist1 '("Hi" "everyone" "nice" "to" "meet" "you"))
(define strlist2 '("jc" "claire" "kate"))

(define (update-name s1 s2 list)
  (list-set list (index-of list s1) (string-append s1 s2)))

(test (update-name "claire" " is nice" '("jc" "claire" "kate")) '("jc" "claire is nice" "kate"))
(test (update-name "claire" " is nice" strlist2) '("jc" "claire is nice" "kate"))
(test (update-name "Hi" " HA HA" strlist1) '("Hi HA HA" "everyone" "nice" "to" "meet" "you"))

;Problem 8:
;Solved by myself : Y
;Time taken : About 1 hour 30 mins
; [contract] binary-search: list of number number -> list of number
; [purpose] To know the binary-search process flow and history for target number
; [tests] (test (binary-search '(1 2 3 4 5 6 7 8 9 10) 2) '(5 2))
;         (test (binary-search '(1 2 3 4 5 6 7) 5) '(4 6 5))
;         (test (binary-search '(1 2 3 4 5 6 7) 1) '(4 2 1))

(define (binary-search numlist target)
  (cond
    ((equal? target (floor (/ (+ (first numlist) (list-ref numlist (- (length numlist) 1))) 2))) (list target))
    ((< target (floor (/ (+ (first numlist) (list-ref numlist (- (length numlist) 1))) 2)))
     (append (list (floor (/ (+ (first numlist) (list-ref numlist (- (length numlist) 1))) 2)))(binary-search (remv* (member (floor (/ (+ (first numlist) (list-ref numlist (- (length numlist) 1))) 2)) numlist) numlist) target)))
    (else 
     (append (list (floor (/ (+ (first numlist) (list-ref numlist (- (length numlist) 1))) 2)))(binary-search (member (floor (+ 1 (/ (+ (first numlist) (list-ref numlist (- (length numlist) 1)))2))) numlist) target)))))

(test (binary-search '(1 2 3 4 5 6 7 8 9 10) 2) '(5 2))
(test (binary-search '(1 2 3 4 5 6 7) 5) '(4 6 5))
(test (binary-search '(1 2 3 4 5 6 7) 1) '(4 2 1))





