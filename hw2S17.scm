; sales.scm contains all the company's sales.
; You should not modify this file. Your code
; should work for other instances of this file.
(load "sales.scm")

; Helper function
(define (mydisplay value)
	(display value)
	(newline)
	#t
)

; Returns the roots of the quadratic formula, given
; ax^2+bx+c=0. Return only real roots. The list will
; have 0, 1, or two unique roots
(define (quadratic a b c)
	(cond
		((= a 0) (list (/ (* -1 c) b)))
		((< (- (* b b)(* 4 a c)) 0) '())
		((= (- (* b b)(* 4 a c)) 0) (list (/ (- 0 b)(* 2 a))))
		(else (list (/ (+ (* -1 b)(sqrt (- (* b b)(* 4 a c)))) (* 2 a)) (/ (- (* -1 b)(sqrt (- (* b b)(* 4 a c)))) (* 2 a))))
	)
)

(mydisplay (quadratic 1 0 0))
(mydisplay (quadratic 0 1 0))
(mydisplay (quadratic 3 4 2))

; Return a list with the original list's values doubled.
(define (doubleIt lst)
	(cond
		((NULL? lst) '())
		(else (append (list (* 2 (car lst)))(doubleIt(cdr lst))))
	)
)

(mydisplay (doubleIt '(-1 1 2 3 4 -4 5)))

; Returns the union of two sets. The inputs are flat lists
; of atoms. The result is a flat list with all the elements
; that appear. No duplicates are present in the result. Order
; is not important.
; (union '(a b c) '(1 2 a b c)) -> (a b c 1 2)
; (union '(a b c) '(1 2 a b c 0)) -> (a b c 1 2 0)
(define (union lst1 lst2)
	(define (isinlst atm lst)
		(cond
			((NULL? lst) #f)
			((EQUAL? atm (car lst)) #t)
			(else (isinlst atm (cdr lst)))
		)
	)
	
	(cond
		((NULL? lst2) lst1)
		((isinlst (car lst2) lst1) (union lst1 (cdr lst2)))
		(else (union (append lst1 (list (car lst2))) (cdr lst2 )))
	)
)

(mydisplay (union '(a b c) '(1 2 a b c)))
(mydisplay (union '(a b c) '(1 2 a b c 0)))

; Returns a list with the original order reversed.
; The function must use tail recursion.
; (reverseTail '(a b c)) -> (c b a)
; (reverseTail '(a (a a) b) -> (b (a a) a)
; (reverseTail '(0)) -> (0)
;
(define (reverseTail lst)
	(cond
		((NULL? lst) '())
		(else (append(reverseTail(cdr lst))(list (car lst))))
	)
)

(mydisplay (reverseTail '(a b c)))
(mydisplay (reverseTail '(a (a a) b)))
(mydisplay (reverseTail '(0)))

; compose takes two functions and returns a new function that 
; is the composition, F1oF2. The two inputs lambda functions.
(define (compose F1 F2)
		; return a list to replace the workings of the
		; lambda function - for example replace each
		; x in square w/ (* x x x) if sqrOfCube
		(define (replaceX Fo1 Fo2)
			(cond
				((NULL? Fo1) '())
				((not (list? Fo1))
					(cond
						((EQUAL? Fo1 'x) (car Fo2))
						(else Fo1)
					)
				)
				(else (cons (replaceX (car Fo1) Fo2)(replaceX (cdr Fo1) Fo2)))
			)
		)
		
		; "compile" the lambda function for use
		(eval (append (list(car F1)) (list (cadr F1)) (replaceX (cddr F1) (cddr F2)))(interaction-environment))
)

(define square '(lambda (x) (* x  x)))
(define cube '(lambda (x) (* x  x x)))
(define clamp '(lambda (x) (if (< x 0) 0 x)))

(define cubeOfClamp (compose cube clamp))
(define sqrOfCube (compose square cube))
(define clampOfCube (compose clamp cube))

(mydisplay (cubeOfClamp -2))
(mydisplay (cubeOfClamp 2))
(mydisplay (sqrOfCube -2))
(mydisplay (sqrOfCube 2))
(mydisplay (clampOfCube -2))
(mydisplay (clampOfCube 2))

; Returns the order information, give a specific order number.
; Returns the empty list, if order number is invalid.
(define (getOrder sales orderNo)
	(cond
		((NULL? sales) '())
		((= (caar sales) orderNo) (cons (list (car sales)) (getOrder (cdr sales) orderNo)))
		(else (getOrder (cdr sales) orderNo))
	)
)

(mydisplay (getOrder SALES 0))
(mydisplay (getOrder SALES 51))
(mydisplay (getOrder SALES 56550))

; Returns the total profits for all sales. Returned
; orders are not included in this total
(define (totalProfits sales returns)
	; check if atm is in the lst, if so return true
	; if it's not in the list then return false
	(define (isinlst atm lst)
		(cond
			((NULL? lst) #f)
			((EQUAL? atm (car lst)) #t)
			(else (isinlst atm (cdr lst)))
		)
	)
	
	; helper for totalprofits function, makes use of
	; tail recursion, breaks recursive call and returns 
	; the current total value
	(define (profitHelper sales returns total)
		(cond
			((NULL? sales) total)
			((isinlst (caar sales) returns) (profitHelper (cdr sales) returns total))
			(else (profitHelper (cdr sales) returns (+ total (caddr (caddar sales)))))
		)
	)
	
	; call helper w/ initial total value of 0
	(profitHelper sales returns 0)
)

(mydisplay (totalProfits SALES RETURNS))

; Returns the set of  provinces that the company sold
; to.
(define (getProvinces sales)
	; checks if atm is in the list passed,
	; returns true if so, false otherwise
	(define (isinlst atm lst)
		(cond
			((NULL? lst) #f)
			((EQUAL? atm (car lst)) #t)
			(else (isinlst atm (cdr lst)))
		)
	)
	
	; helper for getProvinces, makes use of tail recursion
	; to return a list of provinces where sales were made
	; also ensures each province only appears once in the returned
	; list
	(define (provinceHelper sales provs)
		(cond
			((NULL? sales) provs)
			((isinlst (cadar (cdddar sales)) provs) (provinceHelper (cdr sales) provs))
			(else (provinceHelper (cdr sales) (cons (cadar (cdddar sales)) provs)))
		)
	)
	
	(provinceHelper sales '())
)

(mydisplay (getProvinces SALES))


; Returns the provinces with their profits from that
; province. These are total profits fro each province.
(define (provincialProfit sales returns)
	; standard check for an atm in a lst, used to check
	; if an order has been returned
	(define (isinlst atm lst)
		(cond
			((NULL? lst) #f)
			((EQUAL? atm (car lst)) #t)
			(else (isinlst atm (cdr lst)))
		)
	)
	; modified isinlst function, checks if a province is already initialized into
	; a province:profit pair
	(define (provPairPresent atm lst)
		(cond
			((NULL? lst) #f)
			((EQUAL? atm (caar lst)) (cons (cadar lst) (cdr lst)))
			(else (isinlst atm (cdr lst)))
		)
	)
	(define (provincialProfitHelper sales returns pairs)
		(cond
			((NULL? sales) pairs)
			((isinlst (caar sales) returns) (provincialProfitHelper (cdr sales) returns pairs))
			(else 
				(cond
					((not (provPairPresent (cadar (cdddar sales)) pairs)) (provincialProfitHelper (cdr sales) returns (cons (list (cadar (cdddar sales)) (caddr (caddar sales))) pairs)))
					(else (provincialProfitHelper (cdr sales) returns (cons (list (cadar (cdddar sales)) (+ (caddr (caddar sales)) (car (provPairPresent (cadar (cdddar sales)) pairs))))(cdr pairs))))
				)
			)
		)
	)
	
	(provincialProfitHelper sales returns '())
)

(mydisplay (provincialProfit SALES RETURNS))

,exit
