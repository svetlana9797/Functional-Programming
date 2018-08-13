#lang racket

;бройни системи 
;zad1

(define (convert_to_ten x k p) ; ot k v 10
(if(<= x 0) 0
   (+  (* p (remainder x 10)) (convert_to_ten (quotient x 10) k (* p k)))))

(define (convert_from_ten x n p)
(if(<= x 0) 0
   (+ (* p (remainder x n)) (convert_from_ten (quotient x n) n (* p 10)))))

;(convert_to_ten 15 8 1) ;13
;(convert_to_ten 21 8 1) ;17
;(convert_from_ten 13 7 1);16
;(convert_from_ten 17 7 1);23

(define (convert x k n)
  (convert_from_ten (convert_to_ten x k 1) n 1))
;(convert 111011 2 7);113
;(convert 113 7 4);323
;(convert 15 8 7);16
;(convert 1011 2 5);21




;zad 2
(define (after-the-number l)
  (cond((null? l) '())
       ((not(char-numeric? (car l))) l)
       (else (after-the-number (cdr l)))))

(define (convert-to-num l) ;check wheter it is a singe digit or a number
 (define (convert-iter curr l)
         (cond((null? l) curr)
         ((not(char-numeric? (car l))) curr)
         (else (convert-iter (+ (* curr 10)(-(char->integer(car l)) 48)) (cdr l)))))
  (convert-iter 0 l))

(define (sum-numbers l)
  (cond ((null? l) 0)
        ((char-numeric? (car l)) (+ (convert-to-num l )(sum-numbers (after-the-number l)))) 
        (else(sum-numbers(cdr l)))))


;(sum-numbers(string->list "asdc42f"))
;(define l (string->list "23h4gt5a6"))
;(sum-numbers l )
;(not(char-numeric? (cadr (string->list "234gt56"))))
;(cadr (string->list "234gt56"))

; zad 3
(define (number_of_encounters x l)
(cond ((null? l) 0)
 ((not (= x (car l))) 0)
   (else(+ 1 (number_of_encounters x (cdr l))))))

;(number_of_encounters 3 '( 3 3 3 3 45 6))

(define (after x l) ; return the list after the element
  (cond ((null? l) '())
   ((not (= x (car l))) l)
     (else (after x (cdr l)))))

;(after 3 '(3 3 3 4 5 6))

(define (encode l)
  (if(null? l) '()
   (cons (cons (car l) (number_of_encounters (car l) l)) (encode (after (car l) l)))))
  
;(encode '( 3 3 3 4 5 3))

;zad4


(define (search l x v)
  (if (null? l) v
      (if (> ((car l) x) v) (search (cdr l) x ((car l) x))
          (search (cdr l) x v))))

(define (maximize l)
  (lambda(x)(search l x ((car l) x))))

((maximize (list (λ (x) (+ x 10)) (λ (x) (- x 10)) (λ (x) (quotient x 5)) (λ (x) (remainder x 5)) (λ (x) (- x 5)) (λ (x) (* x 5)))) 6) ;30
;((maximize (list (λ (x) (+ x 10) (λ (x) (* x 5))) (λ (x) (- x 5)) (λ (x) (+ x 7)))) 5) ;25