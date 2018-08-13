#lang racket
(require rackunit rackunit/text-ui)

;смята броя на делителите с итерация
(define (count-divisors n)
  (define (count-divisors-iter i br)
    (if ( > i n) br
        ( if (= (remainder n i) 0)
                 (count-divisors-iter (+ 1 i) (+ 1 br))
                 (count-divisors-iter (+ 1 i) br))))
 (count-divisors-iter 1 0))


;смята броя на делителите с рекурсия
(define (is_divisible num n)
    (if(= (remainder n num) 0) 1
           0))

(define (count-divisors-rec n)
  (define (helper num)
    (if(= num 1) 1
       (+ (is_divisible num n) (helper (- num 1)))))
  (helper n))
      

;намира сумата на делителите на число с итерация
(define (sum-divisors n)
  (define (sum-divisors-iter i sum)
    (if(> i n) sum
    (if (= (remainder n i) 0)
             (sum-divisors-iter (+ 1 i) (+ sum i))
             (sum-divisors-iter(+ 1 i) sum))))
  (sum-divisors-iter 1 0))

;елемент от триъгълника на Паскал
(define (binomial-coefficient row index)
  (cond ((= row 1) 1)
        ((= index 1) 1)
        ((= row index) 1)
         (else
            (+
             (binomial-coefficient (- row 1) (- index 1))
             (binomial-coefficient (- row 1) index)))))