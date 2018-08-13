#lang racket
;дали n е равно на сбора на делителите си
(define (perfect-number? n)
  (define (divisible? x)
    (if(= 0 (remainder n x)) #t
       #f))
  (define (perfect? sum divisor)
    (cond((and (= sum n) (>= divisor n)) #t)
         ((or (> sum n) (> divisor n)) #f)
         (else
         (if(divisible? divisor) (perfect? (+ sum divisor) (+ 1 divisor))
            (perfect? sum (+ 1 divisor))))))
  (perfect? 0 2))

;дали цифрите на числото n са подредени в нарастващ ред
(define (inc-digits? n)
  (define (inc-iter prev num)
    (cond ((= num 0) #t)
          (( <= prev (remainder num 10)) #f)
          (else
           (inc-iter (remainder num 10) (quotient num 10)))))
  (inc-iter (remainder n 10) (quotient n 10)))
      
;проверява дали n^2 завършва с цифрите на n
(define (automorphic? n)
  (define (auto-iter num n)
    (cond((= n 0) #t)
         ((not( = (remainder n 10) (remainder num 10))) #f)
         (else
          (auto-iter (quotient num 10) (quotient n 10)))))
  (auto-iter (* n n) n))

;дали n е просто число от вида 2^k - 1
(define (prime? n)
  (define (prime-iter num)
    (cond ((> num (quotient n 2)) #t)
          ((= 0 (remainder n num)) #f)
          (else
           (prime-iter (+ 1 num)))))
  (prime-iter 2))

 (define (mersenne? n)
   (define(mersenne-iter num)
     (cond((= num 1) #t)
          ((= 1 (remainder num 2)) #f)
     (else
      (mersenne-iter (quotient num 2)))))
   (mersenne-iter (+ n 1)))

(define (mersenne-prime? n)
  (if(and (mersenne? n) (prime? n)) #t
     #f))