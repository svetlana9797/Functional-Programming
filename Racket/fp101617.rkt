#lang racket

;смята факториел на число с рекурсия
(define (factorial x)
  (if(<= x 1)
  1
  (* x (factorial (- x 1)))))

;проверява дали числото n е просто с итерация
(define (prime? x)
  (define (prime-iter x y)
    (if(<= y (/ x 2))
      ( if( = (remainder x y) 0)
        #f
        (prime-iter x (+ y 1)))
       #t))
  (prime-iter x 2))

;намира броят на цифрите в числото n с итерация
(define (count-digits n)
  (define (helper counter number)
    (if(< number 10)
        counter 
       (helper (+ counter 1) (quotient number 10))))
  (helper 1 n))

;намира броят на цифрите в числото n с рекурсия
(define (count-digits-rec n)
  (if(< n 10) 1
     ((+ 1 (count-digits-rec (quotient n 10))))))
       
;пресмята сумата на числата в интервала [start, end] с итерация
(define (sum-iter start end)
  (define (helper sum curr)
    (if(> curr end)
       sum
       (helper (+ sum curr) (+ 1 curr))))
  (helper 0 start))

;пресмята сумата на числата в интервала [start, end] с рекурсия
(define (sum-rec start end)
  (if(= start end) start
     (+ start (sum-rec (+ 1 start) end))))

(define (id x) x)
(define (1+ x) (+ 1 x))

;accumulate  с рекурсия
(define (accumulate op nv a b condition term changes)  ;(accumulate + 0 1 10 > id 1+)
  (if (condition a b ) nv
      (op (term a) (accumulate op nv (changes a) b condition term changes))))

;accumulate с итерация
;(define (accumulate-iter op nv a b condtion term changes)

;броя на елементите в интервала [a, b], за които предикатът predicate е истина с рекурсия
(define (divisibleBy3 x)
  (if ( = (remainder x 3) 0) #t
      #f))

(define (count predicate a b)
  (if(> a b) 0
     (+ (if (predicate a) 1 0) (count predicate (+ 1 a) b))))

;броя на елементите в интервала [a, b], за които предикатът predicate е истина с итерация
(define (count-iter predicate a b)
  (define (count-helper curr sum)
   (if(> curr b) sum
      (count-helper (+ 1 curr) (+ sum (if (predicate curr) 1 0)))))
  (count-helper a 0))


;проверява дали за всяко цяло число в интервала [a, b] предикатът predicate е истина
(define(for-all? predicate a b)
  (if (> a b) #t
      (if (not(predicate a)) #f
          (for-all? predicate(+ a 1) b))))
(define (square x) (* x x))

;функция, която прилага подадената функция два пъти
(define (double function)
  

;пресмята x^n с итерация
(define (expt-iter x n) 
  (define (helper counter curr)
    (if(> counter n)
       curr
       (helper(+ counter 1) (* curr x))))
  (helper 1 1))

;пресмята x^n с рекурсия
(define (expt-rec x n)
  (if (<= n 0) 1
      (* x (expt-rec x (- n 1)))))

;връща число с цифрите на числото n в обратен ред с итерация
(define (reverse-digits n)
  (define (helper number reversed )
    (if (= number 0)
        reversed
        (helper (quotient number 10)
                 (+ (* reversed 10) (remainder number 10)))))
    (helper n 0))

(define (digit_count n)
  (if(< n 10) 1
     (+ 1 (digit_count (quotient n 10)))))

(define (pow n k)
  (if(<= k 0) 1
     (* n (pow n (- k 1)))))

 ;връща число с цифрите на числото n в обратен ред с рекурсия
(define (reverse-digits-rec n)
  (if(= (digit_count n) 1) n
     (+ (* (remainder n 10) (pow 10 (-(digit_count n) 1))) (reverse-digits-rec (quotient n 10)))))
        