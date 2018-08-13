#lang racket
;връща броя на елементите в интервала [a, b], за които предикатът predicate е истина

(define (accumulate combiner nv term a next b)
  (if(> a b) nv
     (combiner(term a) (accumulate combiner nv term (next a) next b))))

;>(accumulate + 0 (lambda(x) x) 1 (lambda(x) (+ 1 x)) 3)

(define (count predicate a b)
  (accumulate + 0 (lambda(x) (if(predicate x) 1 0)) a (lambda(x) (+ 1 x)) b))

;наредена двойка
#|>(cons 1 2)
>(cons 1(cons 2 3))
>(cons 1(cons 2 (cons 3'()))) ;последният елемент е винаги празният списък '()
(define pair (cons 1 2))
> (car pair) ;първи елемент
>(cdr pair) ;втори елемент
>(null? '()) ; проверка дали списък е празен

>(list) ;връща '()
> (list 1 2 3 4) ; създава списък
(define l (list 1 2 3 4 5))
>(car l) ;first element
>(car(cdr l)) ;second element
>(cdr l) ; returns the list without the first element
>(caddr l) ; returns the third element
>(caar (list(list 1 2) 3) ; the first element from the pair is a list
|#

;finds the length of a list
(define (length l)
  (if(null? l) 0
     (+ 1 (length(cdr l)))))

;finds the sum of the list elements
(define (sum l)
  (if(null? l) 0
     (+ (car l) (sum (cdr l)))))

;finds if x is in the list
(define (member? l x)
  (if (null? l) #f   
    (if(= (car l) x) #t
       (member? (cdr l) x))))

;returns the last element
(define (last l)
   (if(null? (cdr l)) (car l)  
         (last (cdr l))))

; returns the element on position n
(define (nth l n)
  (if (or (= n 0) (null? l)) (car l)
     (nth (cdr l) (- n 1))))

;returns a list whose elements are multiplied by x
(define (scale l x)
  (if (null? l)  '()
    (cons (* (car l) x) (scale (cdr l) x))))

;>(scale (list 1 2 3 4) 3)

;returns the reversed list
(define (reversed l)
  (define (rev-iter n)
    (if (= n 0) '()
        (cons (nth l (- n 1)) (rev-iter(- n 1)))))
  (rev-iter (length l)))

;adds x to the end of the list
(define (add-last l x)
  (if (null? l) (cons x '())
      (cons (car l) (add-last (cdr l) x))))

;concatenate 2 lists
(define (append l1 l2)
  (if (null? l1) l2
      (cons( car l1) (append (cdr l1) l2))))

;прилага f върху всеки елемент от списъка
(define (map f l)
  (if(null? l) '()
     (cons (f (car l)) (map f (cdr l)))))
  
;doubles the function given
(define (square x) (* x x))
(define (double f) 
  (lambda(x) (f(f x))))

;finds the elements which are true for the predicate
(define (filter l p)
  (if (null? l) '()
    (if(p (car l))
         (cons (car l) (filter (cdr l) p))
         (filter (cdr l) p))))


(define(add2 x) (+ x 2))
;accumulate for lists
(define (accumulate_for_lists combiner nv term procedure next predicate l )
  (if(predicate l) nv
     (combiner (procedure(term l)) (accumulate_for_lists combiner nv term procedure next predicate (next l)))))

;>(accumulate_for_lists cons '() car add2 cdr null? (list 1 2 3 4))


;returns the maximum element from the list
(define (maximum l)
  (define (max-iter x l)
    (cond ((null? l) x)
          ((> (car l) x) (max-iter (car l) (cdr l)))
          (else (max-iter x (cdr l)))))
  (max-iter (car l) l))

;премахва първото срещане на елемента x от списъка
(define (remove l x)
  (cond ((null? l) '())
        ((= (car l) x) (cdr l))
        (else (cons (car l) (remove (cdr l) x)))))
