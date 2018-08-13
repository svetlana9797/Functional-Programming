#lang racket

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

(define (run-length-encode l)
  (if(null? l) '()
   (cons (cons (car l) (number_of_encounters (car l) l)) (run-length-encode (after (car l) l)))))
     ;(cons (evaluate (car l) 0 l) (run-length-encode (after (car l) l)))))
  
(run-length-encode '( 3 3 3 4 5 3))

;zadacha 2
(define (flatten l)
  (define (iter n)
    (if (= n 0) '()
     (cons (car l) (iter (- n 1)))))
  (iter (cdr l)))

(define (run-length-decode l)
  (if (null? l) '()
      (append (flatten (car l)) (run-length-decode (cdr l)))))

;(run-length-decode (run-length-encode '( 3 3 3 4 5 3)))

;zadacha 3
(define (remove x l)
  (cond((null? l) '())
       ((= (car l) x) (remove x (cdr l)))
       (else (cons (car l) (remove x (cdr l))))))

(define (encounters x l)
(cond ((null? l) 0)
 ((= x (car l)) (+ 1 (encounters x (cdr l))))
   (else (encounters x (cdr l)))))

(define (histogram l)
  (if (null? l) '()
      (cons (cons (car l) (encounters (car l) l)) (histogram (remove (car l) l)))))

;(histogram '(1 2 3 3 4 5 3 5 3))
;(remove 2 (remove 1 '(1 2 3 3 4 5 3 5 3)))

;zadacha 4

(define (create x f l)
  (cond((null? l) '())
       ((= (f (car l)) x) (cons (car l)(create x f (cdr l))))
       (else (create x f (cdr l)))))

(define (remove_by_value x f l)
  (cond ((null? l) '())
        ((= (f (car l)) x) (remove_by_value x f (cdr l)))
       (else (cons (car l)(remove_by_value x f (cdr l))))))
     

(define (group-by f l)
  (if(null? l) '()
     (cons (cons (f (car l)) (create (f (car l)) f l)) (group-by f (remove_by_value (f (car l)) f l)))))
(group-by (λ(x) (remainder x 3)) '(1 2 3 4 5 6 7))