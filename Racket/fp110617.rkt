#lang racket

;returns the maximum element from the list
(define (maximum l)
  (define (max-iter x l)
    (cond ((null? l) x)
          ((> (car l) x) (max-iter (car l) (cdr l)))
          (else (max-iter x (cdr l)))))
  (max-iter (car l) l))

;sort a list
(define (compare l comp)
  (define (comp-iter x l)
    (cond ((null? l) x)
          ((comp (car l) x) (comp-iter (car l) (remove l x)))
          (else ( comp-iter x (remove l (car l))))))
  (comp-iter (car l) l))

(define (selection_sort l cmp)
  (if(null? l) '()
     (cons (compare l cmp) (selection_sort (remove l (compare l cmp)) cmp))))

;връща списък от два подсписъка -> #t и #f за p
(define (filter p l)
  (if(null? l) '()
     (if (p (car l)) (cons(car l) (filter p (cdr l)))
         (filter p (cdr l)))))

(define (partition p l)
      (cons (filter p l) (list(filter (λ(x) (not(p x))) l))))

;приема списък от атоми и списъци с атоми и връща списък с всички атоми
(define (flatten l)
  (cond((null? l) '())
       ((pair? (car l)) (flatten(append (car l) (cdr l))))
         (else (cons (car l) (flatten (cdr l))))))

;(append '(1 2) '(3 4))
;(flatten '(3 (1 2 3) 4 5))
;(flatten '((1 2) 3 (4 5) (6 7)))

(define (add2 x )(+ 2 x))
;прилага f върху всеки атом от всеки вложен списък в списъка l
(define (map-deep f l)
  (cond ((null? l) '())
        ((pair? (car l)) (cons (map-deep f (car l)) (map-deep f (cdr l))))
        (else (cons (f (car l)) (map-deep f (cdr l))))))

;връща списък от наредени двойки (ai, bi), където ai и bi са елементи в a и b
(define (zip a b)
  (if(or (null? a) (null? b)) '()
    (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))))

;премахва първото срещане на елемента x от списъка
(define (remove x l)
  (cond ((null? l) '())
        ((= (car l) x) (cdr l))
        (else (cons (car l) (remove x (cdr l))))))

;премахва повтарящите се елементи
(define (remove-duplicates l)
   (if(null? l) '()
      (cons (car l) (remove-duplicates(remove (car l) (cdr l))))))

;която разбива списъка l на подсписъци с дължина n
(define (separate n l)
  (if (or(null? l) (= 0 n)) '()
      (cons (car l) (separate (- n 1) (cdr l)))))

(define (removed n l)
  (if (or (null? l) (= 0 n)) l
     (removed (- n 1) (cdr l))))
      
(define (chunk n l)
  (if(null? l) '()
    (cons (separate n l) (chunk n (removed n l)))))
;
;;;
;;;;
;matrixes
;връща наредена двойка с броя редове и броя колони на матрицата
(define (dimensions m)
  (cons (length (car m)) (length m)))

(define m '((1 2 3) ( 4 5 6) (7 8 9) (10 11 12)))

 ;revrses the columns in the matrix
(define (reverse-columns m)
  (if(null? m) '()
     (cons (reverse (car m)) ( reverse-columns (cdr m)))))

;returns the nth column in the matrix
(define (get-element n l)
  (if ( = n 1) (car l)
      (get-element (- n 1) (cdr l))))
;(get-element 3 '(1 2 3 4 5 6 7 8))

(define (nth-column n m)
  (if (null? m) '()
      (cons (get-element n (car m)) (nth-column n (cdr m)))))
;(nth-column 2 m)

;returns the elemnts from the main diagonal
(define (main-diagonal m)
(define (main-diagonal-iter n m)
  (if (or (> n (length (car m)))(null? m) ) '()
         (cons (get-element n (car m)) (main-diagonal-iter (+ 1 n) (cdr m)))))
  (main-diagonal-iter 1 m))

;(main-diagonal m)

;transposes a matrix
(define (transpose m)
( if(null? (cdr m)) (car m)
    (map cons (car m) (transpose (cdr m)))))

(transpose m)
