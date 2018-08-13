#lang racket
;the first 10 even sqrt's from a stream
;(define (from n)
 ; (cons n (lambda()(from (+ n 1))))) ; λ отлага безкрайното оценяване. Може и с delay <=> lambda()

; дефиниране на специална форма в  Scheme
(define-syntax delay
  (syntax-rules() ((delay x) (lambda() x))))

(define (force delayed-object)
  (delayed-object)) ; delayed-object е в скобки демек прилагаме функцията все едно и ни се дава оценката на процедурата

(define-syntax cons-stream
  (syntax-rules() ((cons-stream head tail) (cons head (delay tail)))))

(define stream-null? null?)
(define the-empty-stream '())

(define (from n)
  (cons-stream n (from(+ n 1))))

(define stream-car car)

(define (stream-cdr s) ((cdr s))) ;апликация на процедура (оценяваме процедурата)

(define naturals (from 0))

;(define (stream-take s n) ;construct a stream with n elements from the stream s
;  (if(= n 0) the-empty-stream
;     (cons-stream (stream-car s) (stream-take(stream-cdr s) (- n 1)))))

;(define (stream->list s)
;  (if(stream-null? s) '()
;     (cons (stream-car s) (stream->list(stream-cdr s)))))

;(define (stream-map s f)
;  (if (stream-null? s) '()
;      (cons-stream (f (stream-car s)) (stream-map (stream-cdr s) f))))
(define (square x) (* x x ))

;(define (stream-filter s p)
;  (if(stream-null? s) the-empty-stream
;     (if( p (stream-car s)) (cons-stream (stream-car s) (stream-filter(stream-cdr s) p))
;        (stream-filter (stream-cdr s) p))))

;(stream->list(stream-take naturals 10))
;(stream->list(stream-take (stream-map naturals square) 10))
(define (stream-take s n)
  (if(= 0 n) the-empty-stream
     (cons-stream (stream-car s) (stream-take (stream-cdr s) (- n 1)))))
;generate all numbers from the interval [a,b]
(define(stream-range a b)
  (stream-take(from a) (+ 1 (- b a))))

;return the element on position n from the stream s
(define (stream-ref s n)
  (define (iterator i s)
    (if(= i n) (stream-car s)
       (iterator (+ 1 i) (stream-cdr s))))
  (iterator 0 s))

;define stream-map
(define (stream-map f s)
  (if (stream-null? s) the-empty-stream
    (cons-stream (f (stream-car s)) (stream-map f (stream-cdr s)))))

(define s (stream-range 1 7))
;(stream->list(stream-map (lambda(x) (+ 1 x)) s))
(define (stream-filter p s)
  (if(stream-null? s) the-empty-stream
     (if(p (stream-car s)) (cons-stream(stream-car s) (stream-filter p (stream-cdr s)))
        (stream-filter p (stream-cdr s)))))

;
(define (stream-fold s acc f)
  (if(stream-null? s) acc
     (f(stream-car s) (stream-fold (stream-cdr s) acc f))))
;(stream-fold (stream-range 1 6) 1 *)
(define (stream->list s)
  (if(stream-null? s) '()
     (cons(stream-car s) (stream->list (stream-cdr s)))))
;
(define (stream-drop s n)
  (if (= n 0) s
      (stream-drop (stream-cdr s) (- n 1))))
;(stream->list(stream-drop (stream-take (stream-filter even? (stream-map square naturals))10) 5))
(define (prime? x)
  (define (iterator p)
    (if(> p (quotient x 2)) #t
      (if(= 0 (remainder x p)) #f
         (iterator (+ 1 p)))))
  (iterator 2))
(define 42nd-prime-number
  (stream-ref (stream-filter prime? naturals) 42))