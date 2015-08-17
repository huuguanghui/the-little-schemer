#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      (else (or (eq? a (car l)) (member? a (cdr l)))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define plus
  (lambda (x y)
    (cond
      ((zero? x) y)
      ((zero? y) x)
      (else (plus (add1 x) (sub1 y))))))

(define minus
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (minus (sub1 x) (sub1 y))))))
