;; 1.1.1

;; Expressions (s-expressions)
(+ 42 3.14)

(- 10)

(* 7 52.143)

(/ 4.75 7.54)

(+ 1 1 2 3 5)

(+ (/ (* 2 4) 2) (- 100 10) 6)

;; 1.1.2

;; Naming
(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))

(* pi (* radius radius))

circumference

;; Expressions (s-expressions) Notation
(* (+ 2 (* 4 6)) (+ 3 5 7))

;;               390_______
;;              /  |       \__
;;             *   26         15
;;               / | \     / | \  \
;;             +   2  24  +  3  5  7
;;                   / | \
;;                  *  4  6

;; 1.1.4

(define square
  (lambda (x)
    (* x x)))

(square 2)

(define sum-of-squares
  (lambda (x y)
    (+ (square x) (square y))))

(sum-of-squares 3 4)

;; 1.1.6

(define abs
  (lambda (x)
    (cond ((< x 0) (- x))
          (else x))))

(define abs-with-if
  (lambda (x)
    (if (< x 0)
        (- x)
        x)))

(abs -5)
(abs-with-if -5)

;; 1.1.7

(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define sqrt-iter-infinite
  (lambda (guess x)
    (new-if (good-enough? guess x)
            guess
            (sqrt-iter-infinite (improve guess x) x))))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

(define sqrt
  (lambda (x)
    (sqrt-iter 1.0 x)))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

;; 1.1.8

(define double
  (lambda (x)
    (+ x x)))

(define square
  (lambda (x)
    (* x x)))

(define square
  ;; Euler's number raised to power `(double (log x))`
  (lambda (x)
    (exp (double (log x)))))

;; Using block structure

(define sqrt
  (lambda (x)

    (define average
      (lambda (x y)
        (/ (+ x y) 2)))

    (define good-enough?
      (lambda (guess x)
        (< (abs (- (square guess) x)) 0.001)))

    (define improve
      (lambda (guess x)
        (average guess (/ x guess))))

    (define sqrt-iter
      (lambda (guess x)
        (if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x) x))))

    (sqrt-iter 1.0 x)))

(sqrt 4)

;; Using block structure and lexical scoping

(define sqrt
  (lambda (x)

    (define average
      (lambda (x y)
        (/ (+ x y) 2)))

    (define good-enough?
      (lambda (guess)
        (< (abs (- (square guess) x)) 0.001)))

    (define improve
      (lambda (guess)
        (average guess (/ x guess))))

    (define sqrt-iter
      (lambda (guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess)))))

    (sqrt-iter 1.0)))

(sqrt 4)
