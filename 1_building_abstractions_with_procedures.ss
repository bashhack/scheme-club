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

(define (square x) (* x x))

(square 2)

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

;; 1.1.6

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs-with-if x)
  (if (< x 0)
      (- x)
      x))

(abs -5)
(abs-with-if -5)
