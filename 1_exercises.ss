;; 1.1

;; Below is a sequence of expressions. What is the result printed by the
;; interpreter in response to each expression? Assume that the sequence
;; is to be evaluated in the order in which it is presented.

;; NOTE: Have written answers to all evaluations as comments...

10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 4) (- 4 6)) ;; 6
(define a 3) ;; #<void>
(define b (+ a 1)) ;; #<void>
(+ a b (* a b)) ;; 19
(= a b) ;; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ;; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;; 16
(+ 2 (if (> b a ) b a)) ;; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;; 16

;; 1.2

;; Translate the following expression into prefix form:

;;      5 + 4 + (2 - (3 - (6 + 4/5)))
;;      -----------------------------
;;            3 (6 - 2)(2 - 7)

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))


;; 1.3

;; Define a procedure that takes three numbers as arguments and returns the
;; sum of the squares of the two larger numbers.

(define insert-g
  (lambda (x xs comparison)
    (cond
     ((null? xs) (cons x '()))
     ((comparison x (car xs)) (cons x xs))
     (else (cons (car xs) (insert-g x (cdr xs) comparison))))))

(define insertion-sort-g
  (lambda (xs comparison)
    (cond ((null? xs) '())
          (else (insert-g (car xs) (insertion-sort-g (cdr xs) comparison) comparison)))))

(define insertion-sort-asc
  (lambda (xs)
    (insertion-sort-g xs <)))

(define insertion-sort-desc
  (lambda (xs)
    (insertion-sort-g xs >)))

(define take
  (lambda (n xs)
    (cond
     ((or (null? xs) (zero? n)) '())
     (else (cons (car xs) (take (- n 1) (cdr xs)))))))

(define square
  (lambda (x)
    (* x x)))

;; Generic
(define sum-n-largest-squares-from-list
  (lambda (n xs)
    (apply + (map square (take n (reverse (insertion-sort-asc xs)))))))

(sum-n-largest-squares-from-list 2 '(1 5 281 492 3 2 1 0))

;; Specific
(define sum-largest-two-squares-from-three-args
  (lambda (n m z)
    (cond
     ((= n (min n m z)) (+ (square m) (square z)))
     (else (sum-largest-two-squares-from-args m z n)))))

(sum-largest-two-squares-from-three-args 3 2 1)

;; 1.4

;; Observe that our model of evaluation allows for combinations whose operators
;; are compound expressions. Use this observation to describe the behavior of
;; the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; Of note here is that our compound expression `(if (> b 0) + -)` must be
;; evaluated (or reduced) first by the interpreter before the rest of the
;; surrounding expression. The compound expression, then, is acting here
;; as a sub-expression, and would evalute to either:
;; (+ a b) or (- a b), depending on whether b > 0 or b < 0, respectively

;; 1.5

;; Ben Bitdiddle has invented a test to determine whether the interpreter he is
;; faced with is using applicative-order evaluation or normal-order evaluation.
;; He defines the following two procedures:

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;; Then he evaluates the expression

(test 0 (p))

;; What behavior will Ben observe with an interpreter that uses applicative-order
;; evaluation? What behavior will he observe with an interpreter that uses
;; normal-order evaluation? Explain your answer.

;; With an interpreter that uses applicative-order evaluation, we'll
;; end up in an infinite loop as the expression `(p)` within `(test 0 (p))` is
;; applied/resolved before `test` can see that indeed `x = 0' is `#t`.
;; In normal-order evaluation, or what we might call 'lazy evaluation',
;; we never arrive at the non-terminating application of `(p)' because the
;; interpreter would 'see' `x = 0` as `#t` first and return a value of `0`.

;; 1.6

;; Alyssa P. Hacker doesn't see why `if` needs to be provided as a special form.
;; "Why can't I just define it as an ordinary procedure in terms of `cond`?"
;; she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and
;; she defines a new version of `if`:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

;; Delighted, Alyssa uses `new-if` to rewrite the square-root program:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

;; What happens when Alyssa attempts to use this to compute square roots?
;; Explain.

;; When Alyssa defined the custom function 'new-if', what was not accounted
;; for was effect of 'applicative-order evaluation'.
;; Because the conveniences afforded by the special-form 'if' are removed,
;; we are dealing with a function subject to an evaluation strategy that
;; would see the innermost function application resolved first - but, since
;; this is a recursive call `(sqrt-iter (improve guess x) x)`, the interpreter
;; would find itself in an infinite loop.
;; When using the special form of `if`, the `else` branch is simply never run,
;; because 'if the "predicate" evaluates to a true value, the interpreter
;; then evaluates the "consequent" and returns its value'.

;; 1.7

;; The `good-enough?` test used in computing square roots will not be very
;; effective for finding the square roots of very small numbers.
;; Also, in real computers, arithmetic operations are almost always
;; performed with limited precision. This makes our test inadequate for
;; very large numbers. Explain these statements, with examples showing
;; how the test fails for small and large numbers. An alternative
;; strategy for implementing `good-enough` is to watch how guess
;; changes from one iteration to the next and to stop when the change
;; is a very small fraction of the guess. Design a square-root procedure
;; that uses this kind of end test. Does this work better for small or
;; large numbers?

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))

(define our-sqrt
  (lambda (x)
    (sqrt-iter 1.0 x)))

(our-sqrt 0.004)
(sqrt 0.004)
(< 0.001 (* (our-sqrt 0.001) (our-sqrt 0.001)))
(our-sqrt 99999999999999999)
(not (= (sqrt 9) (our-sqrt 9)))

;; Here, we see two issues:
;; the most straight-forward is in the case of the smaller values,
;; where we see that our epsilon value is simply too large to account
;; for the much smaller difference between 'guess' and 'x' in 'good-enough?'

;; For example, this would return #t where it would not make sense to do so:
(good-enough? 0.0001 0.000000001)
(* 0.000000001 0.000000001) ;; .0000000000000000001

;; Here, we can wee that the difference between the our calculation and
;; expected calculation was less than our epsilon:
(< 0.001 (- (our-sqrt 0.000000001) (sqrt 0.000000001)))

;; In the case of the larger values, we're encountering an expected issue
;; with floating-point precision that goes well beyond Scheme. As the
;; value of 'x' increases, our guesses become so large that we become
;; unable to represent that value within the bound of our epsilon tolerance.
;; On my 64-bit CPU, that value is near 99999999999999999, at which point
;; no value is returned and the process winds up in infinite recusion.

(define sqrt-iter-based-on-previous-guess
  (lambda (previous-guess guess x)
    (if (good-enough?-based-on-previous-guess previous-guess guess)
        guess
        (sqrt-iter-based-on-previous-guess guess (improve guess x) x))))

(define good-enough?-based-on-previous-guess
  (lambda (previous-guess guess)
    (< (abs (- previous-guess guess)) 0.001)))

(define our-sqrt-based-on-previous-guess
  (lambda (x)
    (sqrt-iter-based-on-previous-guess 0 1.0 x)))

(< (our-sqrt-based-on-previous-guess .000004) (our-sqrt .000004))
(< (our-sqrt-based-on-previous-guess 9) (our-sqrt 9))
(< (our-sqrt-based-on-previous-guess 100000000000) (our-sqrt 100000000000))

;; I modified the original `good-enough?` to take into account
;; the previous-guess in the form of `good-enough?-based-on-previous-guess`.
;; Likewise, I then updated the signature of the main
;; `sqrt-iter-based-on-previous-guess` procedure, such that I could
;; provide a starting (or previous-guess) value when initializing
;; `our-sqrt-based-on-previous-guess`.
;; In general, there is an improvement on precision for smaller values,
;; while larger values retain their inaccuracies.

;; 1.8

;; Newton's method for cube roots is based on the fact that if `y` is
;; an approximation to the cube root of `x`, then a better approximation
;; is given by the value:

;;         x/y^2 + 2y
;;       -------------
;;             3

;; Use this formula to implement a cube-root procedure analogous to the
;; square-root procedure.

;; Specific
(define good-enough?
  (lambda (previous-guess guess)
    (< (abs (- previous-guess guess)) 0.001)))

(define improve-via-newtons-cube-root-method
  (lambda (guess x)
    (/ (+ (/ x (square guess)) (* guess 2)) 3)))

(define cube-root-iter
  (lambda (previous-guess guess x)
    (if (good-enough? previous-guess guess)
        guess
        (cube-root-iter guess (improve-via-newtons-cube-root-method guess x) x))))

(define cube-root
  (lambda (x)
    (cube-root-iter 0 1.0 x)))

(cube-root 27)

;; General solution...
(define nth-root
  (lambda (degree x tolerance)

    (define square
      (lambda (x)
        (* x x)))

    (define power
      (lambda (base x)
        (cond ((zero? x) 1)
              ((even? x) (square (power base (/ x 2))))
              (else (* base (power base (- x 1)))))))

    (define good-enough?
      (lambda (next guess)
        (< (abs (- next guess)) tolerance)))

    (define improve
      (lambda (guess)
        (/ (+ (* (- degree 1) guess) (/ x (power guess (- degree 1)))) degree)))

    (define *nth-root
      (lambda (guess)
        (let ((next (improve guess)))
          (if (good-enough? next guess)
              guess
              (*nth-root next)))))

    (*nth-root 1.0)))

(nth-root 2 4 0.001)
(nth-root 3 27 0.001)
(nth-root 3 -1 0.001)

;; 1.9

;; Each of the following two procedures defines a method for adding two
;; positive integers in terms of the procedures `inc`, which increments
;; its argument by 1, and `dec`, which decrements its argument by 1.

(define +
  (lambda (a b)
    (if (= a 0)
        b
        (inc (+ (dec a) b)))))

(define +
  (lambda (a b)
    (if (= a 0)
        b
        (+ (dec a) (inc b)))))

;; Using the substitution model, illustrate the process generated by each
;; procedure by evaluating (+ 4 5). Are these processes iterative or recursive?

;; For the first procedure:
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
;; For the second procedure:
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
9

;; While both procedures are defined recursively, the first procedure is
;; linearly-recursive and the latter is linearly-iterative.

;; 1.10

;; The following procedure computes a mathematical function called
;; Ackermann's function.

(define A
  (lambda (x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1) (A x (- y 1)))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define f
  (lambda (n)
    (A 0 n)))

(define g
  (lambda (n)
    (A 1 n)))

(define h
  (lambda (n)
    (A 2 n)))

(define k
  (lambda (n)
    (* 5 n n)))

;; Give concise mathematical definitions for the functions computed by
;; the procedures `f`, `g`, and `h` for positive integer values of `n`.
;; For example, `(k n)` computes 5n^2.

;; God, I love this function - introduced to it first in `The Little Schemer',
;; I found wrapping my head around both the simple beauty of the function
;; and its use case in demonstrating tetration.
;;
;; Visually representing the function examples above, it would look like:
(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2))) (A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
(A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
(A 0 (A 0 (A 0 (A 0 4096))))
(A 0 (A 0 (A 0 8192)))
(A 0 (A 0 16384))
(A 0 32768)
65536

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 (A 0 (A 1 1)))
(A 2 (A 0 2))
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
(A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
(A 0 (A 0 (A 0 (A 0 4096))))
(A 0 (A 0 (A 0 8192)))
(A 0 (A 0 16384))
(A 0 32768)
65536

;; In the functions provided above as `f`, `g`, `h`, I've quickly tested against
;; reasonable inputs to determine the following definitions:

(define f
  (lambda (n)
    (A 0 n)))

(f 0) ;; 0
(f 1) ;; 2
(f 2) ;; 4
(f 3) ;; 6
(f 4) ;; 8

;; The definition of function `f` would be 2n
;; (* 2 n)

(define g
  (lambda (n)
    (A 1 n)))

(g 0) ;; 0
(g 1) ;; 2
(g 2) ;; 4
(g 3) ;; 8
(g 4) ;; 16

;; The definition of function `g` would be 2^n
;; (expt n 2)

(define h
  (lambda (n)
    (A 2 n)))

(h 0) ;; 0
(h 1) ;; 2
(h 2) ;; 4
(h 3) ;; 16
(h 4) ;; 65536

;; The definition of function `h` would be 2^^n (or 2↑↑n, using Knuth's notation)
;; (expt 2 (expt 2 (expt 2 2))) ;; 65536

;; 1.2.1 Exercise: Count Change

;; My approach here was inspired by a conversation with a co-worker
;; who was exploring different ways of solving for the `count-change`
;; example in the text.
;;
;; I took an approach here that empahsized the lessons learned in working
;; with recursion through 'The Little Schemer'

(define ways-to-count-change
  (lambda (amount denominations)
    (cond ((or (< amount 0) (null? denominations)) 0)
          ((= amount 0) 1)
          (else (+ (ways-to-count-change amount (cdr denominations))
                   (ways-to-count-change (- amount (car denominations)) denominations))))))

(ways-to-count-change 100 '(50 25 10 5 1))
(ways-to-count-change 11 '(50 25 10 5 1))

;; 1.11

;; A function `f` is defined by the rule that:
;;
;;          n if n < 3
;; f(n) = {
;;          f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3
;;
;; Write a procedure that computes `f` by means of a recursive process.
;; Write a procedure that computes `f` by means of an iterative process.

;; Using a recursive process, it's quite simple to mirror the equation itself:
(define f-recur
  (lambda (n)
    (if (< n 3)
        n
        (+ (f-recur (- n 1))
           (* 2 (f-recur (- n 2)))
           (* 3 (f-recur (- n 3)))))))

;; For the iterative process, I found it less intuitive (to say the least!):

;; First, I want to see the pattern to visualize things...
;; given 0 => 0
;; given 1 => 1
;; given 2 => 2
;;             a       b      c
;; given 3 -> f(2) + 2f(1) + 3f(0) -> 2 + 2 + 0 -> 4
;;          a = previous f(n)  b = a   c = b
;; given 4 -> f(3) + 2f(2) + 3f(1) -> 4 + 2(2) + 3 -> 11
;; given 5 -> f(4) + 2f(3) + 3f(2) -> 11 + 2(4) + 3(2) -> 11 + 8 + 6 -> 25

;; Our iterative solution should start where n >= 3,
;; knowing our 'base cases' is known where n < 3 already,
;; similar to how we started solving our iterative Fibonacci example,
;; where we knew that our 'base cases' of 0 and 1:
;;
;; (define fib-iter
;;   (lambda (a b count)
;;     (if (= count 0)
;;         b
;;         (fib-iter (+ a b) a (- count 1)))))

;; (define fib
;;   (lambda (n)
;;     (fib-iter 1 0 n)))
;;
;; Where we used (fib-iter 1 0 n), our case might look like (f-iter 2 1 0 n)

(define f
  (lambda (n)
    (if (< n 3)
        n
        (f-iter 2 1 0 n))))

;; Here, 2 1 0 are going to stand in for the values of f(n - 1), f(n - 2), and f(n - 3)
;; where n = 3.
;; Next, we need to decrement count, include a check against count < 3, and call
;; the f-iter method on itself...the basic structure should look something like this:

;; (define f-iter
;;   (lambda (n-minus-1 n-minus-2 n-minus-3 count)
;;     (if (< count 3)
;;         ...return something
;;         (f-iter ... (- count 1)))))

(define f-iter
  (lambda (a b c count)
    (if (< count 3)
        a
        ;;      f-iter a is the previous f(n - 1)
        (f-iter (+ a (* 2 b) (* 3 c))
                ;; f-iter b is old a, f-iter c is old b a b
                (- count 1)))))

;; With an input of 4...

;; (f 4)
;; (< 4 3) ;; #f
;; (f-iter 2 1 0 4)
;; (< 4 3) ;; #f
;; (f-iter (+ 2 (* 2 1) (* 3 0)) 2 1 3)
;;   (f-iter (+ 2 2 0) 2 1 3)
;;     (f-iter 4 2 1 3)
;; (f-iter (+ 4 (* 2 2) (* 3 1)) 4 2 2)
;;   (f-iter (+ 4 4 3) 4 2 2)
;;     (f-iter 11 4 2 2)
;;       11

;; 1.12

;; The following pattern of numbers is called 'Pascal's triangle'
;;
;;                         1
;;                       1   1
;;                     1   2   1
;;                   1   3   3   1
;;                 1   4   6   4   1
;;                       . . .
;; The numbers at the edge of the triangle are all 1, and each number
;; inside the triangle is the sum of the two numbers above it.
;; Write a procedure that computes elements of Pascal's triangle by means
;; of a recursive process.

;; First, we need to map this to some sort of ordered data...rows and columns
;; seems like a good method...so we might say:

;; I think it's easier to visualize things a bit like this...
;;
;;                         1   (row 0, col 0)
;;                         1   1 ((row 1, col 0), (row 1, col 1))
;;                         1   2   1 ((row 2, col 0), (row 2, col 1), (row 2, col 2))
;;                         1   3   3   1 ((row 3, col 0), (row 3, col 1), (row 3, col 2), (row 3, col 3))
;;                         1   4   6   4   1
;;                       . . .
;;
;; I visualize it as if I was seeing this:
;;
;;                         1
;;                         1 + 1 <-| <- +    (0)
;;                                 |          |
;;                         1    +  2 <-|<- +  1 <- + (0)
;;                                     |      |       |
;;                                     v      v       v
;;                         1    +    <-3<- +  3   +   1 <- + (0)
;;                                     |      |       |       |
;;                                     v      v       v
;;                         1     +     4   +  6   +   4    +  1

(define pascal
  (lambda (row col)
    (cond ((or (= col 0) (= col row)) 1) ;; top of the triangle (column 0) or sides
          ;; sum of the previous row + same column AND previous row + previous column
          (else (+ (pascal (- row 1) col)
                   (pascal (- row 1) (- col 1)))))))

(pascal 0 0) ;; 1
(pascal 3 2) ;; 3
(pascal 4 2) ;; 6

;; 1.13

;; Prove that Fib(n) is the closest integer to phi^n / sqrt(5) where phi = (1 + sqrt(5))/2
;; Hint: Let psi = (1 - sqrt(5))/2
;; Use induction and the definition of the Fibonacci numbers to prove that
;; Fib(n) = (phi^n - psi^n)/sqrt(5)

;; 1.14

;; Draw the tree illustrating the process generated by the `count-change`
;; procedure of Section 1.2.2 in making change for 11 cents.

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;;  11 cents, 5 kinds of coins
                                 (count-change 11)
                     ;; 5 is partially applied to cc via `count-change`
                                     (cc 11 5)
                                    /        \____________________
                             (cc 11 4)         +                  (cc -39 5) ;; (cc (- 11 50) 5)
                           /         \_______________________          \
                     (cc 11 3)          +                    (cc -14 4) 0
                     /       \____________________                   \
             (cc 11 2)                +          (cc 1 3)             0
            /      \                                \_   \___________
     (cc 11 1) + (cc 6 2)                             (cc 1 2) +     (cc -9 3)
    /        \     ((cc 1 2) reduces to tree at right) /___  /_____          \
(cc 11 0) + (cc 10 1)  (cc 6 1) + (cc 1 2) => (1)       (cc 1 1) + (cc -4 2)  0
    |        /     \         / \                           \___ \_________ \__________
    0  (cc 10 0) + (cc 9 1) ((cc 6 1) reduces to tree at left) (cc 1 0) + (cc 0 1)    0
         |          /    \         \__________                     /            \
         0    (cc 9 0) + (cc 8 1)      0   + (1)                  0             (1)
               |         /     \
               0    (cc 8 0) + (cc 7 1)
                    |           /    \
                    0      (cc 7 0) + (cc 6 1)
                           |           /     \
                           0      (cc 6 0) + (cc 5 1)
                                  |          /      \
                                  0     (cc 5 0) + (cc 4 1)
                                        |          /      \
                                        0    (cc 4 0)  +  (cc 3 1)
                                             |            /       \
                                             0       (cc 3 0)  + (cc 2 1)
                                                     |           /      \
                                                     0      (cc 2 0)  + (cc 1 1)
                                                            |           /       \
                                                            0        (cc 1 0) + (cc 0 1)
                                                                        |          |
                                                                        0         (1)

;; What are the orders of growth of the space and number of steps used by this process as
;; the amount to be changed increases?

;; In analyzing the orders of growth in terms of both space and number, let's look
;; at the behavior of the procedure with a few inputs. This could help
;; us better see and describe patterns of growth.

;; With a small input like (cc 1 1), the following tree is generated:

                                     (cc 1 1)
                                    /        \
                            (cc 1 0)     +    (cc 0 1)
                           /                          \
                          0                            1

;; In terms of space complexity - we a tree depth of 1.
;; The time complexity, expressed in terms of the steps,
;; looks like 2 steps, so perhaps 2n makes sense here?
;; Or, perhaps 2n + 1 to account for the initial call to cc.

;; Now, let's look at what occurs when our amount, n, increases to 2:

                                     (cc 2 1)
                                    /        \
                            (cc 2 0)     +    (cc 1 1)
                           /                 /        \
                          0          (cc 1 0)     +    (cc 0 1)
                                    /                          \
                                   0                            1

;; Space complexity is O(n) still, as the max depth where n = 2 is 2.

;; But let's also look at what happens when the coins increase:

                                    (cc 1 2)
                                   /        \
                            (cc 1 1)    +    (cc -4 2)
                           /        \                 \
                   (cc 1 0)     +    (cc 0 1)          0
                  /                          \
                 0                            1

;; And now, where n = 2, and k = 2:

                                   (cc 2 2)
                                  /        \
                           (cc 2 1)    +    (cc -3 2)
                          /       \                  \
                  (cc 2 0)    +    (cc 1 1)           0
                 /                /        \
                0         (cc 1 0)     +    (cc 0 1)
                         /                          \
                        0                            1

;; In all examples above, we see the space required was never more than
;; the maximum depth of the tree where the depth was the change amount (n).
;; This space complexity is best described as linear, and as we would state in
;; Big O notation: O(n)

;; Let's go back to our comparison of (cc 1 1) and (cc 1 2)

                                  (cc 1 1)
                                 /        \
                         (cc 1 0)     +    (cc 0 1)
                        /                          \
                       0                            1

                                  (cc 1 2)
                                 /        \
                         (cc 1 1)    +    (cc -4 2)
                        /        \                 \
                (cc 1 0)     +    (cc 0 1)          0
               /                          \
              0                            1

;; Now, increasing once more, we would see:

                                  (cc 1 3)
                                 /        \
                         (cc 1 2)     +    (cc -9 3)
                        /        \                  \
                (cc 1 1)     +    (cc -4 2)          0
               /        \                  \
       (cc 1 0)     +    (cc 0 1)           0
      /                          \
     0                            1

;; Where at (cc n 1) we we had 2n + 1 steps or O(n), at (cc n 2) we had
;; trees generated for (cc n 1) and (cc (- n 5) 2). In practice, we
;; should be able to express that sum in simpler notation: O(n).
;; We have recursion in action - if we increase the coins to 3
;; we'll see another (cc n 1) branch of O(n) added to our tree.
;; So, if 2 denominations, we had O(n^2) and 3 has O(n^3) -
;; we could surmise that at k=5 denominations we have O(n^5) number of steps,
;; having concluded that O(n^k) where a = amount and k = denominations.

;; 1.15

;; The sine of an angle (specified in radians) can be computed by making
;; use of the approximation sin x = x if x is sufficiently small, and the
;; trigonometric identity
;;
;;             sin x = 3 sin x / 3 - 4 sin^3 x / 3
;; to reduce the size of the argument of sin. (For purposes of this exercise
;; an angle is considered 'sufficiently small' if its magnitude is not greater
;; than 0.1 radians.) These ideas are incorporated in the following procedures:

(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; a. How many times is the procedure p applied when (sine 12.15) is evaluated?
;; b. What is the order of growth in space and number of steps (as a function of a)
;;    used by the process generated by the sine procedure when (sine a) is evaluated?

(sine 12.15)
(p (sine (/ 12.15 3.0))) ;; (p (sine 4.05))
(p (p (sine (/ 4.05 3.0)))) ;; (p (p (sine 1.3499)))
(p (p (p (sine (/ 1.3499 3.0))))) ;; (p (p (p (sine .4499))))
(p (p (p (p (sine (/ .4499 3.0)))))) ;; (p (p (p (p (sine .1499)))))
(p (p (p (p (p (sine (/ .1499 3.0))))))) ;; (p (p (p (p (p (sine .0499))))))
(p (p (p (p (p .0499)))))

;; There are five applications of the procedure `p`

;; For each tripling of the input `a`, we gain a procedure call to `p` -
;; in other words, the applications of `p` can be expressed in terms of (log3 a).
;; Because Big O analysis should not generally account for constants,
;; and because changing bases of logarithms is the same as multiplying by a constant,
;; we can say that the step and space complexity should be (log2 a) or, with
;; more conventional Big O variable naming, O(log n).

;; Change of base: ((log a) b) = (/ ((log x) b) ((log x) a))

;; NOTE: Came across this helpful little resource for 'visualizing' orders of growth:
;;       https://stackoverflow.com/questions/2307283/what-does-olog-n-mean-exactly

;; 1.16

;; Design a procedure that evolves an iterative exponentiation process
;; that uses successive squaring and uses a logarithmic number of steps,
;; as does `fast-expt`. (HINT: Using the observation that (b^(n/2)^2) = ((b^2)^(n/2)),
;; keep, along with the exponent n and the base b, an additional state variable
;; a, and define the state transformation in such a way that the product ab^n is
;; unchanged from state to state. At the beginning of the process a is taken to be 1,
;; and the answer is given by the value of a at the end of the process. In general,
;; the technique of defining an invariant quantity remains unchanged from state to state
;; is a powerful way to think about the design of iterative algorithms.)

(define square
  (lambda (x)
    (* x x)))

(define fast-expt
  (lambda (b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1)))))))

;; Original:
(define expt-iter
  (lambda (b n product)
    (if (= n 0)
        product
        (expt-iter b (- n 1) (* b product)))))

(define expt-iter-logarithmic
  (lambda (b n product)
    (cond ((= n 0) product)
          ;; b^(n/2)^2 == (b^2)^(n/2)
          ((even? n) (expt-iter-logarithmic (square b) (/ n 2) product))
          (else (expt-iter-logarithmic b (- n 1) (* b product))))))

(define expt
  (lambda (b n)
    (expt-iter b n 1)))

(define expt-logarithmic
  (lambda (b n)
    (expt-iter-logarithmic b n 1)))

(fast-expt 5 3)
(expt 5 3)
(expt-logarithmic 5 3)

;; 1.17

;; The exponentiation algorithms in this section are based on performing
;; exponentiation by means of repeated multiplication. In a similar way,
;; one can perform integer multiplication by means of repeated addition.
;; The following multiplication procedure (in which it is assumed that our
;; language can only add, not multiply) is analogous to the `expt` procedure:
;;
;; (define (* a b)
;;   (if (= b 0)
;;       a
;;       (+ a (* a (- b 1)))))
;; This algorithm takes a number of steps that is linear in b.
;; Now suppose we include, together with addition, operations `double`,
;; which doubles an integer, and `halve`, which divides an (even) integer
;; by 2. Using these, design a multiplication procedure analogous
;; to `fast-expt` that uses a logarithmic number of steps.

(define double
  (lambda (x)
    (+ x x)))

(define halve
  (lambda (x)
    (/ x 2)))

(define fast-multiply
  (lambda (a b)
    (cond ((= b 0) 0)
          ((= b 1) a)
          ((even? b) (double (fast-multiply a (halve b))))
          (else (+ a (fast-multiply a (- b 1)))))))

(fast-mult 2 7)
(= (fast-mult 32 74) (* 32 74))

;; 1.18

;; Using the results of 1.16 and 1.17, devise a procedure that generates
;; an iterative process for multiplying two integers in terms of adding,
;; doubling, and halving and uses a logarithmic number of steps.

(define fast-mult-iter
  (lambda (a b product)
    (cond ((= b 0) product)
          ((even? b) (fast-mult-iter (double a) (halve b) product))
          (else (fast-mult-iter a (- b 1) (+ a product))))))

;; without (double a)
;; 3 2 0
;; (fast-mult-iter 3 1 0)
;; (fast-mult-iter 3 0 3)
;; 3

;; with...
;; 3 2 0
;; (fast-mult-iter 6 1 0)
;; (fast-mult-iter 6 0 6)
;; 6

;; 4 3 0
;; (fast-mult-iter 4 2 4)
;; (fast-mult-iter 8 1 4)
;; (fast-mult-iter 8 0 12)
;; 12

(fast-mult-iter 3 1 0)
(fast-mult-iter 2 1 0)
(fast-mult-iter 3 2 0)

;; 1.19

;; There is a clever algorithm for computing the Fibonacci numbers in a
;; logarithmic number of steps. Recall the transformation of the state
;; variables `a` and `b` in `fib-iter` process of
;; Section 1.2.2: a <- a + b and b <- a.
;; Call this transformation `T`, and observe that applying `T` over and
;; over again `n` times, starting with 1 and 0, produces the pair
;; Fib(n + 1) and Fib(n). In other words, the Fibonacci numbers are
;; produced by applying `T^n`, the `nth` power of the transformation `T`,
;; starting with the pair (1, 0). Now consider `T` to be the special case of
;; `p = 0` and `q = 1` in a family of transformations `Tpq`, where `Tpq`
;; transforms the pair (a, b) according to a < bq + aq + ap and b <- bp + aq.
;; Show that if we apply such a transformation `Tpq` twice, the effect
;; is the same as using a single transformation `Tp'q'` of the same form,
;; and compute `p'` and `q'` in terms of `p` and `q`. This gives us an explicit
;; way to square these transformations, and thus we can compute `T^n` using
;; successive squaring, as in the `fast-expt` procedure. Put this all together
;; to complete the following procedure, which runs in a logarithmic number
;; of steps:

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (??) ; compute p'
                   (??) ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; 1.20

;; The process that a procedure generates is of course dependent on the rules
;; used by the interpreter. As an example, consider the iterative `gcd`
;; procedure given above. Suppose we were to interpret this procedure using
;; normal-order evaluation, as discussed in Section 1.1.5. Using the substitution
;; method (for normal order), illustrate the process generated in evaluating
;; `(gcd 206 40)` and indicate the `remainder` operations that are actually
;; performed. How many `remainder` operations are actually performed in
;; normal-order evaluation of `(gcd 206 40)`? In the applicative-order evaluation?

;; 1.21

;; Use the `smallest-divisor` procedure to find the smallest divisor of each
;; of the following numbers; 199, 1999, 19999

;; 1.22

;; Most Lisp implementations include a primitive called `runtime` that returns
;; an integer that specifies the amount of time the system has been running (
;; measured, for example, in microseconds). The following `timed-prime-test`
;; procedure, when called with an integer `n`, prints `n` and checks to see if
;; `n` is prime. If `n` is prime, the procedure prints three asterisks followed
;; by the amount of time used in performing the test.

(define timed-prime-test
  (lambda (n)
    (newline)
    (display n)
    (start-prime-test n (runtime))))

(define start-prime-test
  (lambda (n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time)))))

(define report-prime
  (lambda (elapsed-time)
    (display " *** ")
    (display elapsed-time)))

;; Using this procedure, write a procedure `search-for-primes` that checks the
;; primality of consecutive odd integers in a specified range. Use your procedure
;; to find the three smallest primes larger than 1000; larger than 10,000; larger
;; than 100,000; larger than 1,000,000. Note the time needed to test each prime.
;; Since the testing algorithm has order of growth of \theta(sqrt(n)), you should
;; expect that testing for primes around 10,000 should take about sqrt(10) times
;; as long as testing for primes around 1000. Do your timing data bear this out?
;; How well do the data for 100,000 and 1,000,000 support the \theta(sqrt(n))
;; prediction? Is your result compatible with the notion that programs on your
;; machine run in time proportional to the number of steps required for the
;; computation?

;; 1.23

;; The `smallest-divisor` procedure shown at the start of this section does
;; lots of needless testing: After it checks to see if the number is divisible
;; by 2 there is no point in checking to see if it is divisible by any larger
;; even numbers. This suggests that the values used for `test-divisor` should
;; not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, ....
;; To implement this change, define a procedure `next` that returns 3 if its
;; input is equal to 2 and otherwise returns its input plus 2. Modify the
;; `smallest-divisor` procedure to use `(next test-divisor)` instead of
;; `(+ test-divisor 1)`. With `timed-prime-test` incorporating this modified
;; version of `smallest-divisor`, run the test for each of the 12 primes found
;; in Exercise 1.22. Since this modification halves the number of test steps,
;; you should expect it to run about twice as fast. Is this expectation confirmed?
;; If not, what is the observed ratio of the speeds of the two algorithms,
;; and how do you explain the facat that it is different from 2?

;; 1.24

;; Modify the `timed-prime-test` procedure of Exercise 1.22 to use
;; `fast-prime?` (the Fermat method), and test each of the 12 primes you found
;; in that exercise. Since the Fermat test has \theta(log n) growth, how would
;; you expect the time to test primes near 1,000,000 to compare with the
;; time needed to test primes near 1000? Do your data bear this out?
;; Can you explain any discrepancy you find?

;; 1.25

;; Alyssa P. Hacker complains that we went to a lot of extra work in writing `expmod`.
;; After all, she says, since we already know how to compute exponentials, we
;; could have simply written:

(define expmod
  (lambda (base exp m)
    (remainder (fast-expt base exp) m)))

;; Is she correct? Would this procedure serve as well for our fast prime tester?
;; Explain.

;; 1.26

;; Louis Reasoner is having great difficulty doing Exercise 1.24.
;; His `fast-prime?` test seems to run more slowly than his `prime?` test.
;; Louis calls his friend Eva Lu Ator over to help. When they examine
;; Louis's code, they find that he has rewritten the `expmod` procedure to use
;; an explicit multiplication, rather than calling `square`:

(define expmod
  (lambda (base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (* (expmod base (/ exp 2) m)
                         (expmod base (/ exp 2) m))
                      m))
          (else
           (remainder (* base
                         (expmod base (- exp 1) m))
                      m)))))

;; "I don't see what difference that could make," says Louis. "I do," says Eva.
;; "By writing the procedure like that, you have transformed the \theta(log n)
;; process into a \theta(n) process." Explain.

;; 1.27

;; Demonstrate that the Carmichael numbers listed in Footnote 1.47 really
;; do fool the Fermat test. That is, write a procedure that takes an integer
;; `n` and tests whether `a^n` is congruent to `a modulo n` for every `a < n`,
;; and try your procedure on the given Carmichael numbers.

;; 1.28

;; One variant of the Fermat test that cannot be fooled is called the
;; Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an alternate
;; form of Fermat's Little Theorem, which states that if `n` is a prime number
;; and `a` is any positive integer less than `n`, then `a` raised to the
;; `(n - 1)`-st power is congruent to 1 modulo `n`. To test the primality
;; of a number `n` by the Miller-Rabin test, we pick a random number `a < n`
;; and raise `a` to the `(n - 1)`-st power modulo `n` using the `expmod` procedure.
;; However, whenever we perform the squaring step in `expmod`, we check to see
;; if we have discovered a "nontrivial square root of 1 module `n`," that is,
;; a number not equal to 1 or n - 1 whose square is equal to 1 modulo n.
;; It is possible to prove that if such a nontrivial square root of 1 exists,
;; then n is not prime. It is also possible to prove that if n is an odd
;; number that is not prime, then, for at least half the numbers `a < n`,
;; computing `a^(n - 1)` in this way will reveal a nontrivial square root
;; of 1 modulo n. (This is why the Miller-Rabin test cannot be fooled.)
;; Modify the `expmod` procedure to signal if it discovers a nontrivial
;; square root of 1, and use this to implement the Miller-Rabin test with
;; a procedure analogous to `fermat-test`. Check your procedure by testing
;; various known primes and non-primes. Hint: One convenient way to make
;; `expmod` signal is to have it return 0.
