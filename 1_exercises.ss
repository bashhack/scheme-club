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
(inc (+ 3 5)
(inc (inc (+ 2 5))
(inc (inc (inc (+ 1 5)))
(inc (inc (inc (inc 5)))
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

;; 1.11

;; A function `f` is defined by the rule that:
;;
;;          n if n < 3
;; f(n) = {
;;          f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3
;;
;; Write a procedure that computes `f` by means of a recursive process.
;; Write a procedure that computes `f` by means of an iterative process.

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

;; 1.13

;; Prove that Fib(n) is the closest integer to phi^n / sqrt(5) where phi = (1 + sqrt(5))/2
;; Hint: Let psi = (1 - sqrt(5))/2
;; Use induction and the definition of the Fibonacci numbers to prove that
;; Fib(n) = (phi^n - psi^n)/sqrt(5)
