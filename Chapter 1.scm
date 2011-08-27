;-- 1.2
(/ (+ 5
      4
      (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;-- 1.3
(define (sumSqu x y) (+ (* x x) (* y y)))
(define (sumTwoBiggers a b c)
  (cond ((> a b) (cond ((> b c) (sumSqu a b))
		       (else (sumSqu a c))))
	(else (cond ((> a c) (sumSqu b a))
		    (else (sumSqu b c))))))


;-- 1.9 fibo
(define (fibo n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fibo (- n 1)) (fibo (- n 2))))))

(define (fib n)
  (fib-iter 0 1 n))
(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter b (+ a b) (- count 1))))

;-- 1.11
(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(define (f n)
  (iter_f 0 1 2 3 n))

(define (iter_f a b c n)
  (if (= n 0)
    a
    (iter_f (+ a (* 2 b) (* 3 c)) a b (- n 1))))

;-- 1.12
(define (pascal line col)
  (cond ((= col 0) 1)
	((< line 1) 0)
	(else (+ (pascal (- line 1) col)
		 (pascal (- line 1) (- col 1))))))

;-- 1.17
(define (fast-mult a b)
  (cond ((= b 1) a)
	((even? b) (fast-mult (double a) (halve b)))
	(else (+ a (fast-mult a (- b 1))))))

;-- 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

;-- 1.31
; a.
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)
(define (addone x) (+ x 1))
(define (factorial x)
  (product identity 1 addone x))

(define (littlepi n)
  (if (even? n)
    (/ (+ 2 n) (+ 1 n))
    (/ (+ 1 n) (+ 2 n))))
(define (pi_approx precision)
  (* 4 (product littlepi 1 addone precision)))


; b.
(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

;-- 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

;-- 1.33
(define (filtered-accumulate combiner null-value filter term a next b)
  (if (> a b)
    null-value
    (combiner (if (filter a)
		(term a)
		null-value)
	      (filtered-accumulate combiner
						  null-value
						  filter
						  term
						  (next a)
						  next
						  b))))

; a.
(define (square x) (* x x))
(define (next x) (+ 1 x))
(define (smsqa a b)
  (filtered-accumulate + 0 prime? square a next b))

; b.
(define (exb n)
  (define (filter x)
    (= (gcd x n) 1))
  (filter-accumulate * 1 filter identity 1 next n))

;-- 1.34
(define (f g)
  (g 2))
;(f f) -> (f 2)
;(2 2)
;error

;-- 1.35
; Golden Number: x when x² = x + 1
;		   i.e. x = 1 + 1/x
(define phi
  (fixed-point (lambda (x) (average x (+ 1 (/ 1 x))))
	       1.0))

; Helpers:
(define (average x y) (/ (+ x y) 2))
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(try next))))
  (try first-guess))

;-- 1.36
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
	next
	(try next))))
  (try first-guess))

(define xxth
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
	       2.0))
(define xxth_av
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
	       2.0))

; The average version uses far less iterations.

;-- 1.37
; Computes the k-term finite continued fraction N / D + N'
; Recursive process :
(define (cont-frac n d k)
  (if (= k 0)
    0
    (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

; Iterative process :
(define (cont-frac n d k)
  (define (loop result term)
    (if (= term 0)
      result
      (loop (/ (n term) (+ (d term) result))
	    (- term 1))))
  (loop 0 k))

;-- 1.38
(define (n x)
  (cond ((= x 1) 1)
	((= x 2) 2)
	(else (if (= 2 (remainder x 3))
		(* 2 (+ 1 (/ (- x 2) 3)))
		1))))

(define e
  (+ 2.0 (cont-frac (lambda (x) 1)
		    (lambda (x) (cond ((= x 0) 1)
				      ((= x 1) 1)
				      ((= x 2) 2)
				      (else (if (= 0 (remainder (- x 2) 3))
					      (* 2 (+ 1 (/ (- x 2) 3)))
					      1))))
		    50)))

;-- 1.39
(define (tan-cf x k)
  (cont-frac  (lambda (z) (if (= 1 z)
			    x
			    (- (* x x))))
	      (lambda (z) (- (* 2 z) 1))
	      k))

;-- 1.40

;-- 1.41
(define (double fun)
  (lambda (x) (fun (fun x))))

(((double (double double)) inc) 5) ; gives 21

;-- 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;-- 1.43
(define (square x) (* x x ))

(define (repeated fun n)
  (if (= n 0)
    (lambda (x) x)
    (compose fun (repeated fun (- n 1)))))

; Or without using compose:
(define (repeated fun n)
  (if (= n 0)
    (lambda (x) x)
    (lambda (x) (fun ((repeated fun (- n 1)) x)))))

;-- 1.44
(define (smooth f)
  (define dx 0.001)
  (lambda (x) (/ (+ (f (- x dx))
		    (f x)
		    (f (+ x dx)))
		 3)))

(define (nfoldsmooth f n)
  ((repeated smooth n) f))

;-- 1.45

;-- 1.46
(define (average x y) (/ (+ x y) 2))

(define (iterative-improve good-enough? improve)
  (lambda (guess) (if (good-enough? guess)
		    guess
		    ((iterative-improve good-enough? improve)(improve guess)))))

(define (sqrt-ii n)
  ((iterative-improve (lambda (guess)
			(< (abs (- (square guess) n)) 0.001))
		      (lambda (guess)
			(average guess (/ n guess))))
   1.0))

(define (fixed-point-ii f)
  ((iterative-improve (lambda (guess)
			(< (abs (- guess (f guess))) 0.00001))
		      (lambda (guess)
			(f guess)))
   1.0))

