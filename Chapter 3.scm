;-- 3.1
(define (make-accumulator acc)
  (lambda (x)
    (set! acc (+ x acc))
    acc))

;-- 3.2
(define (make-monitored function)
  (define times-called 0)
  (define (mf message)
    (cond ((eq? message 'how-many-calls?) times-called)
	  ((eq? message 'reset-count) (set! times-called 0))
	  (else (set! times-called (+ times-called 1))
		(function message))))
  mf)

;-- 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m given-password)
    (if (eq? password given-password)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    (else (error "Unknown request -- MAKE-ACCOUNT"
			 m)))
      (lambda (x) "Incorrect password")))
  dispatch)

;-- 3.4
(define (make-account balance password)
  (define wrong-tries 0)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m given-password)
    (if (eq? password given-password)
      (begin (set! wrong-tries 0)
	     (cond ((eq? m 'withdraw) withdraw)
		   ((eq? m 'deposit) deposit)
		   (else (error "Unknown request -- MAKE-ACCOUNT"
				m))))
      (begin (set! wrong-tries (+ wrong-tries 1))
	     (if (> wrong-tries 7)
	       call-the-cops
	       (lambda (x) "Incorrect password")))))
  dispatch)

; call-the-cops example:
(define call-the-cops
  (lambda (x) "HANDS UP!"))

; Test:
(define pw (make-account 100 'la))
((pw 'withdraw 'lag) 10)
; "Incorrect password"
((pw 'withdraw 'lag) 10)
; "Incorrect password"
((pw 'withdraw 'la) 10)
; 90
((pw 'withdraw 'lag) 10)
; "Incorrect password"
((pw 'withdraw 'lag) 10)
; "Incorrect password"
((pw 'withdraw 'lag) 10)
; "Incorrect password"
((pw 'withdraw 'lag) 10)
; "Incorrect password"
((pw 'withdraw 'lag) 10)
; "Incorrect password"
((pw 'withdraw 'lag) 10)
; "Incorrect password"
((pw 'withdraw 'lag) 10)
; "Incorrect password"
((pw 'withdraw 'lag) 10)
"HANDS UP!"

;-- 3.5
; Given:
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	    (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; Solution:
(define (P x y)
  (< (+ (expt (- x 5) 2)
	(expt (- y 7) 2))
     (expt 3 2)))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (monte-carlo trials experiment))

; Test:
(estimate-integral P 0 12 3 17 100)

; We can estimate pi with the fact that a circle area is (pi * r²)
; Hence pi ≅ (Monte Carlo results * rectangle area) / r²
(define pi-approx
  (/ (* (estimate-integral P 2.0 8.0 4.0 10.0 10000) 36)
     9.0))
pi-approx
; 3.1336

; This function has to be tested under MIT Scheme, neither gambit-scheme or SISC
; implements (random) - actually (random) is not part of R5RS nor SRFI.

; NB: using 2.0 instead of 2 in estimate-integral is primordial. If you pass two
; integers to (random-in-range low high), it will return another integer strictly
; inferior to your 'high' value — and this completely screws the Monte-Carlo method
; (it then estimates pi to ~3.00).

;-- 3.6
(define rand
  (let ((x random-init))
    (define (dispatch message)
      (cond ((eq? message 'generate)
	     (begin (set! x (rand-update x))
		    x))
	    ((eq? message 'reset)
	     (lambda (new-value) (set! x new-value)))))
    dispatch))

; Test:
(define random-init 0)
(define (rand-update x) (+ x 1)) ; Our not-very-evolved PNRG
(rand 'generate)
; 1
(rand 'generate)
; 2
((rand 'reset) 0)
; 0
(rand 'generate)
; 1

; It's interesting to notice that the lambda returned by a call to (rand 'reset) still has the
; closure we created as lexical scope:
x
; Error: undefined variable 'x'.

;-- 3.7
(define (make-account balance password)
  (define password-list (list password))
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m given-password)
    (if (memq given-password password-list)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    ((eq? m 'joint) (lambda (new-pass) (set! password-list (cons new-pass password-list))
			      dispatch))
	    (else (error "Unknown request -- MAKE-ACCOUNT"
			 m)))
      (lambda (x) "Incorrect password")))
  dispatch)
(define (make-joint account password new-account-password)
  ((account 'joint password) new-account-password))

; Test:
(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'withdraw 'open-sesame) 10)
; 90
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'withdraw 'rosebud) 15)
; 75
((peter-acc 'withdraw 'open-sesame) 10)
; 65

; This solution works, but is not perfect. For example, now we can access peter's account with paul's password:
((peter-acc 'withdraw 'rosebud) 10)
; 55
; One would expect that the password would be different for each account.

;-- 3.8
(define f
  (let ((init (- 1)))
    (lambda (x) (if (= init (- 1))
		  (set! init x)
		  0))))

;-- 3.9


