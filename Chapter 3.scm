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

; NB: using 2.0 instead of 2 in (estimate-integral) is primordial. If you pass two
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
;-- 3.10
;-- 3.11
;-- 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define x (list 'a 'b))
;
; x-> [*|*]->[*|/]
;      |      |
;      v      v
;     [a]    [b]
;
(define y (list 'c 'd))
;
; y-> [*|*]->[*|/]
;      |      |
;      v      v
;     [c]    [d]
;
(define z (append x y))
;
; z-> [*|*]->[*|*]->[*|*]->[*|/]
;      |      |      |      |
;      v      v      v      v
;     [a]    [b]    [c]    [d]
;
(cdr x)
; (b)
(define w (append! x y))
; The trick is that now, w == x == (a b c d)
; Hence:
(cdr x)
; (b c d)

;-- 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))
;         ___________
;       /             \
;      v               |
; z-> [*|*]->[*|*]->[*|*]
;      |      |      |
;      v      v      v
;     [a]    [b]    [c]
;
; Trying to compute (last-pair z) would result in an infinite loop - the stop condition cannot occur.

;-- 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
; Mystery looks like a list-reversal function at first glance. Is it? Let's unroll it! We'll try with '(1 2 3)
;
; (mystery '(1 2 3))
;
; => (loop '(1 2 3) '())
; ===> temp is '(2 3)
; ===> x becomes '(1)
;
; => (loop '(2 3) '(1))
; ===> temp is '(3)
; ===> x becomes '(2 1)
;
; => (loop '(3) '(2 1))
; ===> temp is nil
; ===> x becomes '(3 2 1)
;
; => (loop '() '(3 2 1))
; ===> exits because x is nil ans returns y, i.e. '(3 2 1)
;
; It is a list-reversal function! Well done, self!

(define v (list 'a 'b 'c 'd))
;
; v-> [*|*]->[*|*]->[*|*]->[*|/]
;      |      |      |      |
;      v      v      v      v
;     [a]    [b]    [c]    [d]
;
(define w (mystery v))
; w-> [*|*]->[*|*]->[*|*]->[*|/]
;      |      |      |      |
;      v      v      v      v
;     [d]    [c]    [b]    [a]
;                           ^
;                          /
;          v -------------
v
; (a)
w
; (d c b a)

;-- 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
; Simpler case:
(define three-pairs (list 'a 'b 'c))
(count-pairs three-pairs)
; 3
; Double reference:
(define alist (list 'a))
(define twice-alist (cons alist alist))
(define four-pairs (list twice-alist))
(count-pairs four-pairs)
; 4
; Double reference at another level yields 5:
(define alist (list 'a))
(define alistlist (list alist))
(define five-pairs (cons alistlist alistlist))
(count-pairs five-pairs)
; 5
; And double double reference gives 7:
(define alist (list 'a))
(define twice-alist (cons alist alist))
(define seven-pairs (cons twice-alist twice-alist))
(count-pairs seven-pairs)
; 7

;-- 3.17
(define (count-pairs x)
  (define visited '())
  (define count 0)
  (define (visit pair)
    (if (not (memq pair visited))
      (begin (set! visited (cons pair visited))
             (set! count (+ 1 count)))))
  (define (populate-list x)
    (if (pair? x)
      (begin (populate-list (car x))
             (populate-list (cdr x))
             (visit x))))
  (begin (populate-list x)
         count))
; Tests (as defined in 3.16):
(count-pairs three-pairs)
; 3
(count-pairs four-pairs)
; 3
(count-pairs five-pairs)
; 3
(count-pairs seven-pairs)
; 3

;-- 3.18
(define (cycle? x)
  ; cycle? will walk the list and add each element to a stack until either the current
  ; element is not a pair (i.e. the list is not a cycle) or the current element is in
  ; the stack (i.e. there's a cycle).
  (define (test x stack)
    (if (pair? x)
      (if (memq x stack)
          #t
          (test (cdr x) (cons x stack)))
      #f))
  (test x '()))
; Test:
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))
(cycle? z)
; #t
(cycle? (list 'a 'b 'c))
; #f

;-- 3.19
; "(This requires a very clever idea.)"
; I did not have this very clever idea, but Robert Floyd did: two pointers, walking
; the list at different speed (colloquially called the "tortoise and the hare" algorithm).
; More info: http://en.wikipedia.org/wiki/Cycle_detection
; A solution:
(define (cycle? x)
  (define (cycle-iter tortoise hare)
    (cond ((eq? tortoise hare)
            #t)
          ((or (not (pair? tortoise))
               (not (pair? hare))
               (not (pair? (cdr hare))))
            #f)
          (else (cycle-iter (cdr tortoise) (cddr hare)))))
  (if (pair? x)
    (cycle-iter x (cdr x))
    #f))

; Test:
(cycle? z)
; #t
(cycle? (list 'a 'b 'c))
; #f

;-- 3.20
; Environment diagrams

;-- 3.21
; Given queue functions:
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

; Answer:
; The queue is actually empty. The rear-ptr isn't updated, but it doesn't matter - testing
; for the empty queue only bothers with front-ptr, and as soon as we add a new element to
; the queue, rear-ptr will be updated.
(define (print-queue q)
  (define (print-list l)
    (if (not (null? l))
      (begin (display (car l))
             (newline)
             (print-list (cdr l)))))
  (if (null? (car q))
    (display "The queue is empty.\n")
    (print-list (car q))))

; Test:
(define q (make-queue))
(print-queue q)
; The queue is empty.
(insert-queue! q 'a)
(print-queue q)
; a
(insert-queue! q 'b)
(print-queue q)
; a b
(delete-queue! q)
(print-queue q)
; b
(delete-queue! q)
(print-queue q)
; The queue is empty.

;-- 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (cdr front-ptr)))))
    (define (print-queue)
      (define (print-iter l)
        (if (not (null? l))
          (begin (display (car l))
                 (newline)
                 (print-iter (cdr l)))))
      (if (empty-queue?)
        (display "The queue is empty.\n")
        (print-iter front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            ((eq? m 'print-queue) (print-queue))
            (else (error "Unknown request -- MAKE-QUEUE" m))))
    dispatch))
; And some wrapper functions to behave exactly as before:
(define (front-ptr q)
  (q 'front-ptr))
(define (rear-ptr q)
  (q 'rear-ptr))
(define (front-queue q)
  (q 'front-queue))
(define (insert-queue! q item)
  ((q 'insert-queue!) item))
(define (delete-queue! q)
  (q 'delete-queue!))
(define (print-queue q)
  (q 'print-queue))

; Test (same as the previous question):
(define q (make-queue))
(print-queue q)
; The queue is empty.
(insert-queue! q 'a)
(print-queue q)
; a
(insert-queue! q 'b)
(print-queue q)
; a b
(delete-queue! q)
(print-queue q)
; b
(delete-queue! q)
(print-queue q)
; The queue is empty.

;-- 3.23
(define (make-deque) (cons '() '()))
(define front-ptr car)
(define rear-ptr cdr)
(define set-front-ptr! set-car!)
(define set-rear-ptr! set-cdr!)
(define (empty-deque? deque)
  (or (null? deque) (null? (front-ptr deque))))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (caar deque)))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (cadr deque)))
(define (front-insert-deque! deque item)
  (cond ((empty-deque? deque)
          (let ((new-pair (cons item (cons '() '()))))
            (set-front-ptr! deque new-pair)
            (set-rear-ptr! deque new-pair)
            deque))
        (else
          (let ((new-pair (cons item (cons '() (front-ptr deque)))))
            (set-car! (cdr (front-ptr deque)) new-pair)
            (set-front-ptr! deque new-pair)
            deque))))
(define (rear-insert-deque! deque item)
  (cond ((empty-deque? deque)
          (let ((new-pair (cons item (cons '() '()))))
            (set-front-ptr! deque new-pair)
            (set-rear-ptr! deque new-pair)
            deque))
        (else
          (let ((new-pair (cons item (cons (rear-ptr deque) '()))))
            (set-cdr! (cdr (rear-ptr deque)) new-pair)
            (set-rear-ptr! deque new-pair)
            deque))))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
          (error "DELETE! called with an empty deque" deque))
        (else
          (if (eq? (front-ptr deque) (rear-ptr deque))
            (begin (set-front-ptr! deque '())
                   (set-rear-ptr! deque '())
                   deque)
            (begin (set-front-ptr! deque (cddr (front-ptr deque)))
                   (set-car! (cdr (front-ptr deque)) '())
                   deque)))))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
          (error "DELETE! called with an empty deque" deque))
        (else
          (if (eq? (front-ptr deque) (rear-ptr deque))
            (begin (set-front-ptr! deque '())
                   (set-rear-ptr! deque '())
                   deque)
            (begin (set-rear-ptr! deque (cadr (rear-ptr deque)))
                   (set-cdr! (cdr (rear-ptr deque)) '())
                   deque)))))
(define (print-deque d)
  (define (print-iter l)
    (if (not (null? l))
      (begin (display (car l))
             (newline)
             (print-iter (cddr l)))))
  (if (empty-deque? d)
    (display "The deque is empty.\n")
    (print-iter (car d))))

; Test:
(define d (make-deque))
(print-deque d)
; The deque is empty.
(front-insert-deque! d 'b)
(print-deque d)
; b
(front-insert-deque! d 'a)
(print-deque d)
; a b
(rear-insert-deque! d 'c)
(print-deque d)
; a b c
(front-delete-deque! d)
(print-deque d)
; b c
(rear-delete-deque! d)
(print-deque d)
; b
(rear-delete-deque! d)
(print-deque d)
; The deque is empty.
(rear-insert-deque! d 'z)
(print-deque d)
; z

;-- 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    ; We only need to redefine assoc to account for the same-key? test
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    ; -- snip -- ;
    dispatch))


