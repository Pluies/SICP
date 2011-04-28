;-- 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ (if (or (and (< n 0) (< d 0)) (and (> n 0) (< d 0)))
	       (- n)
	       n)
	     g)
	  (/ (if (or (and (< n 0) (< d 0)) (and (> n 0) (< d 0)))
	       (- d)
	       d)
	     g))))

; Clever version, from http://community.schemewiki.org/?sicp-ex-2.1
(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g) (/ d g))))

;-- 2.2
(define (make-segment x y) (cons x y))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (average x y) (/ (+ x y) 2))
(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s))
		       (x-point (end-segment s)))
	      (average (y-point (start-segment s))
		       (y-point (end-segment s)))))

; Given:
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;-- 2.3
(define (rectangle a b c d) ; Where a, b, c and d are (x,y) pairs representing points coordinates as : a   b
  (cons (cons a b) (cons c d)))                                                                      ; d   c
(define (point_a r) (car (car r)))
(define (point_b r) (cdr (car r)))
(define (point_c r) (car (cdr r)))
(define (point_d r) (cdr (cdr r)))
(define (distance a b)
  (sqrt (+ (square (- (x-point b) (x-point a)))
	   (square (- (y-point b) (y-point a))))))
(define (width r)
  (distance (point_a r) (point_b r)))
(define (height r)
  (distance (point_b r) (point_c r)))

(define (perimeter r)
  (+ (* 2 (height r))
     (* 2 (width r))))
(define (area r)
  (* (height r) (width r)))

(define r_test (rectangle (cons 1 3) (cons 4 3) (cons 4 (- 1)) (cons 1 (- 1))))
(perimeter r_test)
(area r_test)

;-- 2.4
(define (cons24 x y)
  (lambda (m) (m x y)))
(define (car24 z)
  (z (lambda (p q) p)))
(define (cdr24 z)
  (z (lambda (p q) q)))

;-- 2.5

;-- 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; we'll find (one) by substitution
; one => (add-1 zero)
;        (lambda (f) (lambda (x) (f ((zero f) x))))
;        (lambda (f) (lambda (x) (f x)))

;-- 2.7 - 2.16

;-- 2.17
(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))

; Test: 
(last-pair (list 1 2 3 4)) 

;-- 2.18
(define (reverse alist)
  (if (null? (cdr alist))
    alist
    (append (reverse (cdr alist))
	    (list (car alist)))))

; Test:
(reverse (list 1 2 3 4)) 

;-- 2.19
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)((or (< amount 0) (no-more? coin-values)) 0)
	(else
	  (+ (cc amount
		 (except-first-denomination coin-values))
	     (cc (- amount
		    (first-denomination coin-values))
		 coin-values)))))

;-- 2.20
(define (same-parity . numberlist)
  (define (filter li condition)
    (if (null? li)
      li
      (if (condition (car li))
	(append (list (car li)) (filter (cdr li) condition))
	(filter (cdr li) condition))))
  (if (even? (car numberlist))
    (filter numberlist even?)
    (filter numberlist (lambda (x) (not (even? x))))))

;-- 2.21
(define (square-list items)
  (if (null? items)
    nil
    (cons (* (car items) (car items))
	  (square-list (cdr items)))))
(define (square-list items)
  (map (lambda (x) (* x x)) items))

;-- 2.22
; The first version won't work because the items are popped from the
; first list and then pushed in the second � resulting in a reverse
; order.
; The second version won't work because "cons"-ing a list to an int
; results in a list-in-a-list ((a b) c); contrary to "cons"-ing an
; int to a list which gives a list (a b c).

;-- 2.23
(define (for-each fun lis)
  (cond ((null? lis) #t)
	(else (fun (car lis))
	      (for-each fun (cdr lis)))))

;-- 2.24
(list 1 (list 2 (list 3 4)))
; Interpreter: (1 (2 (3 4)))
; Boxes:  [.|.]->[.|x]
;          v      v
;         [1]    [.|.]-> [.|x]
;                 v       v
;                [2]     [.|.]->[.|x]
;                         v      v
;                        [3]    [4]
;
; Tree: (1 (2 (3 4)))
;          /  \
;         1  (2 (3 4))
;              /  \
;             2   (3 4)
;                  / \
;                 3   4

;-- 2.25
; Give combinations of cars and cdrs that will pick 7 from each of the following lists:
; (1 3 (5 7) 9)
(define A (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr a)))))
; ((7))
(define B (list (list 7)))
(car (car B))
; (1 (2 (3 (4 (5 (6 7))))))
(define C (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr C))))))))))))

;-- 2.26
; Suppose we define x and y to be two lists:
(define x (list 1 2 3))
(define y (list 4 5 6))
; What result is printed by the interpreter in response to evaluating each of the following expressions:
(append x y)
; (1 2 3 4 5 6)
(cons x y)
; ((1 2 3) 4 5 6)
(list x y)
; ((1 2 3)(4 5 6))

;-- 2.27
(define (deep-reverse li)
  (cond ((null? li) li)
	(else (append (deep-reverse (cdr li))
		      (list (if (pair? (car li))
			      (deep-reverse (car li))
			      (car li)))))))
; Test:
(define x (list (list 1 2) (list 3 (list 4 5))))
(deep-reverse x)

;-- 2.28
(define (fringe node)
  (if (pair? node)
    (append (fringe (car node))
	    (fringe (cdr node)))
    (if (null? node)
      '()
      (list node))))
; Test:
(define x (list (list 1 2) (list 3 4)))
(fringe x)
; (1 2 3 4)
(fringe (list x x))
; (1 2 3 4 1 2 3 4)

;-- 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

; a.
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))

; b.
(define (branch-weight branch)
  (let ((sub (branch-structure branch)))
    (if (structure-is-mobile? sub)
      (total-weight sub)
      sub)))
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch submobile))))

; c.
(define (balanced? mobile)
  (= (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

; d.


; Tests:
(left-branch (make-mobile 2 3)) 
(right-branch (make-mobile 2 3)) 
(branch-length (make-branch 4 5)) 
(branch-structure (make-branch 4 5)) 

;-- 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
; With map:
(define (square-tree-map tree)
  (map (lambda (x)
         (cond ((null? x) nil)
               ((not (pair? x)) (* x x))
               (else (square-tree-map x))))
        tree))
; Tests:
(define my-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree my-tree)
(square-tree-map my-tree)

;-- 2.31
(define (tree-map function tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (function tree))
		(else (cons (tree-map function (car tree))
		            (tree-map function (cdr tree))))))

;-- 2.32
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest
              (map (lambda (x) (cons (car s) x))
                   rest)))))
; Test:
(subsets (list 1 2 3))
; NB: doesn't seem to work well on sisc-scheme.

;-- 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))
(define (append seq1 seq2)
  (accumulate cons 
              seq2
              seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))

;-- 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))
; Test:
(horner-eval 2 (list 1 3 0 5 0 1))
; => 79

;-- 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (node)
                     (if (pair? node) (count-leaves node) 1))
                   t)))
; Test:
(count-leaves (list (list 1 2 3) (list (list 1 2) (list 2)) 2 3 (list 1 2)))
; => 10

;-- 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
; Test:
(define listoflist (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 listoflist)
; => (22 26 30)

;-- 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v))
       m))
(define (transpose mat)
  (accumulate-n cons
                '()
                mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x))
         m)))

; Tests:
(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define identity-vector (list 1 1 1 1))
(define double-vector (list 2 2 2 2))
(matrix-*-vector matrix identity-vector)
; => (10 21 30)
(matrix-*-vector matrix double-vector)
; => (20 42 60)
(transpose matrix)
; => ((1 4 6) (2 5 7) (3 6 8) (4 6 9))
(matrix-*-matrix matrix matrix)
; => ((27 33 39 43) (60 75 90 100) (82 103 124 138))

;-- 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; What are the values of:
(fold-right / 1 (list 1 2 3))
; 3/2
(fold-left / 1 (list 1 2 3))
; 1/6
(fold-right list nil (list 1 2 3))
; (3 (2 (1 ())))
(fold-left list nil (list 1 2 3))
; (((() 1)  2)  3)

; Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence. 
; Let's take a (list 1 2 3) and fold it:
; Fold-right => (op 3 (op 2 (op 1 nil)))  Ex. (3 * ( 2 * (1)))
; Fold-left => (op (op (op nil 1) 2) 3)   Ex. (((1)*2)*3)
; To me it looks like commutativity. However, others point at associativity instead. To investigate.
; Commutativity: https://secure.wikimedia.org/wikipedia/en/wiki/Commutativity
; Associativity: https://secure.wikimedia.org/wikipedia/en/wiki/Associativity

;-- 2.39
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))
(define (reverse sequence)
  (fold-right (lambda (item rest) (append rest (list item)))
              nil
              sequence))
(define (reverse sequence)
  (fold-left (lambda (item rest) (cons rest item))
             nil
             sequence))

;-- 2.40
; Env:
(define (enumerate-interval a b)
  (if (> a b)
    nil
    (append (list a) (enumerate-interval (+ a 1) b))))
(define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence)))))
(define nil '())
(define (prime? x) 
   (define (test divisor) 
     (cond ((> (* divisor divisor) x) #t) 
           ((= 0 (remainder x divisor)) #f) 
           (else (test (+ divisor 1))))) 
   (test 2))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (unique-pairs n)
  (flatmap (lambda (i) 
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;-- 2.41
; Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than or equal to a given integer n that sum to a given integer s. 
(define (ordered-triples n s)
  (filter (lambda (triple) (= (fold-left + 0 triple) s))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 j)))
                              (enumerate-interval 1 i)))
                   (enumerate-interval 1 n))))

;-- 2.42
; Env:
(define nil '())
(define (enumerate-interval a b)
  (if (> a b)
    nil
    (append (list a) (enumerate-interval (+ a 1) b))))
(define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; Skeleton of the function to implement:
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

; Helper functions:
(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
  (append (list (cons k new-row)) rest-of-queens))
(define (same-line queen-a queen-b) ; Coordinates are (x,y) pairs. If the y is the same for both queens, they are on the same line.
  (= (cdr queen-a) (cdr queen-b)))
(define (same-diagonal queen-a queen-b) ; Two queens are on the same diagonal if there horizontal distance is equal to their vertical distance
  (= (- (car queen-a) (car queen-b))
     (abs (- (cdr queen-a) (cdr queen-b)))))
(define (safe? k positions)
  (define (safe-iter queen-pos other-queens)
    (if (null? other-queens)
        #t
        (let ((qtt (car other-queens))) ;qtt = queen to test
            (cond ((same-line queen-pos qtt) #f) ; Same line
                  ((same-diagonal queen-pos qtt) #f) ; Diagonal
                  (else (safe-iter queen-pos (cdr other-queens)))))))
  (safe-iter (car positions) (cdr positions)))


;-- 2.43
; Way longer, Louis is not skilled.

;-- 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;-- 2.45
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;Right-split and up-split can be expressed as instances of a general splitting operation. Define a procedure split with the property that evaluating
;(define right-split (split beside below))
;(define up-split (split below beside))
;produces procedures right-split and up-split with the same behaviors as the ones already defined. 

(define (split first-transform second-transform)
  (define (new-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (new-split painter (- n 1))))
          (first-transform painter (second-transform smaller smaller)))))
  (lambda (painter n) (new-split painter n)))

; A cleverer version that avoids having to name the lambda in order to recurse, from the scheme wiki:
(define (split first-transform second-transform)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split first-transform second-transform) painter (- n 1))))
          (first-transform painter (second-transform smaller smaller))))))

;-- 2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s vect)
  (make-vect (* s (xcor-vect vect))
             (* s (ycor-vect vect))))

;-- 2.45
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
; Accessors:
(define (origin frame)
  (car frame))
(define (edge1 frame)
  (car (cdr frame)))
(define (edge2 frame)
  (car (cdr (cdr frame))))


(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
; Accessors:
(define (origin frame)
  (car frame))
(define (edge1 frame)
  (car (cdr frame)))
(define (edge2 frame)
  (cdr (cdr frame)))

; The next exercises aren't that interesting.

;-- 2.53
(list 'a 'b 'c)
; (a b c)
(list (list 'george))
; ((george))
(cdr '((x1 x2) (y1 y2)))
; ((y1 y2))
(cadr '((x1 x2) (y1 y2)))
; (y1 y2)
(pair? (car '(a short list)))
; false
(memq 'red '((red shoes) (blue socks)))
; false
(memq 'red '(red shoes blue socks))
; (red shoes blue socks)

;-- 2.54
(define (equal? a b)
  (if (and (pair? a) (pair? b))     ; two pairs : we test deeper
    (and (equal? (car a) (car b))
         (equal? (cdr a) (cdr b)))
    (eq? a b))) ; everything else is handled by eq?

;-- 2.55
(car ''abracadabra)
<=> (car '(quote abracadabra))
<=> (car ('quote 'abracadabra))
<=> 'quote
; i.e. the symbol 'quote

;-- 2.56
; Functions given:
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))     ; Naive implementation; is replaced afterwards
(define (make-product m1 m2) (list '* m1 m2)) ; Same
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; The question itself:
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '** )))
(define base cadr) ; No need to copy the argument
(define exponent caddr)
(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (if (number? (exponent exp))
                                                            (- (exponent exp) 1)
                                                            (list '- (exponent exp) '1))))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

; Test cases:
(deriv '(** 5 6) '2)
; 0
(deriv '(** x 1) 'x)
; 1
(deriv '(** 1 x) 'x)
; 0
(deriv '(** x 5) 'x)
; (* 5 (** x 4))
(deriv '(** x y) 'x)
; (* y (** x (- y 1)))

;-- 2.57
(define (addend s) (cadr s)) ; Does not change
(define (augend s)
  (if (null? (cdddr s))         ; Means the addition is just two terms long
    (caddr s)                   ; The term itself
    (append '(+) (cddr s))))    ; A new addition comprised of the next terms
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((sum? a2) (make-sum a1 (make-sum (addend a2) (augend a2))))
        (else (list '+ a1 a2))))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (null? (cdddr p))
    (caddr p)
    (append '(*) (cddr p))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((product? m2) (make-product m1 (make-product (multiplier m2) (multiplicand m2))))
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; Test:
(deriv '(* x y (+ x 3)) 'x)
;  (+ (* x y) (* y (+ x 3)))

;-- 2.58
; a.
(define (sum? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '+)))
(define addend car)
(define augend caddr)
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (product? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '*)))
(define multiplier car)
(define multiplicand caddr)
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

; Test:
(deriv '(x + (3 * (x + (y + 2)))) 'x)
; 4

; b.
; It looks long and complicated - kept in stock for a long winter night.

;-- 2.59
; Functions given:
(define (element-of-set? x set)
  (cond ((null? set) #f) ;"false" and "true" replaced by their Scheme equivalent for convenience
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Union:
(define (union-set set1 set2)
  (cond ((null? set1) set2) ; We'll add elements of set1 to set2 (given they're not already present in set2)
        ((element-of-set? (car set1) set2)
          (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

; Test:
(define s1 (list 1 2 3 4 5))
(define s2 (list 5 6 7 8))
(intersection-set s1 s2)
; (5)
(union-set s1 s2)
; (1 2 3 4 5 6 7 8)

;-- 2.60
; element-of-set?: doesn't need to change
(define adjoin-set cons) ; No need to check for duplicates
; intersection-set: doesn't change either. It will destroy the duplicates.
(define (union-set set1 set2)
  (append set1 set2))

; Test:
(union-set s1 s3)
; (1 2 3 4 5 5 5 5 5)
(intersection-set s1 s3)
; (5)

; Efficiency: by eliminating the need to walk the list, adjoin is now O(1) instead of O(n).
; union becomes O(n) instead of O(n²)

;-- 2.61
; Functions given:
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

; Answer:
(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

; Test:
(define s4 (list 1 2 3 5 6))
(adjoin-set 4 s4)
; (1 2 3 4 5 6)
(adjoin-set 9 s4)
; (1 2 3 5 6 9)

;-- 2.62
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (union-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (cons x1 (union-set (cdr set1) set2)))
              ((< x2 x1)
               (cons x2 (union-set set1 (cdr set2))))))))
; This implementation is O(n) because each iteration selects an item from either set1 or
; set2 and cons it (an O(1) operation). There are n iterations at most, n being the length
; of set1 + the length of set2, hence an O(n) total complexity.

; Test:
(define s1 (list 1 2 3 4 5))
(define s2 (list 5 6 7 8))
(union-set s1 s2)
; (1 2 3 4 5 6 7 8)

;-- 2.63
; Tree functions:
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set? x set)
  (cond ((null? set) #f) ; Changed "true" to #t and false to #f according to Scheme syntax
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; a.
; Do the two procedures produce the same result for every tree?
; Yes. To test:
(define t1 (list 7 (list 3 (list 1 '() '()) (list 5 '() '())) (list 9 '() (list 11 '() '()))))
(define t2 (list 3 (list 1 '() '()) (list 7 (list 5 '() '()) (list 9 '() (list 11 '() '())))))
(define t3 (list 5 (list 3 (list 1 '() '()) '()) (list 9 (list 7 '() '()) (list 11 '() '()))))
; If not, how do the results differ? What lists do the two procedures produce for the trees in figure 2.16?
; Both tree->list algorithm will print (1 3 5 7 9 11) for the three trees.

; b.
; They're O(n)

;-- 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; a.
; Partial-tree splits the list given in two parts of equal size (modulo 1). Both halves are submitted to partial-tree through recursion.
; The recursive procedures will yield a pair made of the subtree and the list of elements that didn't make it into said subtree. By
; splitting the process between right and left hands, we will ensure that we have a correctly balanced tree. The stop condition for
; partial-tree is asking a tree made of 0 elements, who will yield a pair made of an empty list and the list of items.
; Finally, these halves are assembled by a make-tree between the value at the middle of the list (strictly speaking the leftmost of
; the right half) and the left and right trees computed beforehand.
; This only works with ordered lists containing no duplicates.

; Tree given for (1 3 5 7 9 11):
;              5
;           /     \
;         1         9
;          \       /  \
;            3    7    11

; b.
; Basically O(n).

;-- 2.65
; First try:
; We will reuse filter from a few exercises back
(define (filter predicate sequence)
   (cond ((null? sequence) '())
         ((predicate (car sequence))
          (cons (car sequence)
                (filter predicate (cdr sequence))))
         (else (filter predicate (cdr sequence)))))
(define (intersection-set tree1 tree2)
  (list->tree (filter (lambda (x) (element-of-set? x tree1))
                      (tree->list-1 tree2))))
(define (union-set tree1 tree2)
  (define (union-tree-list t l)
    (if (null? l)
      t
      (union-tree-list (adjoin-set (car l) t) (cdr l))))
  (union-tree-list tree1 (tree->list-1 tree2)))
; Complexity:
; intersection uses an O(log n) filter on an O(n) walk of the elements of a list. It's O(n * log n)
; union uses an O(log n) insertion on an O(n) walk: it's O(n * log n) too.
; These algorithms are poor, in the sense that they don't reuse previous techniques.

; Second try:
(define tree->list tree->list-1)
(define (intersection-set-tree tree1 tree2)
  (list->tree (intersection-set (tree->list tree1)
                                (tree->list tree2))))
(define (union-set-tree tree1 tree2)
  (list->tree (union-set (tree->list tree1)
                         (tree->list tree2))))
; Complexity: all operations are O(n) as shown in 2.62 and performed sequentially; hence the result is O(n) too.

; Test:
(define t1 (list 7 (list 3 (list 1 '() '()) (list 5 '() '())) (list 9 '() (list 11 '() '()))))
(define t2 (list 4 (list 1 '() '()) (list 7 (list 6 '() '()) (list 9 '() (list 11 '() '())))))
; First try:
(intersection-set t1 t2)
; (7 (1 () ()) (9 () (11 () ())))
(union-set t1 t2)
; (7 (3 (1 () ()) (5 (4 () ()) (6 () ()))) (9 () (11 () ())))
; Second try:
(intersection-set-tree t1 t2)
; (7 (1 () ()) (9 () (11 () ())))
(union-set-tree t1 t2)
; (5 (3 (1 () ()) (4 () ())) (7 (6 () ()) (9 () (11 () ()))))
; The difference between the two union-set stems from the fact that the first version
; merely adds elements from tree2 to tree1, while the second version flattens both trees,
; creates a new list and turns that list into a balanced tree.

;-- 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key set-of-records)) true)
        ((< given-key (key set-of-records))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key set-of-records))
         (lookup given-key (right-branch set-of-records)))))
; Basically just a tree lookup.

;-- 2.67
; Huffman trees
; Functions given:
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

; Question:
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)
; (a d a b b c a)

;-- 2.68
; Given:
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; Answer:
(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree) (eq? symbol (symbol-leaf tree))) '())
        ((memq symbol (symbols (left-branch tree)))
          (cons '0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
          (cons '1 (encode-symbol symbol (right-branch tree))))))

; Tests:
(encode-symbol 'a sample-tree)
; (0)
(encode-symbol 'b sample-tree)
; (1 0)
(encode-symbol 'c sample-tree)
; (1 1 1)
(encode-symbol 'd sample-tree)
; (1 1 0)
(encode '(a d a b b c a) sample-tree)
; (0 1 1 0 0 1 0 1 0 1 1 1 0)
(equal? sample-message (encode '(a d a b b c a) sample-tree))
; #t

;-- 2.69
; Given:
(define (generate-huffman-tree-1 pairs)
  (successive-merge-1 (make-leaf-set pairs)))
; Answer
(define (successive-merge-1 leaves)
  (if (= (length leaves) 2)
      (make-code-tree (car leaves) (cadr leaves))
      (successive-merge-1 (cons (make-code-tree (car leaves) (cadr leaves))
                                (cddr leaves)))))
; Test:
(generate-huffman-tree-1 '((A 4) (B 2) (C 1) (D 1)))
;((((leaf d 1) (leaf c 1) (d c) 2) (leaf b 2) (d c b) 4) (leaf a 4) (d c b a) 8)

; NB: this only works because of the special 1 1 2 4 case we're in here. Using this algorithm on the first Huffman tree yields an incorrect result:
(generate-huffman-tree-1 '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
;((((((((leaf h 1) (leaf g 1) (h g) 2) (leaf f 1) (h g f) 3) (leaf e 1) (h g f e) 4) (leaf d 1) (h g f e d) 5) (leaf c 1) (h g f e d c) 6) (leaf b 3) (h g f e d c b) 9) (leaf a 8) (h g f e d c b a) 17)

; The correct way to do it is by using adjoin-set instead of cons:
(define (successive-merge nodes)
  (if (= (length nodes) 1)
    (car nodes)
    (successive-merge2 (adjoin-set (make-code-tree (car nodes) (cadr nodes))
                                   (cddr nodes)))))
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
;((leaf a 8) ((((leaf h 1) (leaf g 1) (h g) 2) ((leaf f 1) (leaf e 1) (f e) 2) (h g f e) 4) (((leaf d 1) (leaf c 1) (d c) 2) (leaf b 3) (d c b) 5) (h g f e d c b) 9) (a h g f e d c b) 17)

;-- 2.70
(define rocktree (generate-huffman-tree '((A 2) (NA 16) (BOOM  1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))
rocktree
; ((leaf na 16) ((leaf yip 9) (((leaf a 2) ((leaf wah 1) (leaf boom 1) (wah boom) 2) (a wah boom) 4) ((leaf sha 3) ((leaf job 2) (leaf get 2) (job get) 4) (sha job get) 7) (a wah boom sha job get) 11) (yip a wah boom sha job get) 20) (na yip a wah boom sha job get) 36)
(define rock-song '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))
(define encoded-rock-song (encode rock-song rocktree))
encoded-rock-song
; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
(length encoded-rock-song)
; 84

; If we were to use a fixed-length encoding on that rock song, we would need 3 bits (8 = 2^3) per symbol, i.e.:
(* 3 (length rock-song))
; 108

; We can see a 22% gain by using the huffman encoding

;-- 2.71
; Example with n=5. We will use letters from the alphabet to represent the symbols.
;                     {a b c d e} 31
;                     /           \
;                {a b c d} 15      e 16
;                 /     \
;           {a b c} 7    d 8
;             /    \
;        {a b} 3    c 4
;         /   \
;      a 1    b 2

; The minimum number of bits to construct a symbol (i.e. the minimum depth to reach a leaf) for such trees is 1, for the symbol of weight 2^n-1.
; The maximum number of bits will be n-1, for the two symbols of least weight.

;-- 2.72
; Encoding the most frequent element as per ex. 2.71 is a mere search into the symbol list, which is accomplished in O(n).
; Encoding the least frequent element involves descending down the tree, with a search in the symbol list each time.
; The complexity is O(n) + O(n-1) + ... + O(1), akin to O(n²).

;-- 2.73
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a.
; What was done above: we switched to data-directed programming. Yay!
; We cannot use this method for number? and variable? because these never have the same operator.
; By comparison, we can implement sum? and product? because the first symbol is always identical (+
; and *).

; b.
(define (install-deriv-package)
  ;; internal procedures
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (compute-sum sum var)
    (make-sum (deriv (addend sum) var)
              (deriv (augend sum) var)))
  (define (compute-product pro var)
    (make-sum
      (make-product (multiplier pro)
                    (deriv (multiplicand pro) var))
      (make-product (deriv (multiplier pro) var)
                    (multiplicand pro))))
  ;; interface to the rest of the system
  (put 'sum? '(deriv) sum?)
  (put 'product? '(deriv) product?)
  (put '+ 'deriv compute-sum)
  (put '* 'deriv compute-product)
  'done)

; NB: This is all highly untested, and probably false.
; Chapter 3 will allow us to implement tables. We'll test it when we get there.

; c.
(define (install-deriv-exp-package)
  ; Internal:
  (define (exponentiation? x)
    (and (pair? x) (eq? (car x) '** )))
  (define base car)
  (define exponent cadr)
  (define (make-exponentiation base exponent)
    (cond ((=number? base 1) 1)
          ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((and (number? base) (number? exponent)) (expt base exponent))
          (else (list '** base exponent))))
  (define (compute-exponentiation exp var)
    (make-product (make-product (exponent exp)
                                (make-exponentiation (base exp)
                                                     (if (number? (exponent exp))
                                                        (- (exponent exp) 1)
                                                        (list '- (exponent exp) '1))))
                  (deriv (base exp) var)))
  ; Interface:
  (put 'exponentiation? '(deriv) exponentiation?)
  (put '** 'deriv compute-exponentiation)
  'done)

; Highly untested too.

; d.
; ?

;-- 2.74
; a.
; Each division will put their method in the table.
(put 'get-record 'sales-division get-salesman)
(put 'get-record 'hr-division human-ressources-get-person)
; etc.
; We can then use this method to retrieve the correct record:
(define (get-record division employee)
  ((get 'get-record division) employee))

; b.
; Same thing:
(put 'get-salary 'sales-division get-salesman-salary)
(put 'get-salary 'hr-division needlessly-convoluted-method-of-retrieving-salary)

(define (get-salary division employee)
  ((get 'get-salary division) (get-record division employee)))

; c.
(define (find-employee-record name division-list)
  (if (null? division-list)
      #f
      (if (memq name (car division-list))
          #t
          (find-employee-record name (cdr division-list)))))

; d.
; Make them use an unique identifier.

;-- 2.75
; We can implement make-from-real-imag in message-passing style as follows:
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)
(define (apply-generic op arg) (arg op))

; Implement the constructor make-from-mag-ang in message-passing style:
(define (make-from-mag-ang r A)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos A)))
          ((eq? op 'imag-part) (* r (sin A)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) A)
          (else
            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;-- 2.76

;-- 2.77

;-- 2.78
(define (attach-tag type-tag contents)
  (if (not (equal? 'scheme-number type-tag))
    (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;-- 2.79
(define install-equ-package
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(rational rational)
    (lambda (x y)
      (= (* (numer x) (denom y))
         (* (numer y) (denom x)))))
  (put 'equ? '(complex complex)
    (lambda (x y)
      (and (= (real-part x) (real-part y))
           (= (imag-part x) (imag-part y)))))
  'done)
(define (equ? x y)
  (apply-generic 'equ? x y))

; Another version, using =zero? as defined in the next exercise:
(define (equ? x y)
  (=zero? (sub x y)))

;-- 2.80
(define install-zero-package
  (put '=zero? '(scheme-number) zero?)
  (put '=zero? '(complex)
    (lambda (x) (and (zero? (imag-part a)) (zero? (real-part a)))))
  (put '=zero? '(rational)
    (lambda (x) (zero? (numer x))))
  'done)
(define (=zero? x)
  (apply-generic '=zero? x))

;-- 2.81
; apply-generic with coercion is defined as:
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; a.
; Given:
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)
(define (exp x y) (apply-generic 'exp x y))
;; following added to Scheme-number package
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y)))) ; using primitive expt
; What happens if we call exp with two complex numbers as arguments?

; Calling exp with two complex will result in an infinite loop. apply-generic will find both
; t1->t2 and t2->t1 procedures (both will be complex->complex) and recurse into apply-generic
; with the same types, complex & complex.

; b.
; Louis Reasoner appears to be wrong. As is, apply-generic will raise an error given
; - no method taking t1 and t2 exists
; - no conversion between t1 and t2 is available.

; c.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags))))
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;-- 2.82
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((working-type (find-working-type op args type-tags)))
            (if working-type
                (apply-generic op (map (lambda (arg)
                                         (if (eq? (type-tag arg) working-type)
                                             arg
                                             ((get-coercion (type-tag arg) working-type) arg)))
                                       args))
                (error "No method for these types" (list op type-tags))))))))
; With find-working-type defined as:
(define (find-working-type op type-tags)
  (define (check-coercions type-tags new-type)
    (if (null? type-tags)
        #t
        (if (or (eq? (car type-tags) new-type)
                (get-coercion (car type-tags) new-type))
            (check-coercions (cdr type-tags) new-type)
            #f)))
  (define (find-iter op type-tags type-tags-to-test)
    (if (null? type-tags-to-test)
        #f
        (let* ((tested-tag (car type-tags-to-test))
               (proc (get op (map (lambda (x) tested-tag)
                                 type-tags))))
          (if proc ; there is a procedure that takes all arguments as tested-tag
              (if (check-coercions type-tags tested-tag) ; and we can convert all arguments to said tested-tag
                  tested-tag
                  (find-iter op type-tags (cdr to-test)))))))
  (find-iter op type-tags type-tags))

; NB: untested.

;-- 2.83
(define install-raise-package
  (put 'raise '(scheme-number)
    (lambda (x) (make-rat x 1)))
  (put 'raise '(rational)
    (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'raise '(real)
    (lambda (x) (make-from-real-imag x 0)))
  'done)
(define (raise x)
  (apply-generic 'raise x))

;-- 2.84
(define typelist '(complex real rational scheme-number))
(define (is-higher type1 type2)
  (let* ((type1-sublist (memq type1 typelist)))
    (if type1-sublist
        (if (memq type2 type1-sublist)
            #t
            #f)
        #f)))

; Adapting apply-generic from 2.82:
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((working-type (find-working-type op args type-tags)))
            (if working-type
                (apply-generic op (map (lambda (arg) (raise-until working-type arg))
                                       args))
                (error "No method for these types" (list op type-tags))))))))
(define (raise-until type element)
  (if (eq? type (type-tage element))
      element
      (raise-until type (raise element))))

;-- 2.85
(define install-project-package
  (put 'project '(rational)
    (lambda (x) (make-scheme-number (round (/ (numer x) (denom x))))))
  (put 'project '(real)
    (lambda (x) (make-rat (round x) 1)))
  (put 'project '(complex)
    (lambda (x) (make-real (real-part x))))
  'done)
(define (project x)
  (apply-generic 'project x))

(define (drop x)
  (if (equ? x (raise (project x)))
      (drop (project x))
      x))

; In order to drop the result of apply-generic:
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (let ((working-type (find-working-type op args type-tags)))
            (if working-type
                (drop (apply-generic op (map (lambda (arg) (raise-until working-type arg))
                                             args)))
                (error "No method for these types" (list op type-tags))))))))

;-- 2.86
; Complicated question. Needs further research.

;-- 2.87
; Given:
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; Question:
(define install-zero-poly-package
  (put '=zero? 'polynomial
    (lambda (poly) (= 0
                      (fold-left + 0 (map coeff (term-list poly)))))
  'done)

;-- 2.88
(define install-sub-package
  (define (negation termlist)
    (map (lambda (term) (make-term (order term) (- (coeff term))))
         termlist))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (negation (term-list p2))))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  'done)

;-- 2.89
(define (install-dense-polynomial-package)
  ;; representation of terms and term lists - the rest stays the same
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (if (= (order term) (length term-list)) ; i.e. lower orders are all occupied
          (cons (coeff term) term-list)
          (adjoin-term term (cons 0 term-list))))) ; We will pad with 0 as needed
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (if (null? (cdr term-list))
      (car term-list)
      (first-term (cdr term-list))))
  (define (empty-termlist? term-list) (null? term-list))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  'done)
; And we redefine add-terms and mul-terms:
(define (zip list1 list2)
  (map list list1 list2))
(define (pad lst target-length padding)
  (if (= (length lst) target-length)
    lst
    (pad (cons padding lst) target-length padding)))
(define (add-terms L1 L2)
  ; zipping the two list will give us coefficient pairs. We will merely have to sum these lists using a fold
  (let ((ml (max (length L1) (length L2))))
    (map (lambda (l) (apply + l))
         (zip (pad L1 ml 0) (pad L2 ml 0)))))
(define (mul-terms L1 L2)
  (let ((ml (max (length L1) (length L2))))
    (map (lambda (l) (apply * l))
         (zip (pad L1 ml 1) (pad L2 ml 1)))))

;-- 2.90
; This is a major effort, not a local change. <= okay, maybe later then.

;-- 2.91
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result (div-terms (rest-terms L1) (rest-terms L2)) ))
                (add-terms (make-term new-c new-o) rest-of-result)
                ))))))
; NB: untested.

;-- 2.92
; "(This is not easy!)" <= Hmm... Exercise 2.93 to 2.97 are in the same (mathematical) vein, so I'll leave them alone for the time being.