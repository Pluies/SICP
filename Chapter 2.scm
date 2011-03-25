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
; first list and then pushed in the second ¿ resulting in a reverse
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

;-- 2.45


http://sisc-scheme.org/sisc-online.php 
http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html
http://community.schemewiki.org/?sicp-ex-2.40
