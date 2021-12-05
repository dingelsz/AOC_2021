(ql:quickload :cl-ppcre)

;; -----------------------------------------------------------------------------
;; Utils
;; -----------------------------------------------------------------------------
(defun test (lhs rhs)
  "Simple unit test for comparing two values"
  (assert (equal lhs rhs)))

(defun sum (&rest rest) (apply #'+ rest))

(defun transpose (matrix)
  "transposes a matrix"
  (apply #'mapcar #'list matrix))

;; Tests
(test '((1 4 7) (2 5 8) (3 6 9)) (transpose '((1 2 3) (4 5 6) (7 8 9))))

(defun string-to-list (s)
  (if (equal "" s) ()
      (cons (char s 0) (string-to-list (subseq s 1)))))

(defun curry (fn x)
  "curries the function fn with x"
  (lambda (y) (funcall fn x y)))

(test 7 (funcall (curry #'sum 3) 4))

(defun all (&rest rest)
  (loop
    for x in rest
    always x))

(test T (all 1 2 3))
(test NIL (all 1 2 NIL))
(test T (all))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------
(defun load-input (problem-number)
  "Loads the input for the given problem"
  (uiop:read-file-lines (format NIL "~~/projects/aoc_2021/inputs/~d.txt" problem-number)))

;; -----------------------------------------------------------------------------
;; Day 1
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; Problem 1
;; Given a series of numbers count the number of times a number is greater
;; than the previous number
;; -----------------------------------------------------------------------------
(defparameter sonar-report (mapcar #'parse-integer (load-input 1)))

(defun n-consecutive-increases (nums)
  "Counts the number of consecutive elements that increase in the given list"
  (let ((a (first nums))
	(b (second nums))
	(tail (rest nums)))
    (cond ((null a) 0)
	  ((null b) 0)
	  (T (+ (if (< a b) 1 0) (n-consecutive-increases tail))))))

;; Tests
(test 0 (n-consecutive-increases '()))
(test 0 (n-consecutive-increases '(1)))
(test 1 (n-consecutive-increases '(1 2)))
(test 0 (n-consecutive-increases '(2 1)))
(test 3 (n-consecutive-increases '(1 2 3 4)))
(test 0 (n-consecutive-increases '(4 3 2 1)))

(defun problem1 ()
  (n-consecutive-increases sonar-report))

;; -----------------------------------------------------------------------------
;; Problem 2
;; Similar to problem 1 but we are first calculating the rolling sum with a
;; window of 3 and then counting the increases in that series
;; -----------------------------------------------------------------------------
(defun rolling-window (xs window-length)
  "Creates a xs of rolling windows from the given xs"
  (cond ((< (length xs) window-length) nil)
	((= (length xs) window-length) (list xs))
	(T
	 (cons (subseq xs 0 window-length)
	       (rolling-window (rest xs) window-length)))))

(test '() (rolling-window '() 3))
(test '((1) (2) (3)) (rolling-window '(1 2 3) 1))
(test '((1 2) (2 3)) (rolling-window '(1 2 3) 2))
(test '((1 2 3)) (rolling-window '(1 2 3) 3))

(defun problem2 ()
  (let* ((rolling-report (rolling-window sonar-report 3))
	(aggregated-report (applyl #'sum rolling-report)))
    (n-consecutive-increases aggregated-report)))

;; -----------------------------------------------------------------------------
;; Day 2
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; Problem 3:
;; A submarine is controlled using a DSL with the following functions:
;; - forward x: Move forward x units
;; - down/up x: Move down/up x units
;; -----------------------------------------------------------------------------
(defparameter sub-commands (mapcar
			    (lambda (command) (read-from-string (format NIL "(~A)" command)))
			    (load-input 2)))

;; List based approach - calculate the total offset by command and then combine them
(defun problem2-list ()
  (let* ((forwards (remove-if-not (lambda (command) (eq 'FORWARD (first command))) sub-commands))
	 (downs    (remove-if-not (lambda (command) (eq 'DOWN (first command))) sub-commands))
	 (ups      (remove-if-not (lambda (command) (eq 'UP (first command))) sub-commands))
	 (dx (apply #'sum (mapcar #'second forwards)))
	 (dy (- (apply #'sum (mapcar #'second downs)) (apply #'sum (mapcar #'second ups)))))
    (* dx dy)))

;; Create an interpreter for the DSL (lets call it suby) and evaluate it 
(defun suby-run (command)
  "Runs a suby command"
  (let ((com (first command))
	(x   (second command)))
    (cond ((eq com 'FORWARD) (values x 0))
	  ((eq com 'DOWN) (values 0 x))
	  ((eq com 'UP) (values 0 (- x))))))

(defun suby-eval (program)
  "Evaluates a suby program"
  (if (null program) (values 0 0)
      (multiple-value-bind (futurex futurey) (suby-eval (rest program))
	(multiple-value-bind (dx dy) (suby-run (first program))
	  (values (+ dx futurex) (+ dy futurey))))))

;; tests
(test (values 7 0)  (suby-run '(FORWARD 7)))
(test (values 0 5)  (suby-run '(DOWN 5)))
(test (values 0 -3) (suby-run '(UP 3)))

(test (values 7 0) (suby-eval '((FORWARD 7))))
(test (values 7 5) (suby-eval '((FORWARD 7) (DOWN 5))))
(test (values 7 2) (suby-eval '((FORWARD 7) (DOWN 5) (UP 3))))

(defun problem2-dsl ()
  (multiple-value-bind (dx dy) (suby-eval sub-commands)
    (* dx dy)))

(test (problem2-dsl) (problem2-list))

;; -----------------------------------------------------------------------------
;; Problem 4
;; Down and up are actually rates of change. Forward moves forward x unites and
;; up/down x * aim where aim is the sum of previous down/ups. 
;; -----------------------------------------------------------------------------
(defun suby-aim-run (command aim)
  "Runs a suby command"
  (let ((com (first command))
	(x   (second command)))
    (cond ((eq com 'FORWARD) (values x (* x aim) aim))
	  ((eq com 'DOWN) (values 0 0 (+ aim x)))
	  ((eq com 'UP) (values 0 0 (- aim x))))))

(defun suby-aim-eval (program &optional (aim 0))
  "Evaluates a suby program"
  (if (null program) (values 0 0 0)
      (multiple-value-bind (new-x new-y new-aim) (suby-aim-run (first program) aim)
	(multiple-value-bind (fut-x fut-y fut-aim) (suby-aim-eval (rest program) new-aim)
	  (values (+ new-x fut-x) (+ new-y fut-y) fut-aim)))))

;; tests
(test (values 7 0)  (suby-aim-eval '((FORWARD 7))))
(test (values 5 15) (suby-aim-eval '((DOWN 3) (FORWARD 5))))
(test (values 7 1)  (suby-aim-eval '((DOWN 3) (FORWARD 2) (UP 1) (FORWARD 5))))


(defun problem4 ()
  (multiple-value-bind (x y) (suby-aim-eval sub-commands)
    (* x y)))

;; -----------------------------------------------------------------------------
;; Reflections:
;; I like the DSL approach. It has two main advantages:
;; 1) It's easy to extend
;; 2) It's recursive nature cuts down on the amount of code you need to write
;; One weakness I am noticing is that it is hard to move around state. I think
;; that's just an issue on my part, I haven't come up with any good abstractions
;; to represent state. One approach is to pass around a key value store that
;; can carry the state.
;; On a more detailed level, multiple-value-bind was chosen because it can
;; unpack values easily. It is akward to use besides that. A nice medium
;; could be a state abstraction that easily unpacks into bindings.
;; -----------------------------------------------------------------------------


;; -----------------------------------------------------------------------------
;; Day 3
;; -----------------------------------------------------------------------------
;; Problem 5
;; -----------------------------------------------------------------------------

(defparameter diagnostic-report (mapcar (lambda (x) (mapcar #'digit-char-p x))
					(mapcar #'string-to-list (load-input 3))))

(defun diagnostic-ratios (nums)
  (loop
    with N = (length (first nums))
    with M = (length nums)
    for i below N
    collect (/ (apply #'sum (mapcar (lambda (x) (nth i x)) nums))
	       M)))

(defun bin-list-to-integer (bin-list)
  (parse-integer (apply #'concatenate 'string  (mapcar #'write-to-string bin-list)) :radix 2))

(defun problem5-list ()
  (let* ((data (transpose diagnostic-report))
	 (N (length (first data)))
	 (M (mapcar (lambda (digit) (count 1 digit)) data))
	 (delta (bin-list-to-integer (mapcar (lambda (x) (if (> 0.5 (/ x N)) 1 0)) M)))
	 (epsilon (bin-list-to-integer (mapcar (lambda (x) (if (< 0.5 (/ x N)) 1 0)) M))))
    (* delta epsilon)))

(defun problem5 ()
  (let* ((ratios (diagnostic-ratios diagnostic-report))
	 (delta (bin-list-to-integer (mapcar (lambda (x) (if (> x 0.5) 1 0)) ratios)))
	 (epsilon (bin-list-to-integer (mapcar (lambda (x) (if (< x 0.5) 1 0)) ratios))))
    (* delta epsilon)))

(test  (problem5) (problem5-list))

;; -----------------------------------------------------------------------------
;; Problem 6
;; -----------------------------------------------------------------------------
(defun process (nums predicate &optional (i 0))
  (if (eq 1 (length nums)) (first nums)
      (let* ((ratios (diagnostic-ratios nums))
	     (keep (if (funcall predicate (nth i ratios)) 1 0))
	     (filtered-nums (remove-if-not (lambda (x) (eq (nth i x) keep)) nums)))
	(process filtered-nums predicate (+ 1 i)))))

(defun problem6 ()
  (let ((O-rating (bin-list-to-integer (process diagnostic-report (lambda (ratio) (<= 0.5 ratio)))))
	(CO2-rating (bin-list-to-integer (process diagnostic-report (lambda (ratio) (> 0.5 ratio))))))
    (* O-rating CO2-rating)))


;; -----------------------------------------------------------------------------
;; Retrospective
;; This problem took a long time because I had to implement a few of the binary
;; operations. I also got stuck trying to implement a complex solution to
;; problem 6 on the first go. Once I added a good abstraction to handle ratios
;; the problem became much easier. Lesson: if your answer gets too complicated
;; look for opportunities to apply abstractions. 
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Day 4
;; -----------------------------------------------------------------------------
;; Problem 7
;; Bingo - Given a sequence of numbers and a set of 5x5 boards, figure out
;; which board will win
;; Thoughts
;; - Could update values in place by setting them to -1 to mark drawn numbers
;; - Lets try a top down approach
;; -----------------------------------------------------------------------------

;; While numbers and not a winning board
;; - draw a number and add it to each board
;; With the winning board
;; - return product of all non selected numbers and last draw number
(defun mark-board (n board)
  (if (null board) ()
      (cons (substitute NIL n (first board)) (mark-board n (rest board)))))

(defun unmarked-sum (board)
  (if (null board) 0
      (+ (apply #' sum (remove NIL (first board))) (unmarked-sum (rest board)))))

(defun winner-p (board)
  (flet ((good-board (board) (loop
			       for row in board
			       do (when (null (remove-if #'null row)) (return T)))))
    (or (good-board board) (good-board (transpose board)))))

 
;; Tests
(defparameter test-board '((1 2 3) (4 5 6) (7 8 9)))
(test '((1 2 NIL) (4 5 6) (7 8 9)) (mark-board 3 test-board))
(test test-board (mark-board 10 test-board))

(test 45 (unmarked-sum test-board))
(test 42 (unmarked-sum '((1 2 NIL) (4 5 6) (7 8 9))))

(test NIL (winner-p test-board))
(test T (winner-p '((NIL 1) (NIL 2))))
(test T (winner-p '((NIL NIL) (3 2))))

(winner-p '((NIL 1) (NIL 2)))

;; Problem driver
(defparameter bingo-draws
  (mapcar #'parse-integer
	  (cl-ppcre:split " " (cl-ppcre:regex-replace-all "," (first (load-input 4)) " "))))

(defun bingo-boards ()
  (let ((rows (remove NIL
		       (loop
			 for row in (rest (load-input 4))
			 collect (mapcar #'parse-integer
					 (remove-if (lambda (x) (or (equal x "") (equal x " ") (null x)))
						    (cl-ppcre:split " " row)))))))
    (loop repeat (/ (length rows) 5)
	  for i = 0 then (+ i 5)
	  collect (subseq rows i (+ i 5)))))

(defun problem7 (numbers boards)
  (loop
    for n in numbers
    do (setf boards (mapcar (lambda (b) (mark-board n b)) boards))
    do (let ((winning-board (find T boards :key #'winner-p)))
	 (when winning-board (return (* n (unmarked-sum winning-board)))))))


;; -----------------------------------------------------------------------------
;; Problem 8
;; Instead of looking for the first wining board we are looking for the last
;; winning board. 
;; -----------------------------------------------------------------------------
(defun problem8 (numbers boards)
  (loop
    for n in numbers
    do (let ((remaining-boards (remove-if #'winner-p boards)))
	 (when (and (= 1 (length remaining-boards))
		    (winner-p (mark-board n (first remaining-boards))))
	   (return (* n (unmarked-sum (mark-board n (first remaining-boards)))))))
    do (setf boards (mapcar (lambda (b) (mark-board n b)) boards))))


;; -----------------------------------------------------------------------------
;; Retrospective:
;; Working from the top down was a good approach for this problem. Lisp is a
;; language that likes to go top down. I do keep running into issues processing
;; data. ppcre helped with some of the string processing (regex-replace-all and
;; split).
;; -----------------------------------------------------------------------------


