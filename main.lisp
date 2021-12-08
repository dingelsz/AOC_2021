(ql:quickload :cl-ppcre)

;; -----------------------------------------------------------------------------
;; Utils
;; -----------------------------------------------------------------------------
(defun test (lhs rhs &optional (test #'equal))
  "Simple unit test for comparing two values"
  (if (and (consp lhs) (consp rhs))
      (loop
	for i from 0 below (length lhs)
	do (assert (funcall test (nth i lhs) (nth i rhs))))
      (assert (funcall test lhs rhs))))

(test 1 1)
(test 1 1 #'=)
(test '(1 2 3) '(1 2 3))

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

(defun flatten (xs)
  (cond ((null xs) ())
	((atom (first xs)) (cons (first xs) (flatten (rest xs))))
	(T (append (flatten (first xs)) (flatten (rest xs))))))

(test '(1 2 3 2 3 4) (flatten '((1 2 3) (2 3 4))))

(defun hash-counter (items &optional (test #'equal))
  (setq counter (make-hash-table :test test))
  (loop for x in items do (setf (gethash x counter) (+ 1 (or (gethash x counter) 0))))
  counter
  )

(defun hash-inc (key hash delta)
  (setf (gethash key hash) (+ delta (gethash key hash 0))))

(defun hash-print (hash)
  (loop for key being the hash-keys in hash
	do (format T "key=~A, value=~A~%" key (gethash key hash))))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun hash-values (hash-table)
  (loop for value being the hash-values of hash-table collect value))

(defun hash-get-inv (value hash-table &optional (test #'eq))
  (loop for key in (hash-keys hash-table)
	when (funcall test value (gethash key hash-table)) collect key))

(test '(1 2) (hash-get-inv 3 (hash-counter '(1 1 1 2 2 2))))

(defun curry (fn x)
  (lambda (&rest rest) (apply fn (cons x rest))))

(test 6 (funcall (curry #'+ 1) 2 3))

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
	(aggregated-report (apply #'sum rolling-report)))
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


;; -----------------------------------------------------------------------------
;; Day 5
;; -----------------------------------------------------------------------------
;; Problem 9
;; Given a list of line endpoints, and considering only lines that are
;; vertical or horizontal, how many points exist where two or more lines
;; overlap?
;; -----------------------------------------------------------------------------
(defun line-from-string (s)
  (ppcre:register-groups-bind ((#'parse-integer x1)
			       (#'parse-integer y1)
			       (#'parse-integer x2)
			       (#'parse-integer y2))
      ("([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)" s)
    `((,x1 ,y1) (,x2 ,y2))))

(defun points-from-line (l)
  (destructuring-bind ((x1 y1) (x2 y2)) l
    (if (= x1 x2)
	(loop
	  for y from (min y1 y2) to (max y1 y2)
	  collect (list x1 y))
	(let* ((m (/ (- y2 y1) (- x2 x1)))
	       (b (- y1 (* m x1))))
	  (loop
	    for x from (min x1 x2) to (max x1 x2)
	    collect (list x (+ (* m x) b)))))))
 
;; tests
(test '((1 9) (2 9)) (line-from-string "1,9 -> 2,9"))
(test '((3 9) (2 9)) (line-from-string "3,9 -> 2,9"))
(test '((0 0) (1 1)) (line-from-string "0,0 -> 1,1"))
(test '((0 0) (0 1)) (line-from-string "0,0 -> 0,1"))

(test '((1 9) (2 9)) (points-from-line (line-from-string "1,9 -> 2,9")))
(test '((2 9) (3 9)) (points-from-line (line-from-string "3,9 -> 2,9")))
(test '((0 0) (1 1)) (points-from-line (line-from-string "0,0 -> 1,1")))
(test '((0 0) (0 1)) (points-from-line (line-from-string "0,0 -> 0,1")))
(test '((2 0) (3 1) (4 2)) (points-from-line (line-from-string "4,2 -> 2,0")))

(defun diagonal-p (line)
  (destructuring-bind ((x1 y1) (x2 y2)) line
    (and (/= x1 x2)
	 (/= y1 y2))))

(test NIL (diagonal-p (line-from-string "0,0 -> 1,0")))
(test T   (diagonal-p (line-from-string "0,0 -> 1,1")))

;; Problem driver
(defun problem9 ()
  (let ((points (reduce
		 #'append
		 (loop for line in (mapcar #'line-from-string (load-input 5))
		       unless (diagonal-p line) collect (points-from-line line)))))
    (loop for count being the hash-values in (hash-counter points)
	  counting (> count 1))))


;; -----------------------------------------------------------------------------
;; Problem 10
;; Easy - just remove diagonal filtering
;; -----------------------------------------------------------------------------
(defun problem10 ()
  (let ((points (reduce
		 #'append
		 (loop for line in (mapcar #'line-from-string (load-input 5))
		       collect (points-from-line line)))))
    (loop for count being the hash-values in (hash-counter points)
	  counting (> count 1))))

;; -----------------------------------------------------------------------------
;; Day 6
;; -----------------------------------------------------------------------------
;; Problem 11
;; -----------------------------------------------------------------------------
(defparameter fish-ages (mapcar #'parse-integer (ppcre:split "," (first (load-input 6)))))
(defparameter fish-count (hash-counter fish-ages))

(defun fish-step (fish-count)
  (let ((fish-count_t+1 (hash-counter ()))
	(ready-fish (gethash 0 fish-count 0)))
    (loop for age from 8 downto 1
	  do (setf (gethash (- age 1) fish-count_t+1) (gethash age fish-count 0)))
    (hash-inc 6 fish-count_t+1 ready-fish)
    (hash-inc 8 fish-count_t+1 ready-fish)
    fish-count_t+1))

(defun fish-sim (fish-count days)
  (if (< days 1) fish-count
      (fish-sim (fish-step fish-count) (- days 1))))

(defun problem11 ()
  (loop for c being the hash-values in (fish-sim fish-count 80) sum c))

;; -----------------------------------------------------------------------------
;; Problem 12
;; Counting fish over a larger time span. They are "fishing" for scalablility
;; issues
;; -----------------------------------------------------------------------------
(defun problem12 ()
  (loop for c being the hash-values in (fish-sim fish-count 256) sum c))

;; -----------------------------------------------------------------------------
;; Day 7
;; -----------------------------------------------------------------------------
;; Problem 13
;; -----------------------------------------------------------------------------

(defun cost-to-move-to (x crabs cost)
  (loop for pos being the hash-keys of crabs
	summing (* (gethash pos crabs) (funcall cost x pos)) into sum
	finally (return sum)))

;; Tests
(test 1 (cost-to-move-to 0 (hash-counter '(1)) (lambda (x pos) (abs (- x pos)))))
(test 2 (cost-to-move-to 0 (hash-counter '(1 1)) (lambda (x pos) (abs (- x pos)))))
(test 4 (cost-to-move-to 0 (hash-counter '(1 1 2)) (lambda (x pos) (abs (- x pos)))))
(test 37 (cost-to-move-to 2 (hash-counter '(16 1 2 0 4 2 7 1 2 14)) (lambda (x pos) (abs (- x pos)))))

(test 1 (cost-to-move-to 0
			   (hash-counter '(1))
			   (lambda (x pos) (let ((n (abs (- x pos))))
					     (/ (* (+ 1 n) n) 2)))))

(test 3 (cost-to-move-to 0
			   (hash-counter '(2))
			   (lambda (x pos) (let ((n (abs (- x pos))))
					     (/ (* (+ 1 n) n) 2)))))
(test 168 (cost-to-move-to 5
			   (hash-counter '(16 1 2 0 4 2 7 1 2 14))
			   (lambda (x pos) (let ((n (abs (- x pos))))
					     (/ (* (+ 1 n) n) 2)))))



(defun problem13 ()
  (let* ((counter (hash-counter (mapcar #'parse-integer (ppcre:split "," (first  (load-input 7))))))
	 (left (apply #'min (hash-keys counter)))
	 (right (apply #'max (hash-keys counter))))
    (loop
      for i from left to right
      minimizing (cost-to-move-to i counter (lambda (x pos) (abs (- x pos)))) into min
      finally (return min))))

;; -----------------------------------------------------------------------------
;; Problem14
;; -----------------------------------------------------------------------------

(defun problem14 ()
  (let* ((counter (hash-counter (mapcar #'parse-integer (ppcre:split "," (first  (load-input 7))))))
	 (left (apply #'min (hash-keys counter)))
	 (right (apply #'max (hash-keys counter))))
    (loop
      for i from left to right
      minimizing (cost-to-move-to
		  i
		  counter
		  (lambda (x pos) (let ((n (abs (- x pos))))
				    (/ (* (+ 1 n) n) 2)))) into min
      finally (return min))))

;; -----------------------------------------------------------------------------
;; Retrospective
;; Whats the runtime?
;; The outer loop of the program runs for every number between the smallest
;; and largest. That could be a very large number but lets call the difference n
;; Cost-to-move-to calculates a constant value for each unique crab position.
;; We can call that number m. So, the runtime is O(n*m).
;; I'm not sure how but I have a feeling a hueristic could be used to speed up
;; the algo. Maybe look around the median/mean? 
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Day 8
;; -----------------------------------------------------------------------------
;; Problem 15
;; -----------------------------------------------------------------------------
(defun problem15 ()
  (loop
    with lines = (mapcar (lambda (row) (ppcre:split " \\| " row)) (load-input 8))
    with outputs = (mapcar #'second lines)
    for output in outputs
    sum (loop for segment in (ppcre:split " " output)
	      counting (member (length segment) '(2 3 4 7)))))

;; -----------------------------------------------------------------------------
;; Problem 16
;; 1) Count occurances of letters in patterns
;; 2) Deduce segment mapping
;; 3) Apply segment map to output
;; 4) Map 3) to digits
;; 5) Sum all outputs
;; -----------------------------------------------------------------------------
(defun make-segment-map (segment-counts one four seven eight)
  (let ((hash (make-hash-table)))
    (setf (gethash #\a hash) (first (set-difference seven one)))
    (setf (gethash #\b hash) (first (hash-get-inv 6 segment-counts)))
    (setf (gethash #\c hash) (first (set-difference (hash-get-inv 8 segment-counts) (set-difference seven one))))
    (setf (gethash #\d hash) (first (intersection (hash-get-inv 7 segment-counts) four)))
    (setf (gethash #\e hash) (first (hash-get-inv 4 segment-counts)))
    (setf (gethash #\f hash) (first (hash-get-inv 9 segment-counts)))
    (setf (gethash #\g hash) (first (set-difference (hash-get-inv 7 segment-counts) four)))
    hash))

(defun decode-digit (digit mapping)
  (flet ((match (a b) (null (set-exclusive-or (string-to-list a) (string-to-list b)))))
    (let ((segment (coerce (flatten (mapcar (lambda (d) (hash-get-inv d mapping)) (string-to-list digit))) 'string)))
      (cond
	((match segment "abcefg")	0)
	((match segment "cf")		1)
	((match segment "acdeg")	2)
	((match segment "acdfg")	3)
	((match segment "bcdf")		4)
	((match segment "abdfg")	5)
	((match segment "abdefg")	6)
	((match segment "acf")		7)
	((match segment "abcdefg")	8)
	((match segment "abcdfg")	9)
	(T (format T "Unknown segment ~s" segment))))))

(defun problem16 ()
  (loop
    ;; Parse inputs
    with lines = (load-input 8)
    with splits = (mapcar (lambda (row) (ppcre:split " \\| " row)) lines)
    
    for split in splits
    ;; Split each row into signal and output
    sum (let* ((signals (first split))
	       (outputs (second split))
	       (one (string-to-list (find 2 (ppcre:split " " signals) :key #'length)))
	       (four (string-to-list (find 4 (ppcre:split " " signals) :key #'length)))
	       (seven (string-to-list (find 3 (ppcre:split " " signals) :key #'length)))
	       (eight (string-to-list (find 7 (ppcre:split " " signals) :key #'length)))
	       (segment-counts (hash-counter (string-to-list (ppcre:regex-replace-all " " signals ""))))
	       (segment-map (make-segment-map segment-counts one four seven eight))
	       (solution (loop 
			   for mapped-digit in (ppcre:split " " outputs)
			   collect (digit-char (decode-digit mapped-digit segment-map)))))
	      (parse-integer (coerce solution 'string)))))

(problem16)


