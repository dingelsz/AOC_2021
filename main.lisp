;; -----------------------------------------------------------------------------
;; Utils
;; -----------------------------------------------------------------------------
(defun test (lhs rhs)
  "Simple unit test for comparing two values"
  (assert (equal lhs rhs)))

(defun sum (&rest rest) (apply #'+ rest))
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
;; -----------------------------------------------------------------------------













