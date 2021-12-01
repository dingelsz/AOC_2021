;; -----------------------------------------------------------------------------
;; Utils
;; -----------------------------------------------------------------------------
(defun test (lhs rhs)
  "Simple unit test for comparing two values"
  (assert (equal lhs rhs)))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------
(defun load-input (problem-number)
  "Loads the input for the given problem"
  (uiop:read-file-lines (format NIL "~~/projects/aoc_2021/inputs/~d.txt" problem-number)))

(defparameter sonar-report (mapcar #'parse-integer (load-input 1)))
;; -----------------------------------------------------------------------------
;; Problem 1
;; Given a series of numbers count the number of times a number is greater
;; than the previous number
;; -----------------------------------------------------------------------------
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
  (COND ((< (length xs) window-length) NIL)
	((= (length xs) window-length) (list xs))
	(T
	 (cons (subseq xs 0 window-length)
	       (rolling-window (rest xs) window-length)))))

(test '() (rolling-window '() 3))
(test '((1) (2) (3)) (rolling-window '(1 2 3) 1))
(test '((1 2) (2 3)) (rolling-window '(1 2 3) 2))
(test '((1 2 3)) (rolling-window '(1 2 3) 3))

(defun problem2 ()
  (LET ((aggregated-report (applyl #'+ (rolling-window sonar-report 3))))
    (n-consecutive-increases aggregated-report)))

