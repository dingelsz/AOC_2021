;; -----------------------------------------------------------------------------
;; Utils
;; -----------------------------------------------------------------------------
(defun test (lhs rhs)
  "Simple unit test for comparing two values"
  (assert (= lhs rhs)))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------
(defun load-input (problem-number)
  "Loads the input for the given problem"
  (uiop:read-file-lines (format NIL "~~/projects/aoc_2021/inputs/~d.txt" problem-number)))

;; -----------------------------------------------------------------------------
;; Problem 1
;; Given a series of numbers count the number of times a number is greater
;; than the previous number
;; -----------------------------------------------------------------------------
(defun problem1 (nums)
  (let ((a (first nums))
	(b (second nums))
	(tail (rest nums)))
    (cond ((null a) 0)
	  ((null b) 0)
	  (T (+ (if (< a b) 1 0) (problem1 tail))))))

;; Tests
(test 0 (problem1 '()))
(test 0 (problem1 '(1)))
(test 1 (problem1 '(1 2)))
(test 0 (problem1 '(2 1)))
(test 3 (problem1 '(1 2 3 4)))
(test 0 (problem1 '(4 3 2 1)))

