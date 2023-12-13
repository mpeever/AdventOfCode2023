;; -*- lisp-mode -*-

(load (compile-file "~Projects/AdventOfCode2023/util.lisp"))

(defvar sample-input-1 "
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
")

(defun first-and-last-digits (line)
  "Find the first and last digits of a line of text. If only one digit appears, then it is both the first and the last."
  (let* ((digits (remove-if #'(lambda (char) (null (parse-integer (string char)
								  :junk-allowed T)))
			    line))
	 (len (length digits)))
    (cond ((> len 0) (concatenate 'string (subseq digits 0 1)
				  (subseq digits (- len 1))))
	  (T '()))))

(defun two-digit-numbers (text)
  "Return all two-digit numbers from a block of text."
  (remove-if #'null (mapcar #'first-and-last-digits (split-string text))))


(defun puzzle1 (text)
  "Solve first puzzle"
  (reduce #'+ (mapcar #'parse-integer (two-digit-numbers text))))

