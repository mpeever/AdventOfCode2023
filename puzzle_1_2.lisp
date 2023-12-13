;; -*- lisp-mode -*-

;; include utilities
(load (compile-file "~/Projects/AdventOfCode2023/util.lisp"))

(defvar sample-input-2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(defvar symbols '(zero one two three four five six seven eight nine))
(defvar chars (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defun find-digits (text)
  "Search through TEXT, finding all the digits in the order they occur."
  (labels ((ssrch (txt)
	     (mapcar #'(lambda(digit)
			 (let* ((subseq (string digit))
				(start (search subseq txt :test #'equalp))
				(end (and start
					  (+ (length subseq) start))))
			   (if start
			       (list subseq start end))))
		     (concatenate 'list symbols chars)))
	   (first-digit (txt)
	     (car (sort (remove-if #'null (ssrch txt))
			#'(lambda(a b) (< (cadr a) (cadr b))))))
	   (all-digits (txt acc)
	     (let ((1st (first-digit txt)))
	       (cond ((equalp "" txt) acc)
		     ((and 1st
			   (>= (length txt) (car (last 1st))))
		      (all-digits (subseq txt (car (last 1st))) (cons (car 1st) acc)))
		     (T acc)))))
    (reverse (all-digits text '()))))

(defun string-value (string)
  "If a string can be parsed into an integer, parse it!"
  (or (parse-integer string :junk-allowed T)
      ;; ok, so it's not parseable, let's see if we can find it in our list
      (labels ((match (symbol seq posn)
		 (cond ((null seq) '())
		       ((equalp symbol (first seq)) posn)
		       (T (match symbol (cdr seq) (+ 1 posn))))))
	(match string (mapcar #'string symbols) 0))))

(defun first-and-last-digits (line)
  "find first and last digits in a line of text"
  (let* ((digits (find-digits line))
	 (d0 (car digits))
	 (dn (car (reverse digits))))
    (parse-integer (format nil "~d~d" (string-value d0) (string-value dn)) :junk-allowed T)))

(defun two-digit-numbers (text)
  "Return all two-digit numbers from a block of text."
  (remove-if #'null (mapcar #'first-and-last-digits (split-string text))))

(defun puzzle2 (text)
  "Solve first puzzle"
  (reduce #'+ (two-digit-numbers text)))



