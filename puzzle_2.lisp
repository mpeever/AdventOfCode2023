;; -*- lisp-mode -*-

;; include utilities from Puzzle 1
(load (compile-file "~/Projects/AdventOfCode2023/puzzle_1.lisp"))

(defvar sample-input-2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(defvar numbers '(zero one two three four five six seven eight nine))

(defun parse-digit-symbol (symbol)
  "Find numeric value for a symbol"
  (labels ((match (symbol seq posn)
	     (cond ((null seq) '())
		   ((eq symbol (first seq)) posn)
		   (T (match symbol (cdr seq) (+ 1 posn))))))
    (match symbol numbers 0)))

(defun search-symbol (symbol str)
  "Find start of SYMBOL embedded in STR."
  (search (string symbol) str :test #'equalp))

(defgeneric parse (digit)
  (:documentation "parse a digit"))

(defmethod parse ((digit string))
  (parse-integer digit :junk-allowed T))

(defmethod parse (digit)
  (cond ((symbolp digit) (parse-digit-symbol digit))
	(T (parse (string digit)))))

(defgeneric location (digit string)
  (:documentation "find all the locations of DIGIT in STRING"))

(defmethod location ((digit symbol) string)
  (labels ((loc (digit str acc)
	     (let ((locn (search-symbol digit str)))
	       (cond ((null locn) acc)
		     ((= locn (length str)) acc)
		     (T (loc digit (subseq str (+ 1 locn)) (cons locn acc)))))))
    (nreverse (loc digit string '()))))

(defmethod location (digit string)
  (labels ((loc (digit str acc)
	     (let ((locn (search (string digit) str)))
	       (cond ((null locn) acc)
		     ((= locn (length str)) acc)
		     (T (loc digit (subseq str (+ 1 locn)) (cons locn acc)))))))
    (nreverse (loc digit string '()))))
		       
(defun digits (line)
  "Return all digits in a line, both literals and spelled."
  (labels ((decorate (digit locations)
	     (mapcar #'(lambda (lcn) (list digit lcn)) locations)))
    (let* ((literals (remove-if #'(lambda(char) (null (parse-integer (string char) :junk-allowed T)))
				line))
	   (symbols (remove-if #'null (mapcar #'(lambda (symbol)
						  (if (search-symbol symbol line)
						      symbol))
					      numbers)))
	   (digs (concatenate 'list literals symbols)))
      
      (let ((locns (apply #'concatenate 'list
				(mapcar #'(lambda (digit)
					    (decorate digit (location digit line)))
					digs))))
	(sort locns #'(lambda (a b) (< (cadr a) (cadr b))))))))
    
(defun first-and-last-digits (line)
  "find first and last digits in a line of text"
  (let* ((digits (digits line))
	 (d0 (car (first digits)))
	 (dn (car (first (reverse digits)))))
    (parse-integer (format nil "~d~d" (parse d0) (parse dn)) :junk-allowed T)))

(defun two-digit-numbers (text)
  "Return all two-digit numbers from a block of text."
  (remove-if #'null (mapcar #'first-and-last-digits (split-string text))))

(defun puzzle2 (text)
  "Solve first puzzle"
  (reduce #'+ (two-digit-numbers text)))



