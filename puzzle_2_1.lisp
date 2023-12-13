;; Advent of Code, Puzzle 3
;; -*- lisp-mode -*-

;; Load utils
(load (compile-file "~/Projects/AdventOfCode2023/util.lisp"))

(defvar sample-input-3 "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
")

(defvar digits (list #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))

(defun digit-p (char)
  (find char digits))

(defun digits (line)
  "loop through a line, capturing all digits and their positions"
  (loop for char across line
	for x = 0 then (+ 1 x)
	when (digit-p char)
	  collect (list char x)))

(defun munch(line acc numbers)
  "read through a line of text, capturing the numbers in a line"
  (loop for char across line
	for x = 0
	then x = x + 1
	when (not (equalp char #\.))
	  collect 


(defun number-locations (line)
  "give the positions in a line occupied by a number."
  
	  


