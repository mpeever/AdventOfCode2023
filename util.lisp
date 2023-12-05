;; -*- lisp-mode -*-

;; Utilities we need for more than one puzzle.

(defun split-string (text &optional (record-separator (string #\Newline)))
  "Split some text string into lines. Default record separator is \n."
  (labels ((split (txt lines)
	     (let ((next-record (search record-separator txt)))
	       (cond ((and next-record
			   (> next-record 0))
		      (split (subseq txt (+ 1 next-record))
			     (cons (subseq txt 0 next-record) lines)))
		     ((and next-record
			   (= 0 next-record))(split (subseq txt 1) lines))
		     ((> (length txt) 0) (cons txt lines))
		     (T lines)))))
    (nreverse (split text '()))))


