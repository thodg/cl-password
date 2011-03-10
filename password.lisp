;;
;;  cl-password  -  Password generation in Common Lisp
;;
;;  Copyright 2010 Thomas de Grivel <billitch@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defpackage :cl-password
  (:use :cl)
  (:export #:make-password))

(in-package :cl-password)

(declaim (optimize speed safety))

(let ((character-classes
       '((:upper . #.(coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'simple-base-string))
	 (:lower . #.(coerce "abcdefghijklmnopqrstuvwxyz" 'simple-base-string))
	 (:alpha . (:upper :lower :lower))
	 (:number . #.(coerce "0123456789" 'simple-base-string))
	 (:alphanum . (:alpha :alpha :number))
	 (:punctuation . #.(coerce "_,.:;?!+-*/%|{}()[]~&$@" 'simple-base-string))
	 (:all . (:alphanum :alphanum :punctuation)))))
  
  (defun collect-character-class (name)
    (let ((members (cdr (assoc name character-classes))))
      (unless members (error "Unknown character class ~S" name))
      (etypecase members
	(string (list members))
	(list   (mapcan 'collect-character-class members)))))

  (defun character-class-list (&rest classes)
    (mapcan #'collect-character-class classes))

  (defun compose (from-chars into)
    (declare (type simple-base-string from-chars into))
    (let ((from-chars-length (the (unsigned-byte 16)
			       (length from-chars))))
      (dotimes (i (length into) into)
	(setf (char into i) (char from-chars
				  (random from-chars-length))))))

  (defun check-class (class string)
    (declare (type simple-base-string class string))
    (dotimes (i (length class) nil)
      (when (find (char class i) string)
	(return-from check-class t))))

  (defun check-classes (classes string)
    (declare (type simple-base-string string)
	     (type list classes))
    (dolist (c classes t)
      (unless (check-class c string)
	(return-from check-classes nil))))
  
  (defun make-password (&key (bits 64) (classes :all))
    (let* ((classes (apply #'character-class-list
			   (if (listp classes) classes (list classes))))
	   (chars (apply #'concatenate 'simple-base-string classes))
	   (single-chars (the simple-base-string
			   (remove-duplicates chars)))
	   (password-length (ceiling (the single-float
				       (log (expt 2 bits)
					    (length single-chars)))))
	   (password (make-string password-length
				  :element-type 'base-char)))
      (assert (< (length classes) password-length))
      (loop
	 do (compose chars password)
	 until (check-classes classes password))
      password)))
