(defpackage cl-swiss.util
  (:use :cl :circular-streams))
(in-package :cl-swiss.util)

;; blah blah blah.

(defun caesar-cipher (text shift)
  (loop
     with cap-lo = (char-code #\A)
     with min-lo = (char-code #\a)
     with range = (- (char-code #\Z)
                     (char-code #\A)
                     -1)
     for c across text
     if (char<= #\A c #\Z)
     collect (code-char (+ (mod (+ (- (char-code c) cap-lo) shift) range) cap-lo)) into caesar
     else if (char<= #\a c #\z)
     collect (code-char (+ (mod (+ (- (char-code c) min-lo) shift) range) min-lo)) into caesar
     else
     collect c into caesar
     finally (return (coerce caesar 'string))))

(defun letter-frequency (text)
  (loop
     with hash = (make-hash-table)
     for c across text
     if (or (char<= #\A c #\Z)
            (char<= #\a c #\z))
     do (incf (gethash (char-downcase c) hash 0))
     finally (let (alist)
               (maphash #'(lambda (k v) (push (cons k v) alist)) hash)
               (return (sort alist #'> :key #'cdr)))))

(defmacro %polyalphabetic-process (codeword text step)
  `(with-output-to-string (s)
     (loop
        with cw-len = (length ,codeword)
        with offsets = (loop
                          for cc across (string-downcase ,codeword)
                          collect (- (char-code cc)
                                     (char-code #\a)) into res
                          finally (return (apply #'vector res)))
        with i = 0
        for c across (string-downcase ,text)
        if (char<= #\a c #\z)
        do (write-char (code-char (+ (mod (,step (- (char-code c)
                                                (char-code #\a))
                                             (aref offsets (mod i cw-len))) 26)
                                     (char-code #\a))) s)
          (incf i))))

(defun polyalphabetic-encode (codeword text)
  (%polyalphabetic-process codeword text +))

(defun polyalphabetic-decode (codeword text)
  (%polyalphabetic-process codeword text -))
