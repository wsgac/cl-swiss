(defpackage cl-swiss.machine-independent-graphics
  (:use :cl)
  (:nicknames :cl-swiss.mig :cl-swiss.mi-graphics)
  (:use :xlib))
(in-package :cl-swiss.mi-graphics)

;;;; This is an implementation of machine-independent graphics
;;;; utilities adapted from Mark Watson's book "Common LISP Modules:
;;;; Artificial Intelligence in the Era of Neural Networks and Chaos
;;;; Theory".

(defparameter *plot-window* nil)

;; Some CLX examples: http://www.cawtech.demon.co.uk/clx/simple/examples.html

(defun init-plot (&key (title "Plot Window") (x-size 500) (y-size 500))
  (let* ((display (open-default-display))
	 (screen (car (display-roots display)))
	 (root-window (screen-root screen)))
    (setf *plot-window*
	  (create-window :parent root-window
			 :x 0
			 :y 0
			 :width x-size
			 :height y-size))
    (change-property *plot-window*
		     :wm_name title
		     :string 8)
    *plot-window*))
