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
(defparameter *graphics-context* nil)

;; Some CLX examples: http://www.cawtech.demon.co.uk/clx/simple/examples.html

(defun init-plot (&key (title "Plot Window") (x-size 500) (y-size 500))
  "Initialize an emtpy Xorg window. Return the window object."
  (let* ((display (open-default-display))
	 (screen (car (display-roots display)))
	 (root-window (screen-root screen))
         (white (screen-white-pixel screen)))
    (setf *plot-window*
	  (create-window :parent root-window
			 :x 0
			 :y 0
			 :width x-size
			 :height y-size
			 :background white
			 ))
    (change-property *plot-window*
		     :wm_name title
		     :string 8)
    (map-window *plot-window*)
    (display-finish-output display)
    (init-graphics-context)
    *plot-window*))

(defun init-graphics-context ()
  (let* ((display (open-default-display))
         (screen (car (display-roots display)))
         (black (screen-black-pixel screen))
         (white (screen-white-pixel screen)))
   (setf *graphics-context*
         (create-gcontext
          :drawable *plot-window*
          :foreground white
          :background black))))

(defun plot-fill-rect (x y xsize ysize pattern)
  (declare (ignorable x y xsize ysize pattern)))

(defun plot-size-rect (x y xsize ysize val)
  (declare (ignorable x y xsize ysize val)))

(defun clear-plot (&key (window *plot-window*))
  (clear-area window
              :width (drawable-width window)
              :height (drawable-height window)))

(defun pen-width (nibs)
  (declare (ignorable nibs)))

(defun plot-frame-rect (x y xsize ysize)
  (declare (ignorable x y xsize ysize)))

(defun plot-line (x1 y1 x2 y2)
  ""
  (let* ((display (open-default-display))
         (screen (car (display-roots display)))
         ;; (white (screen-white-pixel screen))
         (black (screen-black-pixel screen)))
    (map-window *plot-window*)
    (setf (gcontext-foreground *graphics-context*) black)
    (event-case (display :force-output-p t
                         :discard-p t)
      (:exposure ()
                 (break)
                 (draw-line *plot-window*
                            *graphics-context*
                            x1 y1 x2 y2)
                 nil)
      (:button-press () t)
      (:key-press (code state) (char= #\Space (xlib:keycode->character display code state))))

    (display-finish-output display)
    (close-display display)))

(defun show-plot ())

(defun plot-string (x y str &optional (size 10))
  (declare (ignorable x y str size)))

(defun plot-string-bold (x y str &optional (size 12))
  (declare (ignorable x y str size)))

(defun plot-string-italic (x y str)
  (declare (ignorable x y str)))

(defun plot-mouse-down ())

#+nil
(defun test ()
  (show-plot)
  (clear-plot)
  (dotimes (i 6)
    (plot-fill-rect
     (* i 9)
     (* i 9)
     8 8
     i)
    (plot-frame-rect (* i 9) (* I 9) 8 8))
  (dotimes (i 50)
    (plot-size-rect
     (+ 160 (random 200)) (random 100) (random 20) (random 20) (random 5)))
  (dotimes (i 4)
    (plot-string (* i 10) (+ 150 (* i 22)) "Mark's plot utilities ...."))
  (plot-string-bold 20 260 "This is a test... of BOLD")
  (plot-string-italic 20 280 "This is a test ... of ITALIC"))
