(defpackage cl-swiss.onlisp.util
  (:use :cl :alexandria)
  (:nicknames :onlisp-util))

(in-package :cl-swiss.onlisp.util)

#+nil
(defun something (n m)
  (macrolet ((with-binding (a b)
               (let ((g (gensym)))
                 `(progn
                    (setf (symbol-function ,g) #'(lambda (x) (+ x ,a)))
                    (,g ,b)))))
    (with-binding n m)))

(defun find2 (fn lst)
  "Find the first element in LST for which the application of FN
returns non-NIL and return both the element and result as values."
  (when lst
    (let ((val (funcall fn (car lst))))
      (if val
          (values (car lst) val)
          (find2 fn (cdr lst))))))

(defun last1 (lst)
  "Return the CAR of the last cons i.e. the last element of a list."
  (declare (inline))
  (car (last lst)))

(defun single (lst)
  "Check if LST is a singleton."
  (declare (inline))
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  "Non-destructively append OBJ to the end of LST."
  (declare (inline))
  (append lst (list obj)))

(defun nconc1 (lst obj)
  "Destructively append OBJ to the end of LST."
  (declare (inline))
  (nconc lst (list obj)))

(defun mklist (obj)
  "Ensure that OBJ is a list."
  (declare (inline))
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  "Check if X is longer than Y. If both are lists, use a
short-circuiting approach rather than computing full length."
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  "Return a list of non-NIL results of applying FN to elements of
LST."
  (let ((acc nil))
    (dolist (el lst)
      (let ((val (funcall fn el)))
        (if val
            (push val acc))))
    (nreverse acc)))

(defun group (lst n)
  "Group elements of LST into lists of N elements. The last list might
  contain fewer elements."
  (assert (integerp n))
  (if (zerop n) (error "Zero length"))
  (labels ((rec (lst acc)
             (let ((rest (nthcdr n lst)))
               (if (consp rest)
                   (rec rest (cons (subseq lst 0 n) acc))
                   (nreverse (cons lst acc))))))
    (when lst (rec lst nil))))

;; Already defined in :alexandria
#+nil
(defun flatten (x)
  "Make X into a completely flat list."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  "Prune TREE by applying TEST to each leaf."
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

(defun before (x y lst &key (test #'eql))
  ""
  (and lst
       (let ((first (car x)))
         (cond ((funcall test first y) nil)
               ((funcall test first x) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  ""
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  ""
  (member obj (cdr (member obj lst :test test)) :test test))

(defun split-if (fn lst)
  ""
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn lst)
  ""
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(defun best (fn lst)
  ""
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(defun mostn (fn lst)
  ""
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

;; Mapping functions

(defun map-> (fn start test-fn succ-fn)
  "Apply FN to each item of the sequence starting with START, ending
when TEST-FN yields T, with successive elements generated by applying
SUCC-FN."
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mapa-b (fn a b &optional (step 1))
  ""
  (map-> fn a
         (rcurry #'> b)
         (curry #'+ step)))

(defun map0-n (fn n)
  ""
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  ""
  (mapa-b fn 1 n))

;; Already defined in :alexandria
#+nil
(defun mappend (fn &rest lsts)
  ""
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  ""
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (el lst)
        (push (funcall fn el) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  ""
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))

(defun readlist (&rest args)
  ""
  (values (read-from-string
           (concatenate 'string "(" (apply #'read-line args) ")"))))

(defun prompt (&rest args)
  ""
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  ""
  (format *query-io* "Entering break-loop.~%")
  (loop
     (let ((in (apply #'prompt args)))
       (if (funcall quit in)
           (return)
           (format *query-io* "~A~%" (funcall fn in))))))

(defun mkstr (&rest args)
  ""
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  ""
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  ""
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  ""
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
        (symbol-name sym)))

;; Chapter 5

(defvar *!equivs* (make-hash-table)
  "Hash table for storing destructive counterparts of functions.")

(defun ! (fn)
  "Retrieve destructive counterpart of function FN. Requires previous
association to be established by DEF!."
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  "Establish FN! as a destructive counterpart of FN."
  (setf (gethash fn *!equivs*) fn!))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fn args)))))))

(defun fif (if then &optional else)
  "Functional IF. Return a function, whose argument gets passed
  through an IF-like expression, applying IF, THEN and ELSE to that
  argument."
  #'(lambda (x)
      (if (funcall if x)
	  (funcall then x)
	  (if else (funcall else x)))))

;; Paul Graham's version
#+nil
(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
	#'(lambda (x)
	    (and (funcall fn x) (funcall chain x))))))

;; My version using EVERY
(defun fintersection (&rest fns)
  "Functional intersection. Return a predicate function checking if
  all the functions FN and FNS evaluate to T on its argument."
  #'(lambda (x)
      (every #'(lambda (f) (funcall f x)) fns)))

(defun funion (&rest fns)
  "Functional intersection. Return a predicate function checking if
  all the functions FN and FNS evaluate to T on its argument."
  #'(lambda (x)
      (some #'(lambda (f) (funcall f x)) fns)))
