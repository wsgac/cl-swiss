(defpackage cl-swiss.gopher
  (:use :cl :usocket))
(in-package :cl-swiss.gopher)

(defparameter *gopher-port* 70)

(defun list-server-contents (host &key directory (port *gopher-port*))
  "Get listing of DIRECTORY at a particular Gopher HOST. If DIRECTORY
is not specified, display root directory contents."
  (with-client-socket (socket stream host port)
    (when magic-string
      (write-string magic-string stream))
    (write-char #\newline stream)
    (force-output stream)
    (loop
       for line = (read-line stream nil)
       while line
       collect line into lines
       finally (return lines))))
