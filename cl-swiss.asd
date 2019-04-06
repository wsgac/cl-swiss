#|
  This file is a part of cl-swiss project.
  Copyright (c) 2019 Wojciech S. Gac (wojciech.s.gac@gmail.com)
|#

#|
  Author: Wojciech S. Gac (wojciech.s.gac@gmail.com)
|#

(defsystem "cl-swiss"
  :version "0.1.0"
  :author "Wojciech S. Gac"
  :license "GPLv3"
  :depends-on ("cl-ppcre"
               "alexandria"
               "clx"
               "usocket"
               "ironclad"
               "flexi-streams"
               "circular-streams"
               "inferior-shell"
               "bordeaux-threads")
  :components ((:module "src"
                :components
                ((:module "onlisp"
			  :components
			  ((:file "util")))
		 (:file "util")
                 (:file "cl-swiss")
		 (:file "machine-independent-graphics")
                 (:file "gopher")
                 (:file "sailfish"))))
  :description "An effort to build a personal library of useful Common
  Lisp utilities (Swiss Army knife)"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "cl-swiss-test"))))
