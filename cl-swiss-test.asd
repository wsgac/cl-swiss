#|
  This file is a part of cl-swiss project.
  Copyright (c) 2019 Wojciech S. Gac (wojciech.s.gac@gmail.com)
|#

(defsystem "cl-swiss-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Wojciech S. Gac"
  :license "GPLv3"
  :depends-on ("cl-swiss"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-swiss"))))
  :description "Test system for cl-swiss"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
