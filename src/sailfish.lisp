(defpackage cl-swiss.sailfish
  (:use :cl :cl-swiss.util :inferior-shell :bt)
  (:nicknames :sailfish-util)
  (:documentation "This package contains assorted utilities for
  communicating with Sailfish OS."))
(in-package :cl-swiss.sailfish)

;; dbus-send --system --print-reply --dest=org.ofono /ril_0 org.ofono.MessageManager.SendMessage string:"+48601789679" string:"A jednak do 20:00"

(defun send-sms (sailfish-host recipient message-text)
  "Send an SMS message composed of MESSAGE-TEXT to
RECIPIENT (specified as a telephone number). The function requires SSH
access to a Sailfish OS device, available at SAILFISH-HOST. It
currently only supports synchronous command execution, which is a
limitation of the underlying INFERIOR-SHELL."
  (run/ss (format nil "dbus-send --system --print-reply \\
--dest=org.ofono /ril_0 org.ofono.MessageManager.SendMessage \\
string:\"~a\" \\
string:\"~a\"" recipient message-text)
          :host sailfish-host))

