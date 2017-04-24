;;; build-it.lisp --- script for making SBCL binaries for Linux and
;;;                   MS Windows using only Free Software (GNU/Linux and
;;;                   Wine)
;;; (C) Copyright 2011-2016 by David O'Toole <dto@xelf.me>
;;; (C) Copyright 2011-2016 by David O'Toole <dto@xelf.me>
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Note, I modified this of course to build spell-and-dagger.

(defpackage :build
  (:use #:cl)
  (:export *name* *system* *binary* *startup*))

(in-package :build)

#-quicklisp (load #P"~/quicklisp/setup.lisp")
(require 'sb-posix)
(setf sb-impl::*default-external-format* :utf-8)

(defun argument (name)
  (let* ((args sb-ext:*posix-argv*)
	 (index (position name args :test 'equal))
	 (value (when (and (numberp index)
			   (< index (length args)))
		  (nth (1+ index) args))))
    value))

(defparameter *name* (argument "--name"))
(defparameter *binary*
  (or (argument "--binary")
      #+win32 (concatenate 'string *name* ".exe")
      #+linux (concatenate 'string *name* ".bin")))
(defparameter *system*
  (or (argument "--system")
      (intern *name* :keyword)))
(ql:quickload *system*)
(defparameter *startup*
  (or (argument "--startup")
      (concatenate 'string (string-upcase *name*) "::" (string-upcase *name*))))

(sb-ext:save-lisp-and-die *binary*
			  :toplevel (lambda ()
                                      (sdl2:make-this-thread-main
                                       (lambda ()
                                         (sb-ext:disable-debugger)
                                         (gk:gk-init)
                                         (funcall (read-from-string *startup*))
                                         0)))
			  :executable t)

