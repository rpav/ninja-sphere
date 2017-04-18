(in-package :cl-user)
(defpackage+-1:defpackage+ :build
  (:use #:cl)
  (:export *name* *system* *binary* *startup*))

(in-package :build)
(defvar *name* nil)
(defvar *system* nil)
(defvar *binary* nil)
(defvar *startup* nil)

(defpackage+-1:defpackage+ :ninja-sphere
  (:use #:cl #:alexandria #:util.rpav-1)
  (:import-except #:gk #:create #:process #:destroy)
  (:import-from #:kit.sdl2 #:define-start-function)
  (:export #:run))

(in-package :ninja-sphere)

 ;; Variables

