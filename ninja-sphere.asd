(defpackage :ninja-sphere.asdf
  (:use #:cl #:asdf))

(in-package :ninja-sphere.asdf)

(defsystem :ninja-sphere
  :description "Ninja Sphere : Lisp Game Jam 2017 Easy Mode"
  :author "Ryan Pavlik"
  :license "GPL2"
  :version "0.0"

  :depends-on (:alexandria :defpackage-plus :sdl2kit :gamekernel)
  :serial t

  :components
  ((:module #:src
    :pathname "src"
    :components
    ((:file "util.rpav-1")

     (:file "package")
     (:file "util")
     (:file "proto")
     (:file "sprite")
     (:file "image")

     (:file "tilemap")
     (:file "anim")

     (:file "assets")

     (:file "startup")
     (:file "window")))))
