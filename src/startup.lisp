(in-package :ninja-sphere)

;;; Entity
(defvar +motion-none+)
(defvar +motion-up+)
(defvar +motion-down+)
(defvar +motion-left+)
(defvar +motion-right+)

(defvar +motions+)
(defvar +reverse-motion+)

(defvar +default-box+)

(defvar +motion-mask+)

;;; Static startup initialization

(defun static-startup ()
  (setf +motion-none+  (gk-vec2  0  0))
  (setf +motion-up+    (gk-vec2  0  1))
  (setf +motion-down+  (gk-vec2  0 -1))
  (setf +motion-left+  (gk-vec2 -1  0))
  (setf +motion-right+ (gk-vec2  1  0))

  (setf +motions+ (list +motion-up+ +motion-down+ +motion-left+ +motion-right+))
  (setf +reverse-motion+
        `((,+motion-none+ . ,+motion-none+)
          (,+motion-up+ . ,+motion-down+)
          (,+motion-down+ . ,+motion-up+)
          (,+motion-left+ . ,+motion-right+)
          (,+motion-right+ . ,+motion-left+)))

  (setf +default-box+ (cons (gk-vec2 0 0) (gk-vec2 16 16)))

  (setf +motion-mask+
        `((,+motion-left+  . #b1000)
          (,+motion-right+ . #b0001)
          (,+motion-up+    . #b0100)
          (,+motion-down+  . #b0010))))
