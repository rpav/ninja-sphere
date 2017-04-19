(in-package :ninja-sphere)

 ;; UI

(defclass ui ()
  ((visible :initform nil :accessor ui-visible)
   (children :initform (make-hash-table))))

(defmethod ui-add ((ui ui) &rest children)
  (with-slots ((c children)) ui
    (loop for child in children
          do (setf (gethash child c) t))))

(defmethod ui-remove ((ui ui) &rest children)
  (with-slots ((c children)) ui
    (loop for child in children
          do (remhash child c))))

(defmethod ui-clear ((ui ui))
  (with-slots ((c children)) ui
    (clrhash c)))

(defmethod draw ((ui ui) lists m)
  (with-slots ((c children)) ui
    (loop for child being each hash-key in c
          do (draw child lists m))))

 ;; SCREEN

(defclass screen (ui) ())

(defmethod screen-opening ((s screen)))
(defmethod screen-closing ((s screen)))

(defmethod (setf ui-visible) (v (s screen))
  (call-next-method)
  (if v
      (unless (and (current-screen) (eq s (current-screen)))
        (screen-opening s)
        (setf (current-screen) s))
      (when (and (current-screen) (eq s (current-screen)))
        (screen-closing s)
        (setf (current-screen) nil))))
