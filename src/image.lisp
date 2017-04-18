;;; Modified from the example in cl-gamekernel

(in-package :ninja-sphere)

(defclass image ()
  (anchor quad trs))

(defmethod initialize-instance ((s image) &key key (tex 0) anchor size pos)
  (with-slots (quad trs scale) s
    (setf quad (cmd-quad tex :key key))
    (setf trs (cmd-tf-trs :out (quad-tfm quad)
                          :translate pos
                          :scale size))
    (setf (image-anchor s) anchor)))

(defun image-tex (s)
  (quad-tex (slot-value s 'quad)))

(defun (setf image-tex) (v s)
  (setf (quad-tex (slot-value s 'quad)) v))

(defun image-pos (s)
  (tf-trs-translate (slot-value s 'trs)))

(defun (setf image-pos) (v s)
  (setf (tf-trs-translate (slot-value s 'trs)) v))

(defun image-anchor (image)
  (slot-value image 'anchor))

(defun (setf image-anchor) (v image)
  (with-slots (anchor quad) image
    (setf anchor v)
    (let* ((x+ (- 1 (vx v)))
           (x- (vx v))
           (y+ (- 1 (vy v)))
           (y- (vy v))
           (verts (list (gk-quadvert x- y- 0 1 0 1)
                        (gk-quadvert x- y+ 0 1 0 0)
                        (gk-quadvert x+ y- 0 1 1 1)
                        (gk-quadvert x+ y+ 0 1 1 0))))
      (setf (quad-attr quad) verts)))
  v)

(defmethod draw ((image image) lists m)
  (with-slots (pre-list sprite-list) lists
    (with-slots (quad trs) image
      (setf (tf-trs-prior trs) m)
      (cmd-list-append pre-list trs)
      (cmd-list-append sprite-list quad))))
