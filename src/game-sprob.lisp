(in-package :ninja-sphere)

(defclass game-sprob (game-senseob)
  ((sprite :initform nil)
   (state :initform nil :accessor object-state)))

(defmethod initialize-instance :after ((g game-sprob) &key sprite-name pos &allow-other-keys)
  (with-slots (sprite) g
    (setf sprite (make-instance 'sprite
                   :sheet (asset-sheet *assets*)
                   :name sprite-name
                   :pos (gk-vec2 (* (vx pos) 16.0) (* (vy pos) 16.0))
                   :size (gk-vec2 1.0 1.0)))))

(defmethod draw ((g game-sprob) lists m)
  (with-slots (sprite) g
    (draw sprite lists m)))

(defmethod collide ((a game-sprob) b id-a id-b)
  (collide b a id-b id-a)
  (call-next-method))

(defmethod item-remove-p ((o game-sprob)) nil)

(defmethod die ((o game-sprob) actor)
  (on-hit (game-item-type o) actor o))

(defgeneric on-hit (type actor ob)
  (:method (type actor ob)))

 ;; type :hit-to-open

(defmethod on-hit ((type (eql :hit-to-open)) actor o)
  (with-slots (sprite properties state type) o
    (setf (sprite-index sprite)
          (find-frame (asset-sheet *assets*)
                      (property o :alt)))
    (setf type :door)))


