(in-package :ninja-sphere)

(defclass game-item (game-senseob)
  ((sprite :initform nil)
   (removep :initform nil :reader item-remove-p)
   (remove-body :initform nil :reader item-remove-cmd)))

(defmethod initialize-instance :after ((g game-item) &key world sprite-name pos &allow-other-keys)
  (with-slots (sprite body remove-body) g
    (setf sprite (make-instance 'sprite
                   :sheet (asset-sheet *assets*)
                   :name sprite-name
                   :pos (gk-vec2 (* (vx pos) 16.0) (* (vy pos) 16.0))
                   :size (gk-vec2 1.0 1.0)))
    (setf remove-body (cmd-b2-body-destroy world body))))

(defmethod draw ((g game-item) lists m)
  (with-slots (sprite) g
    (draw sprite lists m)))

(defmethod collide ((g game-item) b id-a id-b)
  (with-slots (type remove-body) g
    (when (eql 0 id-b)
      (on-collect (game-item-type g) b g)
      (mark-removal g))
    (call-next-method)))

(defmethod mark-removal ((g game-item))
  (with-slots (removep) g
    (setf removep t)))

(defmethod remove-body-p ((g game-item)) t)
(defmethod remove-sprite-p ((g game-item)) t)

(defgeneric on-collect (type actor item)
  (:method (type actor item)
    (:say "Collected " type " with no handler")))

 ;; Item type handling

(defmethod on-collect ((type (eql :goal)) actor item)
  (goal actor)
  (anim-play *anim-manager*
             (animation-instance (make-instance 'anim-delay
                                   :duration 1.0
                                   :function (lambda (o)
                                               (map-change (game-item-name o))))
                                 item)))


(defmethod on-collect ((type (eql :points)) actor item)
  (let ((points (property item :value)))
    (incf (game-value :score) points)))
