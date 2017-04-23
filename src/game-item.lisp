(in-package :ninja-sphere)

(defclass game-item ()
  ((sprite :initform nil)
   (body :initform nil)
   (name :initform nil :initarg :name :reader game-item-name)
   (type :initform nil :initarg :type :reader game-item-type)
   (properties :initform nil :initarg :properties)
   (removep :initform nil :reader item-remove-p)
   (remove-body :initform nil :reader item-remove-cmd)))

(defmethod initialize-instance :after ((g game-item) &key world sprite-name pos &allow-other-keys)
  (with-slots (sprite body remove-body) g
    (setf sprite (make-instance 'sprite
                   :sheet (asset-sheet *assets*)
                   :name sprite-name
                   :pos (gk-vec2 (* (vx pos) 16.0) (* (vy pos) 16.0))
                   :size (gk-vec2 1.0 1.0)))
    (setf body (make-b2-body g))
    (setf remove-body (cmd-b2-body-destroy world body))

    (let* ((bundle (make-instance 'bundle))
           (list (make-instance 'cmd-list-b2))
           (bodydef (b2-bodydef body :static
                                :position (gk-vec2 (f* (vx pos)) (f* (vy pos)))))
           (body-create (cmd-b2-body-create world bodydef))
           (fixture-create
             (cmd-b2-fixture-create
              body
              (list
               :begin
               :rect (f* -0.5) (f* -0.5) (f* 1.0) (f* 1.0)
               :sensor
               :fill))))
      (bundle-append bundle list)
      (cmd-list-append list body-create fixture-create)
      (gk:process *gk* bundle))))

(defmethod draw ((g game-item) lists m)
  (with-slots (sprite) g
    (draw sprite lists m)))

(defmethod collide ((g game-item) b id-a id-b)
  (with-slots (type remove-body) g
    (when (eql 0 id-b)
      (on-collect (game-item-type g) b g)
      (mark-removal g))))

(defmethod collide (a (b game-item) id-a id-b)
  (collide b a id-b id-a))

(defmethod mark-removal ((g game-item))
  (with-slots (removep) g
    (setf removep t)))

(defmethod remove-body-p ((g game-item)) t)
(defmethod remove-sprite-p ((g game-item)) t)

(defgeneric on-collect (type actor item)
  (:method (type actor item)
    (:say "Collected " type " with no handler")))

(defmethod on-collect ((type (eql :goal)) actor item)
  (goal actor)
  (anim-play *anim-manager*
             (animation-instance (make-instance 'anim-delay
                                   :duration 1.0
                                   :function (lambda (o)
                                               (map-change (game-item-name o))))
                                 item)))

(defgeneric property (object name))
(defmethod property ((o game-item) name)
  (with-slots (properties) o
    (aval name properties)))
