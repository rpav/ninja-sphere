(in-package :ninja-sphere)

(defclass game-item ()
  ((sprite :initform nil)
   (body :initform nil)
   (type :initform nil :initarg :type)
   (remove-body :initform nil :reader item-remove-cmd)
   (removep :initform nil :reader item-remove-p)))

(defmethod initialize-instance :after ((g game-item) &key world sprite-id pos &allow-other-keys)
  (with-slots (sprite body remove-body) g
    (setf sprite (make-instance 'sprite
                   :sheet (asset-sheet *assets*)
                   :index sprite-id
                   :pos (gk-vec2 (* (vx pos) 16.0) (* (1+ (vy pos)) 16.0))
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
               :rect (f* -0.5) (f* 0.5) (f* 1.0) (f* 1.0)
               :sensor
               :fill))))
      (bundle-append bundle list)
      (cmd-list-append list body-create fixture-create)
      (gk:process *gk* bundle))))

(defmethod draw ((g game-item) lists m)
  (with-slots (sprite) g
    (draw sprite lists m)))

(defmethod collide ((g game-item) b id-a id-b)
  (with-slots (type removep remove-body) g
    (when (eql 0 id-b)
      (setf removep t))))

(defmethod collide (a (b game-item) id-a id-b)
  (collide b a id-b id-a))
