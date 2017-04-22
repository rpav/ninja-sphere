(in-package :ninja-sphere)

(defclass game-mob ()
  ((sprite :initform nil)
   (sprite-anims :initform nil)
   (pos :initform nil)
   (removep :initform nil :reader item-remove-p)
   (remove-body :initform nil :reader item-remove-cmd)

   (body :initform nil)
   (deadp :initform nil)

   (jump-force :initform nil)
   (set-move :initform nil)))

(defmethod initialize-instance :after ((g game-mob)
                                       &key
                                       world
                                       sprite-name
                                       start
                                       &allow-other-keys)
  (with-slots (sprite sprite-anims body remove-body
               pos set-move jump-force remove-cmd) g
    (setf sprite (make-instance 'sprite
                   :sheet (asset-sheet *assets*)
                   :name (string+ sprite-name "_idle_0.png")
                   :size (gk-vec2 -1 1)
                   :pos (gk-vec2 (* 16.0 (vx start)) (* 16.0 (vy start))))
          sprite-anims (make-instance 'sprite-anim-set
                         :sprite sprite
                         :animation-list
                         (gethash sprite-name +anims+)))

    (sprite-anim-set-play sprite-anims :walk)

    (setf body (make-b2-body g))
    (setf remove-body (cmd-b2-body-destroy world body))

    (setf (b2-body-position body) start
          pos start)

    (setf set-move (cmd-b2-set-velocity body (gk-vec2 -0.3 0) 0.0))

    (let* ((bundle (make-instance 'bundle))
           (list (make-instance 'cmd-list-b2))
           (bodydef (b2-bodydef body :dynamic
                                :fixed-rotation-p t
                                :position (gk-vec2 (f* (vx start)) (f* (vy start)))))
           (body-create (cmd-b2-body-create world bodydef))
           (r (f* 0.3))
           (fixture-create
             (cmd-b2-fixture-create
              body
              (list
               :begin
               :circle 0.0 (f* -0.15) r
               :friction 0.3
               :density 500.0
               :fixture-id 1.0
               :fill

               ))))
      (bundle-append bundle list)
      (cmd-list-append list body-create fixture-create)
      (gk:process *gk* bundle))))

(defmethod draw ((g game-mob) lists m)
  (with-slots (sprite body pos) g
    (setf (sprite-pos sprite) pos)
    (nv2* (sprite-pos sprite) *physics-scale*)
    (draw sprite lists m)))

(defmethod physics ((g game-mob) lists)
  (with-slots (set-move body) g
    (with-slots (phys-list) lists
      (setf (vy (b2-linear-impulse set-move)) (vy (b2-body-velocity body)))
      (cmd-list-append phys-list set-move))))

(defmethod mark-removal ((g game-mob))
  (with-slots (removep) g
    (setf removep t)))

(defmethod die ((g game-mob))
  (with-slots (sprite-anims deadp) g
    (setf deadp t)
    (sprite-anim-set-play sprite-anims :death)
    (mark-removal g)))

(defmethod remove-sprite-p ((g game-mob))
  (with-slots (deadp) g (not deadp)))
