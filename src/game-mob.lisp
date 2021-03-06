(in-package :ninja-sphere)

(defclass game-mob ()
  ((properties :initform nil :initarg :properties)

   (sprite :initform nil)
   (sprite-anims :initform nil)
   (pos :initform nil)
   (removep :initform nil :reader item-remove-p)
   (remove-body :initform nil :reader item-remove-cmd)

   (body :initform nil)
   (deadp :initform nil)

   (jump-force :initform nil)
   (set-move :initform nil)))

(defmethod property ((o game-mob) name)
  (with-slots (properties) o
    (aval name properties)))

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
                   :pos (gk-vec2 (* 16.0 (vx start)) (* 16.0 (vy start)))
                   :key 3)
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
    (setf jump-force (cmd-b2-linear-impulse body (gk-vec2 0 0.0) (gk-vec2 0.0 0)))))

(defmethod go-live ((o game-mob) &key world &allow-other-keys)
  (with-slots (body pos) o
    (let* ((bundle (make-instance 'bundle))
           (list (make-instance 'cmd-list-b2))
           (bodydef (b2-bodydef body :dynamic
                                :fixed-rotation-p t
                                :position (gk-vec2 (f* (vx pos)) (f* (vy pos)))))
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

               :begin
               :circle (- r) (f* -0.15) (/ r 2.0)
               :sensor :fixture-id 2.0
               :fill

               :begin
               :circle r (f* -0.15) (/ r 2.0)
               :sensor :fixture-id 3.0
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
  (with-slots (set-move body deadp) g
    (with-slots (phys-list) lists
      (if (not deadp)
          (progn
            (setf (vy (b2-linear-impulse set-move)) (vy (b2-body-velocity body)))
            (cmd-list-append phys-list set-move))
          (when (< (vy (b2-body-position body)) -1.0)
            (mark-removal g))))))

(defmethod mark-removal ((g game-mob))
  (with-slots (removep) g
    (setf removep t)))

(defmethod collide ((a game-mob) (b game-mob) id-a id-b)
  (when (/= (mob-direction a) (mob-direction b))
    (reverse-direction b))
  (reverse-direction a))

(defun mob-direction (a)
  (with-slots (set-move) a
   (float-sign (vx (b2-linear-impulse set-move)))))

(defun mob-bump (a id-a)
  (with-slots (set-move) a
    (let ((vx (vx (b2-linear-impulse set-move))))
      (case id-a
        (2 (when (< vx 0) (reverse-direction a)))
        (3 (when (> vx 0) (reverse-direction a)))))))

(defun reverse-direction (m)
  (with-slots (set-move sprite) m
      (setf (vx (b2-linear-impulse set-move)) (- (vx (b2-linear-impulse set-move)))
            (vx (sprite-scale sprite)) (- (vx (sprite-scale sprite))))))

(defmethod die ((g game-mob) actor)
  (with-slots (sprite sprite-anims deadp set-move jump-force body) g
    (unless deadp
      (setf deadp t)
      (sprite-anim-set-play sprite-anims :death)

      (setf (vx (b2-velocity-linear set-move)) 0.0
            (vy (b2-linear-impulse jump-force)) 15.0)

      (with-bundle (b)
        (let ((list (make-instance 'cmd-list-b2))
              (fixup (cmd-b2-fixture-update body 1 :category 2 :mask #x0 :group 0)))
          (bundle-append b list)
          (cmd-list-append list fixup set-move jump-force)
          (gk:process *gk* b)
          (setf (sprite-key sprite) 1000)))

      #++(mark-removal g)
      (when-let (val (property g :value))
        (addscore val)))))

(defmethod remove-sprite-p ((g game-mob)) t)
