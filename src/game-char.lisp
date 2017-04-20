(in-package :ninja-sphere)

(defclass game-char ()
  ((jump-force :initform nil)
   (move-force :initform nil)
   (set-move :initform nil)
   (body :initform nil)
   (pos :initform (gk-vec2 0 0))

   (state :initform nil)
   (actions :initform nil)
   (collide-count :initform (make-array 4 :element-type 'fixnum :initial-element 0))
   (fix-update :initform nil)

   (sprite :initform nil)
   (ballsprite :initform nil)

   (motion :initform +motion-none+)
   (motion-mask :initform 0)))

(defmethod initialize-instance :after ((c game-char) &key world &allow-other-keys)
  (with-slots (sprite ballsprite
               jump-force move-force set-move body pos fix-update) c
    (setf body (make-b2-body c))

    (setf move-force (cmd-b2-force body (gk-vec2 0 0) (gk-vec2 0 0)))
    (setf jump-force (cmd-b2-linear-impulse body (gk-vec2 0 0.0) (gk-vec2 *f* 0)))
    (setf set-move (cmd-b2-set-velocity body (gk-vec2 0 0) 0))

    (setf fix-update (cmd-b2-fixture-update body 0 :elasticity 0.0))

    (setf sprite (make-instance 'sprite
                   :sheet (asset-sheet *assets*)
                   :name "ninja/idle_1.png"
                   :pos (gk-vec2 16 16)
                   :key 50))
    (setf ballsprite (make-instance 'sprite
                       :sheet (asset-sheet *assets*)
                       :name "ninja-sphere/ninja-sphere_001.png"
                       :pos (gk-vec2 16 16)
                       :key 50))

    (setf (b2-body-position body) pos)

    (let* ((bundle (make-instance 'bundle))
           (list (make-instance 'cmd-list-b2))
           (bodydef (b2-bodydef body :dynamic
                                :position (gk-vec2 1.0 3.0) :fixed-rotation-p t))
           (body-create (cmd-b2-body-create world bodydef))

           (x 0.0 #++(* *f* 1.0))
           (y 0.0 #++(* *f* 3.0))
           (r (* *f* 0.3))
           (tr (* *f* 0.1))
           (fixture-create (cmd-b2-fixture-create body
                                                  (list
                                                   :begin
                                                   :circle x y r
                                                   :friction 0.0
                                                   :fill

                                                   :begin
                                                   :circle x (- y r) tr
                                                   :sensor
                                                   :fixture-id 1.0
                                                   :fill

                                                   :begin
                                                   :circle (- x r) y tr
                                                   :sensor :fixture-id 2.0
                                                   :fill

                                                   :begin
                                                   :circle (+ x r) y tr
                                                   :sensor :fixture-id 3.0
                                                   :fill))))
      (bundle-append bundle list)
      (cmd-list-append list body-create fixture-create)
      (gk:process *gk* bundle))))

(defmethod on-ground-p ((gc game-char))
  (with-slots (collide-count) gc
    (> (aref collide-count 1) 0)))

(defun side-hit (gc)
  (with-slots (collide-count) gc
    (cond
      ((> (aref collide-count 2) 0) :left)
      ((> (aref collide-count 3) 0) :right))))

(defmethod char-jump ((gc game-char))
  (with-slots (actions) gc
    (push :jump actions)))

(defmethod change-state ((o game-char) (s (eql :walking)) from-state))
(defmethod change-state ((o game-char) (s (eql :jumping)) (from-state (eql :walking)))
  (with-slots (jump-force) o
    (setf (vy (b2-linear-impulse jump-force)) 4.0)))
(defmethod change-state ((o game-char) (s (eql :falling)) from-state))

(defmethod change-state ((o game-char) (s (eql :ball)) from-state)
  (with-slots (fix-update) o
    (setf (b2-fixture-update-elasticity fix-update) 0.9)))

(defmethod change-state :before ((o game-char) to-state (from-state (eql :ball)))
  (with-slots (fix-update) o
    (setf (b2-fixture-update-elasticity fix-update) 0.0)))

(defmethod change-state ((o game-char) (s (eql :bounce)) from-state)
  (with-slots (jump-force) o
    (let ((side (side-hit o)))
      (setf (vy (b2-linear-impulse jump-force)) 2.0
            (vx (b2-linear-impulse jump-force)) (if (eql side :left) -1.5 1.5)))))


(defmethod run-state ((o game-char) (s null))
  (if (on-ground-p o)
      (setf (state o) :walking)
      (setf (state o) :falling)))

(defmethod run-state ((o game-char) (s (eql :falling)))
  (with-slots (body motion set-move) o
    (if (zerop (vx motion))
        (if (plusp (vx (b2-velocity-linear set-move)))
            (incf (vx (b2-velocity-linear set-move)) -0.1)
            (incf (vx (b2-velocity-linear set-move)) 0.1))
        (setf (vx (b2-velocity-linear set-move))
              (clamp (+ (vx (b2-velocity-linear set-move))
                        (* 0.7 (vx motion)))
                     -1.5 1.5))))

  (when (on-ground-p o)
    (setf (state o) :walking))
  (when (action-is o :ball)
    (setf (state o) :ball)))

(defmethod run-state ((o game-char) (s (eql :walking)))
  (with-slots (motion set-move) o
    (setf (b2-velocity-linear set-move) motion)
    (nv2* (b2-velocity-linear set-move) 1.5))

  (when (action-is o :jump)
    (setf (state o) :jumping))
  (when (not (on-ground-p o))
    (setf (state o) :falling))
  (when (action-is o :ball)
    (setf (state o) :ball)))

(defmethod run-state ((o game-char) (s (eql :jumping)))
  (when (not (on-ground-p o))
    (setf (state o) :falling)))

(defmethod run-state ((o game-char) (s (eql :ball)))
  (with-slots (motion set-move) o
    (setf (vx (b2-velocity-linear set-move))
          (* 1.5 (vx motion))))

  (when (action-is o :stand)
    (setf (state o) :falling))

  ;; If the player's jumping thta's allowed
  (when (and (on-ground-p o)
             (action-is o :jump))
    (with-slots (jump-force) o
      (setf (vy (b2-linear-impulse jump-force)) 4.0)))

  ;; If we hit a wall
  (when (side-hit o)
    (setf (state o) :bounce)))


(defmethod run-state ((o game-char) (s (eql :bounce)))
  (if (action-is o :stand)
      (setf (state o) :falling)))


;;; FIXME: Pretty inefficient but we should only have a couple actions per frame
(defun action-is (gc &rest actions)
  (with-slots ((a actions)) gc
    (loop for action in actions
          do (when-let (found (find action a))
               (return found)))))

(defmethod physics ((gc game-char) lists)
  (with-slots (jump-force move-force body set-move
               actions fix-update) gc
    (setf (b2-velocity-linear set-move) (b2-body-velocity body))
    (do-state gc)
    (with-slots (phys-list) lists
      (when-let (action (action-is gc :ball :stand))
        (cmd-list-append phys-list fix-update))
      (cmd-list-append phys-list set-move)
      (cmd-list-append phys-list jump-force))))

(defmethod post-physics ((gc game-char) lists)
  (with-slots (collide-count) gc)
  (with-slots (jump-force) gc
    (setf (vx (b2-linear-impulse jump-force)) 0.0
          (vy (b2-linear-impulse jump-force)) 0.0))
  (with-slots (actions) gc
    (setf actions nil)))

(defmethod draw ((gc game-char) lists m)
  (with-slots (pos body sprite ballsprite) gc
    (let ((spr (if (state-is gc :ball :bounce) ballsprite sprite)))
      (setf (sprite-pos spr) pos)
      (nv2* (sprite-pos spr) *physics-scale*)
      (draw spr lists m))))

(defmethod collide ((a game-char) b id-a id-b)
  (with-slots (collide-count) a
    (incf (aref collide-count id-a))))

(defmethod separate ((a game-char) b id-a id-b)
  (with-slots (collide-count) a
    (decf (aref collide-count id-a))))

(defun set-motion-bit (e direction)
  (with-slots (motion-mask) e
    (let ((mask (aval direction +motion-mask+)))
      (setf motion-mask (logior motion-mask mask)))
    (game-char-update-motion e)))

(defun clear-motion-bit (e direction)
  (with-slots (motion-mask) e
    (let ((mask (aval direction +motion-mask+)))
      (setf motion-mask (logandc2 motion-mask mask)))
    (game-char-update-motion e)))

(defun game-char-update-motion (e)
  (with-slots (motion-mask motion move-force set-move) e
    (setf motion (or (akey motion-mask +motion-mask+)
                     +motion-none+))))

(defun char-set-ball (gc ballp)
  (with-slots (actions) gc
    (if ballp
        (push :ball actions)
        (push :stand actions))))
