(in-package :ninja-sphere)

(defclass game-char ()
  ((jump-force :initform nil)
   (set-move :initform nil)
   (body :initform nil)
   (pos :initform nil)
   (pos16 :initform (gk-vec2 0 0))

   (state :initform nil)
   (actions :initform nil)
   (collide-count :initform (make-array 6 :element-type 'fixnum :initial-element 0))
   (fix-update :initform nil)

   (sprite :initform nil)
   (sprite-anims :initform nil)

   (left-mobs :initform nil)
   (right-mobs :initform nil)

   (facing :initform :right :reader char-facing)
   (motion :initform +motion-none+)
   (motion-mask :initform 0)
   (runp :initform nil)))

(defmethod initialize-instance :after ((c game-char) &key world start &allow-other-keys)
  (with-slots (sprite sprite-anims
               jump-force set-move body pos fix-update) c
    (setf body (make-b2-body c))

    (setf jump-force (cmd-b2-linear-impulse body (gk-vec2 0 0.0) (gk-vec2 *f* 0)))
    (setf set-move (cmd-b2-set-velocity body (gk-vec2 0 0) 0))

    (setf fix-update (cmd-b2-fixture-update body 0 :elasticity 0.0 :friction 0.0))

    (setf sprite (make-instance 'sprite
                   :sheet (asset-sheet *assets*)
                   :name "ninja/idle_1.png"
                   :pos (gk-vec2 16 16)
                   :size (gk-vec2 1.0 1.0)))

    (setf sprite-anims
          (make-instance 'sprite-anim-set
            :sprite sprite
            :animation-list
            (list
             (list :idle "ninja/idle")
             (list :walk "ninja/run" :frame-length (/ 64.0 1000))
             (list :run "ninja/run" :frame-length (/ 56.0 1000))
             (list :attack "ninja/attack"
                   :count 1
                   :frame-length (/ 64.0 1000)
                   :on-stop (lambda (s)
                              (on-stop c :attack s)))
             (list :ball "ninja-sphere/ninja-sphere"))))

    (sprite-anim-set-play sprite-anims :idle)

    (setf (b2-body-position body) start
          pos start)

    (let* ((bundle (make-instance 'bundle))
           (list (make-instance 'cmd-list-b2))
           (bodydef (b2-bodydef body :dynamic
                                :position (gk-vec2 (f* (vx start)) (f* (vy start)))
                                :fixed-rotation-p t))
           (body-create (cmd-b2-body-create world bodydef))

           (x 0.0)
           (y 0.0)
           (r (* *f* 0.3))
           (tr (* *f* 0.1))
           (fixture-create
             (cmd-b2-fixture-create
              body
              (list
               :begin
               :circle x y r
               :friction 0.2
               :density 60.0
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
               :fill

               :begin
               :circle (- x (* 2 r)) y (* 2 tr)
               :sensor :fixture-id 4.0
               :fill

               :begin
               :circle (+ x (* 2 r)) y (* 2 tr)
               :sensor :fixture-id 5.0
               :fill
               ))))
      (bundle-append bundle list)
      (cmd-list-append list body-create fixture-create)
      (gk:process *gk* bundle))))

(defgeneric on-stop (o key state))

(defmethod on-stop ((o game-char) (key (eql :attack)) s)
  (setf (state o) :falling))

(defmethod on-ground-p ((gc game-char))
  (with-slots (collide-count) gc
    (> (aref collide-count 1) 0)))

(defun side-hit (gc)
  (with-slots (collide-count) gc
    (cond
      ((> (aref collide-count 2) 0) :left)
      ((> (aref collide-count 3) 0) :right))))

(defun crushed-p (gc)
  (with-slots (collide-count) gc
    (and (> (aref collide-count 2) 0)
         (> (aref collide-count 3) 0))))

(defmethod char-action ((gc game-char) action)
  (with-slots (actions) gc
    (push action actions)))

(defmethod (setf char-facing) (v (gc game-char))
  (with-slots (sprite facing) gc
    (setf facing v)
    (case v
      (:left  (setf (sprite-scale sprite) (gk-vec3 -1.0 1.0 1.0)))
      (:right (setf (sprite-scale sprite) (gk-vec3  1.0 1.0 1.0))))))

(defmethod change-state ((o game-char) (s (eql :walking)) from-state)
  (with-slots (sprite-anims) o
    (sprite-anim-set-play sprite-anims :idle)))

(defmethod change-state ((o game-char) (s (eql :jumping)) (from-state (eql :walking)))
  (with-slots (jump-force body) o
    (setf (vy (b2-linear-impulse jump-force)) 4.5
          (vx (b2-linear-impulse jump-force)) 0.0
          (b2-linear-impulse-point jump-force) (gk-vec2 0 0))
    (incf (vx (b2-linear-impulse-point jump-force)))))

(defmethod change-state ((o game-char) (s (eql :falling)) from-state)
  (with-slots (sprite-anims) o
    (sprite-anim-set-play sprite-anims :idle)))

(defmethod change-state ((o game-char) (s (eql :ball)) from-state)
  (with-slots (fix-update sprite-anims sprite) o
    (setf (sprite-scale sprite) (gk-vec3 1.0 1.0 1.0))
    (setf (b2-fixture-update-elasticity fix-update) 0.9
          (b2-fixture-update-friction fix-update) 0.0)
    (sprite-anim-set-play sprite-anims :ball)))

(defmethod change-state :before ((o game-char) to-state (from-state (eql :ball)))
  (with-slots (fix-update sprite-anims) o
    (setf (b2-fixture-update-elasticity fix-update) 0.0
          (b2-fixture-update-friction fix-update) 0.2)))

(defmethod change-state ((o game-char) (s (eql :bounce)) from-state)
  (with-slots (jump-force) o
    (let ((side (side-hit o)))
      (setf (vy (b2-linear-impulse jump-force)) 2.2
            (vx (b2-linear-impulse jump-force)) (if (eql side :left) -1.7 1.7)))))

(defmethod change-state ((o game-char) (s (eql :bounce-fall)) from-state))

(defmethod change-state ((o game-char) (s (eql :attack)) from-state)
  (with-slots (sprite-anims set-move left-mobs right-mobs facing) o
    (sprite-anim-set-play sprite-anims :attack)
    (when (eql from-state :walking)
      (setf (vx (b2-velocity-linear set-move)) 0.0))))

(defmethod run-state ((o game-char) (s null))
  (if (on-ground-p o)
      (setf (state o) :walking)
      (setf (state o) :falling)))

(defmethod run-state ((o game-char) (s (eql :falling)))
  (with-slots (body motion set-move sprite) o
    (unless (zerop (vx motion))
      (setf (vx (b2-velocity-linear set-move))
            (clamp (+ (vx (b2-velocity-linear set-move))
                      (* #++ 0.7 0.3 (vx motion)))
                   -1.5 1.5)))

    (let ((v (vx (b2-velocity-linear set-move))))
      (if (< v -0.1) (setf (char-facing o) :left))
      (if (> v  0.1) (setf (char-facing o) :right))))

  (cond
    ((on-ground-p o)
     (setf (state o) :walking))
    ((action-is o :attack)
     (setf (state o) :attack))
    ((action-is o :ball)
     (setf (state o) :ball))))

(defmethod run-state ((o game-char) (s (eql :walking)))
  (with-slots (motion set-move sprite sprite-anims runp) o
    (setf (b2-velocity-linear set-move) motion)
    (nv2* (b2-velocity-linear set-move) (if runp 2.0 1.5))

    (let ((v (vx (b2-velocity-linear set-move))))
      (if (< v -0.1) (setf (char-facing o) :left))
      (if (> v  0.1) (setf (char-facing o) :right))

      (if (> (abs v) 0.1)
          (if runp
              (sprite-anim-set-play sprite-anims :run)
              (sprite-anim-set-play sprite-anims :walk))
          (sprite-anim-set-play sprite-anims :idle))))

  (cond
    ((action-is o :attack)
     (setf (state o) :attack))
    ((action-is o :jump)
     (setf (state o) :jumping))
    ((not (on-ground-p o))
     (setf (state o) :falling))
    ((action-is o :ball)
     (setf (state o) :ball))))

(defmethod run-state ((o game-char) (s (eql :jumping)))
  (when (not (on-ground-p o))
    (setf (state o) :falling)))

(defmethod run-state ((o game-char) (s (eql :ball)))
  (with-slots (motion set-move runp) o
    (setf (vx (b2-velocity-linear set-move))
          (* (vx motion) (if runp 2.2 1.5))))

  (cond
    ((action-is o :stand)
     (setf (state o) :falling))

    ;; If the player's jumping thta's allowed
    ((and (on-ground-p o)
          (action-is o :jump))
     (with-slots (jump-force) o
       (setf (vy (b2-linear-impulse jump-force)) 4.0)))

    ;; If we hit a wall
    ((side-hit o)
     (setf (state o) :bounce))))


(defmethod run-state ((o game-char) (s (eql :bounce)))
  (cond
    ((action-is o :stand)
     (setf (state o) :falling))
    ((not (side-hit o))
     (setf (state o) :bounce-fall))))

(defmethod run-state ((o game-char) (s (eql :bounce-fall)))
  (cond
    ((action-is o :stand)
     (setf (state o) :falling))
    ((side-hit o)
     (setf (state o) :bounce))))

(defmethod run-state ((o game-char) (s (eql :attack)))
  (with-slots (facing left-mobs collide-count right-mobs) o
    (case facing
      (:left
       (mapcar (lambda (x) (attack-mob o x)) left-mobs)
       (setf left-mobs nil))
      (:right
       (mapcar (lambda (x) (attack-mob o x)) right-mobs)
       (setf right-mobs nil)))))

;;; FIXME: Pretty inefficient but we should only have a couple actions per frame
(defun action-is (gc &rest actions)
  (with-slots ((a actions)) gc
    (loop for action in actions
          do (when-let (found (find action a))
               (return found)))))

(defmethod physics ((gc game-char) lists)
  (with-slots (jump-force body set-move
               collide-count actions fix-update runp) gc
    (setf (b2-velocity-linear set-move) (b2-body-velocity body))

    (if (action-is gc :run)
        (setf runp t)
        (when (action-is gc :slow)
          (setf runp nil)))

    (do-state gc)
    (with-slots (phys-list) lists
      (when-let (action (action-is gc :ball :stand))
        (cmd-list-append phys-list fix-update))
      (cmd-list-append phys-list set-move)
      (cmd-list-append phys-list jump-force))))

(defmethod post-physics ((gc game-char) lists)
  (when (crushed-p gc)
    (:say "CRUSHED!"))
  (with-slots (jump-force) gc
    (setf (vx (b2-linear-impulse jump-force)) 0.0
          (vy (b2-linear-impulse jump-force)) 0.0))
  (with-slots (actions) gc
    (setf actions nil)))

(defmethod draw ((gc game-char) lists m)
  (with-slots (pos body sprite) gc
    (let ((spr sprite))
      (setf (sprite-pos spr) pos)
      (nv2* (sprite-pos spr) *physics-scale*)
      (draw spr lists m))))

(defmethod collide ((a game-char) b id-a id-b)
  (with-slots (collide-count) a
    (when (= 0 id-b)
      (incf (aref collide-count id-a)))))

(defmethod collide ((a game-mob) (b game-char) id-a id-b)
  (collide b a id-b id-a))

(defmethod separate ((a game-mob) (b game-char) id-a id-b)
  (separate b a id-b id-a))

(defmethod collide ((a game-char) (b game-mob) (id-a (eql 0)) id-b)
  (:say "DEATH BY MOB!")
  (call-next-method))

(defmethod collide :before ((a game-char) (b game-mob) (id-a (eql 4)) id-b)
  (with-slots (left-mobs) a
    (push b left-mobs)))

(defmethod collide :before ((a game-char) (b game-mob) (id-a (eql 5)) id-b)
  (with-slots (right-mobs) a
    (push b right-mobs)))

(defmethod separate :before ((a game-char) (b game-mob) (id-a (eql 4)) id-b)
  (with-slots (left-mobs collide-count) a
    (deletef left-mobs b)))

(defmethod separate :before ((a game-char) (b game-mob) (id-a (eql 5)) id-b)
  (with-slots (right-mobs collide-count) a
    (deletef right-mobs b)))

(defmethod separate ((a game-char) b id-a id-b)
  (with-slots (collide-count) a
    (when (= 0 id-b)
      (decf (aref collide-count id-a)))))

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
  (with-slots (motion-mask motion set-move) e
    (setf motion (or (akey motion-mask +motion-mask+)
                     +motion-none+))))

(defun game-char-pos (c)
  (with-slots (pos pos16) c
    (set-vec2 pos16 pos)
    (nv2* pos16 *physics-scale*)
    pos16))


(defun attack-mob (gc mob)
  (declare (ignore gc))
  (die mob))

(defmethod die ((gc game-char)))
