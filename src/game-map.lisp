(in-package :ninja-sphere)

(defclass game-map ()
  ((tilemap :initform nil)
   (name :initform nil :initarg map)
   (gktm :initform nil)
   (world :initform (gk:make-b2-world))
   (level-body :initform nil)
   (block-body :initform nil)

   (game-char :initform nil :reader game-map-char)
   (map-starts :initform (make-hash-table :test 'equal))

   (mobs :initform nil)
   (objects :initform nil)
   (sense-objects :initform nil)
   (dead-objects :initform nil)
   (bundle :initform (make-instance 'bundle))
   (gk-list :initform (make-instance 'cmd-list-b2))

   (scroll :initform nil)
   (scroll-amt :initform nil)
   (step :initform nil)
   (iter :initform nil)
   (ddraw :initform nil)))

(defmethod initialize-instance :after ((gm game-map) &key map start &allow-other-keys)
  (with-slots (tilemap name gktm world level-body block-body game-char ddraw scroll step iter map-starts) gm
    (setf tilemap (load-tilemap (get-path "assets" "maps" (string+ map ".json"))
                                (asset-props *assets*)))
    (setf gktm (make-instance 'gk-tilemap :tilemap tilemap))

    (setf level-body (make-b2-body gm))
    (setf block-body (make-b2-body gm))

    (setf scroll (cmd-b2-body-update block-body (gk-vec2 0 0) 0.0))
    (setf step (cmd-b2-step world))
    (setf iter (cmd-b2-iter-bodies world))

    (let ((scale 1.0))
     (setf ddraw (gk:cmd-b2-draw-debug world (gk-vec2 512 288)
                                       :translate (gk-vec2 (f* 0.5) (f* 0.5))
                                       :scale (gk-vec2 (* *physics-scale* scale)
                                                       (* *physics-scale* scale)))))

    (game-map-build-physics gm)

    (setf game-char (make-instance 'game-char
                      :world world
                      :start (or (gethash (or start "default") map-starts)
                                 (gethash "" map-starts)
                                 (error "No default start for map"))))))

(defmethod go-live ((o game-map) &key &allow-other-keys)
  (with-slots (game-char) o
    (setf (game-value :map) o)
    (setf (game-value :char) game-char)
    (map-scroll o 0)))

(defmethod cleanup ((o game-map))
  (with-slots (world) o
    (let ((bundle (make-instance 'bundle))
          (list (make-instance 'cmd-list-b2))
          (world-destroy (cmd-b2-world-destroy world)))
      (bundle-append bundle list)
      (cmd-list-append list world-destroy)
      (gk:process *gk* bundle))
    (setf (game-value :map) nil)
    (setf (game-value :char) nil)))

(defun game-map-build-physics (gm)
  (with-slots (tilemap world objects sense-objects bundle gk-list
               level-body block-body map-starts mobs) gm
    (let* ((size (tilemap-size tilemap))
           (fixtures
             (list
              (list :begin
                    :rect (f* (- (vx size) 0.5)) (f* -100) 1.0 100.0
                    :fill)))
           (ts (/ 1.0 16.0)))
      (map-tilemap-tiles (lambda (tile x y key)
                           (declare (ignore key))
                           (when tile
                             (let* ((x0 (- (* *f* x) (/ *f* 2.0)))
                                    (y0 (- (* *f* y) (/ *f* 2.0))))
                               (push (list :begin
                                           :rect x0 y0 *f* *f*
                                           :fill)
                                     fixtures))))
                         tilemap "Solid")

      (let* ((world-create (cmd-b2-world-create world :gravity (gk-vec2 0.0 -10)))
             (level-bodydef (b2-bodydef level-body :static))
             (block-bodydef (b2-bodydef block-body :static))
             (body-create (cmd-b2-body-create world level-bodydef block-bodydef))
             (fixture-create (cmd-b2-fixture-create level-body
                                                    (apply 'concatenate 'list fixtures)))
             (blockfix-create (cmd-b2-fixture-create block-body
                                                     (list
                                                      :begin
                                                      :rect (f* -1.5) (f* -100.0) (f* 1.0) 100.0
                                                      :fixture-id 1.0
                                                      :fill))))
        (bundle-append bundle gk-list)
        (cmd-list-append gk-list world-create body-create fixture-create blockfix-create)
        (gk:process *gk* bundle)
        (cmd-list-clear gk-list))

      (map-tilemap-objects (lambda (o)
                             (let* ((type (make-keyword (string-upcase
                                                         (or (aval :type (aval :properties o))
                                                             (aval :type o)))))
                                    (x (/ (aval :x o) 16.0))
                                    (y (/ (aval :y o) 16.0))
                                    (w (f* (aval :width o) ts))
                                    (h (f* (aval :height o) ts)))
                               (case type
                                 (:start (setf (gethash (aval :name o) map-starts)
                                               (gk-vec2 (* ts (aval :x o))
                                                        (* ts (aval :y o)))))
                                 (otherwise
                                  (push (make-instance 'game-senseob
                                          :name (aval :name o)
                                          :type type
                                          :properties (aval :properties o)
                                          :world world
                                          :pos (gk-vec2 x y)
                                          :size (gk-vec2 w h))
                                        sense-objects)))))
                           tilemap "Areas")
      (map-tilemap-objects (lambda (o)
                             (let* ((type (make-keyword (string-upcase (aval :type (aval :properties o)))))
                                    (x (/ (aval :x o) 16.0))
                                    (y (/ (aval :y o) 16.0))
                                    (object (make-instance 'game-item
                                              :name (aval :name o)
                                              :type type
                                              :properties (aval :properties o)
                                              :world world
                                              :sprite-name (tilemap-find-gid tilemap (aval :gid o))
                                              :pos (gk-vec2 x (1+ y)))))
                               (push object objects)))
                           tilemap "Items")
      (map-tilemap-objects (lambda (o)
                             (let* ((type (make-keyword (string-upcase (or (aval :type (aval :properties o))
                                                                           (aval :type o)))))
                                    (x (/ (aval :x o) 16.0))
                                    (y (/ (aval :y o) 16.0))
                                    (object (make-instance 'game-sprob
                                              :name (aval :name o)
                                              :type type
                                              :properties (aval :properties o)
                                              :world world
                                              :sprite-name (tilemap-find-gid tilemap (aval :gid o))
                                              :pos (gk-vec2 x (1+ y)))))
                               (push object objects)))
                           tilemap "Objects")
      (map-tilemap-objects (lambda (o)
                             (let* ((type (aval :type o))
                                    (x (/ (aval :x o) 16.0))
                                    (y (/ (aval :y o) 16.0))
                                    (object (make-instance 'game-mob
                                              :sprite-name type
                                              :world world
                                              :start (gk-vec2 x (1+ y)))))
                               (push (cons x object) mobs)))
                           tilemap "Mobs")
      (setf mobs
            (sort mobs (lambda (a b) (< (car a) (car b))))))))

(defmethod physics ((gm game-map) lists)
  (with-slots (game-char objects scroll step iter) gm
    (physics game-char lists)
    (loop for o in objects
          do (physics o lists))
    (with-slots (phys-list) lists
      (cmd-list-append phys-list scroll step iter))))

(defmethod post-physics ((gm game-map) lists)
  (with-slots (bundle gk-list game-char objects dead-objects step) gm
    (when (< (vy (game-char-pos game-char)) 0.0)
      (if (deadp game-char)
          (if (>= (game-value :lives) 0)
              (map-change "untitled")
              (setf (current-screen) (make-instance 'game-over-screen)))
          (die game-char gm)))
    (gk:map-b2-collisions
     (lambda (c a b)
       (plus-c:c-val ((c gk.raw:gk-b2-contact-pair))
         (if (> (c :contact) 0)
             (collide a b (c :id-a) (c :id-b))
             (separate a b (c :id-a) (c :id-b)))))
     step)
    (post-physics game-char lists)
    (setf objects
          (delete-if (lambda (o)
                       (when (item-remove-p o)
                         (when-let (cmd (item-remove-cmd o))
                           (cmd-list-append gk-list cmd))
                         (unless (remove-sprite-p o)
                           (push o dead-objects))
                         t))
                     objects))
    (gk:process *gk* bundle)
    (cmd-list-clear gk-list)))

(defgeneric map-sensor (map object type)
  (:method ((map game-map) object type)
    (:say "Unimplemented sensor: " type)))

(defgeneric backwall-hit (map object)
  (:method (map object)))

;;; FIXME: This kinda sucks, should use channels so mobs can just pass
;;; the wall
(defmethod backwall-hit (map (object game-mob))
  (mark-removal object))

(defmethod collide ((a game-map) b id-a id-b)
  (case id-a
    (1 (backwall-hit a b)))
  (collide b a id-b id-a))

(defmethod separate ((a game-map) b id-a id-b)
  (separate b a id-b id-a))

(defmethod draw ((gm game-map) lists m)
  (with-slots (gktm game-char objects dead-objects ddraw pos) gm
    (draw gktm lists m)
    (draw game-char lists m)

    (loop for o in objects
          do (draw o lists m))
    (loop for o in dead-objects
          do (draw o lists m))

    #++
    (with-slots (phys-list-dd) lists
      (cmd-list-append phys-list-dd ddraw))))


(defun map-scroll (gm amt)
  (with-slots (scroll mobs objects world) gm
    (setf (vx (b2-body-update-translate scroll)) (f* amt (/ 1.0 16.0)))
    (let ((x (+ 32.0 (/ (* *physics-scale* (vx (b2-body-update-translate scroll))) 16.0))))
      (loop while (and mobs (< (caar mobs) x))
            do (let ((mob (cdr (pop mobs))))
                 (go-live mob :world world)
                 (push mob objects))))))

(defun game-map-size (m)
  (with-slots (tilemap) m
    (tilemap-size tilemap)))


(defun map-title (m)
  (with-slots (tilemap) m
    (tilemap-property tilemap :title)))
