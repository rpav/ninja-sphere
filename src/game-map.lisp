(in-package :ninja-sphere)

(defclass game-map ()
  ((tilemap :initform nil)
   (gktm :initform nil)
   (world :initform (gk:make-b2-world))
   (level-body :initform nil)
   (block-body :initform nil)

   (game-char :initform nil :reader game-map-char)
   (map-start :initform nil)

   (objects :initform nil)
   (bundle :initform (make-instance 'bundle))
   (gk-list :initform (make-instance 'cmd-list-b2))

   (scroll :initform nil)
   (scroll-amt :initform nil)
   (step :initform nil)
   (iter :initform nil)
   (ddraw :initform nil)))

(defmethod initialize-instance :after ((gm game-map) &key map &allow-other-keys)
  (with-slots (tilemap gktm world level-body block-body game-char ddraw scroll step iter map-start) gm
    (setf tilemap (load-tilemap (get-path "assets" "maps" (string+ map ".json"))))
    (setf gktm (make-instance 'gk-tilemap :tilemap tilemap))

    (setf level-body (make-b2-body gm))
    (setf block-body (make-b2-body gm))

    (setf scroll (cmd-b2-body-update block-body (gk-vec2 0 0) 0.0))
    (setf step (cmd-b2-step world))
    (setf iter (cmd-b2-iter-bodies world))

    (setf ddraw (gk:cmd-b2-draw-debug world (gk-vec2 512 288)
                                      :translate (gk-vec2 (f* 0.5) (f* 0.5))
                                      :scale (gk-vec2 (* *physics-scale* 0.5) (* 0.5 *physics-scale*))))

    (game-map-build-physics gm)

    (setf game-char (make-instance 'game-char :world world :start map-start))))

(defvar +m-first-sensor+ 100.0)
(defvar +m-death+ +m-first-sensor+)

(defparameter *m-types*
  `((:death . ,+m-death+)))

(defun game-map-build-physics (gm)
  (with-slots (tilemap world objects bundle gk-list
               level-body block-body map-start) gm
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
                                                      :fill))))
        (bundle-append bundle gk-list)
        (cmd-list-append gk-list world-create body-create fixture-create blockfix-create)
        (gk:process *gk* bundle)
        (cmd-list-clear gk-list))

      (map-tilemap-objects (lambda (o)
                             (let* ((type (make-keyword (string-upcase (aval :type o))))
                                    (x (- (f* (aval :x o) ts) (f* 0.5)))
                                    (y (- (f* (aval :y o) ts) (f* 0.5)))
                                    (w (f* (aval :width o) ts))
                                    (h (f* (aval :height o) ts)))
                               (case type
                                 (:start (setf map-start (gk-vec2 (* ts (aval :x o))
                                                                  (* ts (aval :y o)))))
                                 (otherwise
                                  (push (list :begin
                                              :rect x y w h
                                              :sensor :fixture-id (aval type *m-types*)
                                              :fill)
                                        fixtures)))))
                           tilemap "Areas")
      (map-tilemap-objects (lambda (o)
                             (let* ((type (make-keyword (string-upcase (aval :type o))))
                                    (x (/ (aval :x o) 16.0))
                                    (y (/ (aval :y o) 16.0))
                                    (object (make-instance 'game-item
                                              :type type
                                              :world world
                                              :sprite-id (aval :gid o)
                                              :pos (gk-vec2 x y))))
                               (push object objects)))
                           tilemap "Items"))))

(defmethod physics ((gm game-map) lists)
  (with-slots (game-char scroll step iter) gm
    (physics game-char lists)
    (with-slots (phys-list) lists
      (cmd-list-append phys-list scroll step iter))))

(defmethod post-physics ((gm game-map) lists)
  (with-slots (bundle gk-list game-char objects step) gm
    (when (< (vy (game-char-pos game-char)) 0.0)
      (:say "Death by pit!"))
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
                         (cmd-list-append gk-list (item-remove-cmd o)))
                       (item-remove-p o))
                     objects))
    (gk:process *gk* bundle)
    (cmd-list-clear gk-list)))

(defgeneric map-sensor (map object type)
  (:method ((map game-map) object type)
    (:say "Unimplemented sensor: " type)))

(defmethod collide ((a game-map) b id-a id-b)
  (if (< id-a +m-first-sensor+)
      (collide b a id-b id-a)
      (when (= 0 id-b)
        (map-sensor a b id-a))))

(defmethod separate ((a game-map) b id-a id-b)
  (when (< id-a +m-first-sensor+)
    (separate b a id-b id-a)))

(defmethod draw ((gm game-map) lists m)
  (with-slots (gktm game-char objects ddraw pos) gm
    (draw gktm lists m)
    (draw game-char lists m)

    (loop for o in objects
          do (draw o lists m))

    #++
    (with-slots (phys-list-dd) lists
      (cmd-list-append phys-list-dd ddraw))))


(defun map-scroll (gm amt)
  (with-slots (scroll) gm
    (setf (vx (b2-body-update-translate scroll)) (f* amt (/ 1.0 16.0)))))

(defun game-map-size (m)
  (with-slots (tilemap) m
    (tilemap-size tilemap)))
