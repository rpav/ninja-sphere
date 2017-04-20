(in-package :ninja-sphere)

(defclass game-map ()
  ((tilemap :initform nil)
   (gktm :initform nil)
   (world :initform (gk:make-b2-world))
   (level-body :initform nil)

   (game-char :initform nil :reader game-map-char)

   (step :initform nil)
   (iter :initform nil)
   (ddraw :initform nil)))

(defmethod initialize-instance :after ((gm game-map) &key map &allow-other-keys)
  (with-slots (tilemap gktm world level-body game-char ddraw step iter) gm
    (setf tilemap (load-tilemap (get-path "assets" "maps" (string+ map ".json"))))
    (setf gktm (make-instance 'gk-tilemap :tilemap tilemap))

    (setf level-body (make-b2-body gm))

    (setf step (cmd-b2-step world))
    (setf iter (cmd-b2-iter-bodies world))

    (setf ddraw (gk:cmd-b2-draw-debug world 512 288 :xscale *physics-scale* :yscale *physics-scale*))

    (game-map-build-physics gm)

    (setf game-char (make-instance 'game-char :world world))))

(defun game-map-build-physics (gm)
  (with-slots (tilemap world level-body char-body) gm
    (let* ((size (tilemap-size tilemap))
           (fixtures))
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
      (let* ((bundle (make-instance 'bundle))
             (list (make-instance 'cmd-list-b2))
             (world-create (cmd-b2-world-create world :gravity (gk-vec2 0.0 -10)))
             (level-bodydef (b2-bodydef level-body :static))
             (body-create (cmd-b2-body-create world level-bodydef))
             (fixture-create (cmd-b2-fixture-create level-body
                                                          (apply 'concatenate 'list fixtures))))
        (bundle-append bundle list)
        (cmd-list-append list world-create body-create fixture-create)
        (gk:process *gk* bundle)))))

(defmethod physics ((gm game-map) lists)
  (with-slots (game-char step iter) gm
    (physics game-char lists)
    (with-slots (phys-list) lists
      (cmd-list-append phys-list step iter))))

(defmethod post-physics ((gm game-map) lists)
  (with-slots (game-char step) gm
    (gk:map-b2-collisions
     (lambda (c a b)
       (plus-c:c-val ((c gk.raw:gk-b2-contact-pair))
         #++
         (:say (c :contact) :br
               "  " a (c :id-a) :br
               "  " b (c :id-b))
         (if (> (c :contact) 0)
             (collide a b (c :id-a) (c :id-b))
             (separate a b (c :id-a) (c :id-b)))))
     step)
    (post-physics game-char lists)))

(defmethod collide ((a game-map) b id-a id-b)
  (collide b a id-b id-a))

(defmethod separate ((a game-map) b id-a id-b)
  (separate b a id-b id-a))

(defmethod draw ((gm game-map) lists m)
  (with-slots (gktm game-char ddraw) gm
    (draw gktm lists m)
    (draw game-char lists m)
    #++
    (with-slots (phys-list-dd) lists
      (cmd-list-append phys-list-dd ddraw))))

