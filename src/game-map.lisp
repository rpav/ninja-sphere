(in-package :ninja-sphere)

(defclass game-map ()
  ((tilemap :initform nil)
   (gktm :initform nil)
   (world :initform (gk:make-b2-world))
   (body :initform (gk:make-b2-body))
   (ddraw :initform nil)))

(defmethod initialize-instance :after ((gm game-map) &key map &allow-other-keys)
  (with-slots (tilemap gktm world ddraw) gm
    (setf tilemap (load-tilemap (get-path "assets" "maps" (string+ map ".json"))))
    (setf gktm (make-instance 'gk-tilemap :tilemap tilemap))
    (setf ddraw (gk:cmd-b2-draw-debug world 512 288 :xscale 1.0 :yscale 1.0))

    (game-map-build-physics gm)))

(defun game-map-build-physics (gm)
  (with-slots (tilemap world body) gm
    (let* ((size (tilemap-size tilemap))
           (fixtures))
      (map-tilemap-tiles (lambda (tile x y key)
                           (declare (ignore key))
                           (when tile
                             (let* ((f 16.0)
                                    (x0 (- (* f x) (/ f 2.0)))
                                    (y0 (- (* f y) (/ f 2.0))))
                               (push (list :begin
                                           :rect x0 y0 f f
                                           :fill)
                                     fixtures))))
                         tilemap "Solid")
      (let* ((bundle (make-instance 'bundle))
             (list (make-instance 'cmd-list-b2))
             (world-create (cmd-b2-world-create world :gravity (gk-vec2 0.0 -9.8)))
             (bodydef (b2-bodydef body :static))
             (body-create (cmd-b2-body-create world bodydef))
             (fixture-create (cmd-b2-fixture-create body
                                                    (apply 'concatenate 'list fixtures))))
        (bundle-append bundle list)
        (cmd-list-append list world-create body-create fixture-create)
        (gk:process *gk* bundle)))))

(defmethod draw ((gm game-map) lists m)
  (with-slots (gktm ddraw) gm
    (draw gktm lists m)
    (with-slots (phys-list-dd) lists
      (cmd-list-append phys-list-dd ddraw))))
