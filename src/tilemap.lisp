(in-package :ninja-sphere)

(defparameter *tileset-cache* (make-hash-table :test 'equal))

 ;; TILE, TILESET

(defclass tileset ()
  ((props :initform nil :accessor props)
   (name :initform nil :initarg :name :reader tileset-name)
   (count :initform nil :initarg :count :reader tileset-count)
   (tile-width :initform nil :initarg :tile-width :reader tileset-tile-width)
   (tile-height :initform nil :initarg :tile-height :reader tilset-tileheight)))

(defmethod initialize-instance :after ((tm tileset) &key props)
  (with-slots (tiles (p props)) tm
    (setf p props)))

(defun tileset-tile (ts i)
  (with-slots (name) ts
    (string+ name "/" (format nil "~3,'0D" i) ".png")))

(defun translate-props (props)
  (mapcar
   (lambda (x)
     (cons (make-keyword (string-upcase (car x)))
           (cdr x)))
   props))

(defun tilemap-name-to-key (s)
  (or (parse-integer s :junk-allowed t)
      (make-keyword (string-upcase s))))

(defun load-tileset (path &key reload)
  (let ((oldset (gethash (namestring path) *tileset-cache*)))
    (if (or reload (not oldset))
        (with-open-file (s path)
          (let* ((json:*json-identifier-name-to-lisp* #'identity)
                 (json:*identifier-name-to-key* #'tilemap-name-to-key)
                 (json (json:decode-json s))
                 (ts (make-instance 'tileset
                       :name (aval :name json)
                       :count (aval :tilecount json)
                       :tile-width (aval :tilewidth json)
                       :tile-height (aval :tileheight json)
                       :props (translate-props (aval :properites json)))))
            (setf (gethash (namestring path) *tileset-cache*) ts)
            ts))
        oldset)))

 ;; TILE-MERGESET

(defclass tile-mergeset ()
  ((offsets :initform nil)
   (sets :initform nil)))

(defmethod initialize-instance :after ((tms tile-mergeset) &key sets &allow-other-keys)
  (let ((len (length sets))
        (sets (sort sets
                    (lambda (a b)
                      (< (aval :firstgid a)
                         (aval :firstgid b))))))
    (with-slots (offsets (s sets)) tms
      (setf offsets (make-array len :element-type '(unsigned-byte 16)
                                    :initial-contents
                                    (mapcar (lambda (x) (aval :firstgid x)) sets)))
      (setf s (make-array len
                          :initial-contents
                          (mapcar (lambda (x)
                                    (load-tileset (get-path "assets" "maps"
                                                            (aval :source x))))
                                  sets))))))

(defun tms-find (tms num)
  (if (= num 0)
      nil
      (with-slots (offsets sets) tms
        (loop for i from 0
              as offset = (aref offsets i)
              as next-offset = (and (< (1+ i) (length offsets))
                                    (aref offsets (1+ i)))
              when (or (not next-offset)
                       (< num next-offset))
                do (let ((set (aref sets i)))
                     (return-from tms-find (tileset-tile set (- num offset))))))))

 ;; TILEMAP

(defclass tile-layer ()
  ((props :initform nil :initarg :props :accessor props)
   (tiles :initform nil :reader tile-layer-tiles)))

(defun tile-layer-parse (json)
  (let ((layer (make-instance 'tile-layer
                 :props (aval :properties json)))
        (data (aval :data json)))
    (with-slots (tiles) layer
      (setf tiles (make-array (length data) :element-type '(unsigned-byte 16)
                                            :initial-contents data))
      layer)))

(defclass object-layer ()
  ((props :initform nil :initarg :props :accessor props)
   (objects :initform nil :reader object-layer-objects)
   (names :initform (make-hash-table :test 'equal))))

(defun object-layer-parse (tm json)
  (let* ((layer (make-instance 'object-layer
                  :props (aval :properties json)))
         (names (slot-value layer 'names))
         (objects (aval :objects json))
         (size (tilemap-size tm)))
    (loop for object in objects
          as y-before = (aval :y object)
          do (setf (aval :y object) (- (* 16 (vy size))
                                       (+ (aval :height object)
                                          (aval :y object))))
             (when-let (name (aval :name object))
               (setf (gethash name names) object)))
    (setf (slot-value layer 'objects) objects)
    layer))

(defclass tilemap ()
  ((props :initform nil :reader tilemap-props :initarg :props)
   (size :initform nil :reader tilemap-size :initarg :size)
   (layers :initform nil :reader tilemap-layers)
   (layer-names :initform (make-hash-table :test 'equal))
   (mergeset :initform nil :initarg :mergeset :reader tilemap-mergeset)
   (render-order :initform nil :initarg :render-order :reader tilemap-render-order)))

(defmethod initialize-instance :after ((tm tilemap) &key layercount)
  (with-slots (layers) tm
    (setf layers (make-array layercount))))

(defun load-tilemap (path)
  (with-open-file (s path)
    (let* ((json:*json-identifier-name-to-lisp* #'identity)
           (json:*identifier-name-to-key* #'tilemap-name-to-key)
           (json (json:decode-json s))
           (layers (aval :layers json))
           (mergeset (make-instance 'tile-mergeset
                       :sets (aval :tilesets json)))
           (tm (make-instance 'tilemap
                 :props (aval :properties json)
                 :size (gk-vec2 (aval :width json)
                                (aval :height json))
                 :layercount (length layers)
                 :render-order (make-keyword (string-upcase (aval :renderorder json)))
                 :mergeset mergeset))
           (names (slot-value tm 'layer-names)))
      (loop for layer in layers
            for i from 0
            do (setf (gethash (aval :name layer) names) i
                     (aref (tilemap-layers tm) i)
                     (cond
                       ((aval :data layer) (tile-layer-parse layer))
                       ((aval :objects layer) (object-layer-parse tm layer)))))
      tm)))

(defun tilemap-find-layer (tm name)
  (with-slots (layers layer-names) tm
    (typecase name
      (string (when-let (i (gethash name layer-names))
                (aref layers i)))
      (integer (aref layers name)))))

(defun map-tilemap-tiles (function tm layer)
  (with-slots (size layers mergeset) tm
    (let ((layer (tilemap-find-layer tm layer)))
      (when (typep layer 'tile-layer)
        (with-slots (tiles props) layer
          (loop for idx across tiles
                for i from 0
                as key = (or (aval :layer props) i)
                as tile = (tms-find mergeset idx)
                as x = (truncate (mod i (vx size)))
                as y = (- (vy size) (truncate (/ i (vx size))) 1)
                do (funcall function tile x y key)))))))

(defun map-tilemap-objects (function tm layer)
  (with-slots (size layers) tm
    (let ((layer (tilemap-find-layer tm layer)))
      (when (typep layer 'object-layer)
        (with-slots (objects) layer
          (loop for ob in objects
                do (funcall function ob)))))))

(defun tilemap-find-object (tm layer name)
  (with-slots (layers) tm
    (let ((layer (tilemap-find-layer tm layer)))
      (when (typep layer 'object-layer)
        (gethash name (slot-value layer 'names))))))

(defun tilemap-find-gid (tm gid)
  (when gid
    (with-slots (mergeset) tm
      (tms-find mergeset gid))))

(defun tilemap-property (tm name)
  (with-slots (props) tm
    (aval name props)))

 ;; GK-TILEMAP

(defclass gk-tilemap ()
  ((tilemap :initform nil :initarg :tilemap)
   (sprites :initform (make-array 150 :adjustable t :fill-pointer 0))))

;;; This could in theory be done slightly more ideally, but at great
;;; inconvenience.

(defmethod initialize-instance :after ((gktm gk-tilemap)
                                       &key (sheet (asset-sheet *assets*))
                                       &allow-other-keys)
  (with-slots (tilemap sprites) gktm
    (let ((layer-count (length (tilemap-layers tilemap))))
      (loop for i from 0 below layer-count
            do (map-tilemap-tiles
                (lambda (tile x y key)
                  (when tile
                    (let ((sprite (make-instance 'sprite
                                    :sheet sheet
                                    :key key
                                    :name (tile-image tile)
                                    :pos (gk-vec3 (* 16 x) (* 16 y) 0))))
                      (vector-push-extend sprite sprites))))
                tilemap i)))))

(defmethod draw ((gktm gk-tilemap) lists m)
  (with-slots (sprites) gktm
    (loop for sprite across sprites
          do (draw sprite lists m))))
