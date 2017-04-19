(in-package :ninja-sphere)

(defclass map-screen (screen)
  ((map :initform nil)
   (gktm :initform nil)))

(defmethod initialize-instance :after ((m map-screen) &key &allow-other-keys)
  (with-slots (map gktm) m
    (setf map (load-tilemap (get-path "assets" "maps" "untitled.json")))
    (setf gktm (make-instance 'gk-tilemap :tilemap map))))

(defmethod draw ((s map-screen) lists m)
  (with-slots (gktm) s
    (draw gktm lists m)))
