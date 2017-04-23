(in-package :ninja-sphere)

(defclass title-screen (screen)
  ((im :initform nil)
   (fstyle :initform nil)
   (text :initform nil)))

(defmethod initialize-instance :after ((g title-screen) &key &allow-other-keys)
  (with-slots (fstyle text im) g
    (multiple-value-bind (w h) (window-size)
      (declare (ignorable w h))
      (setf fstyle
            (cmd-path (list
                       :font-size (/ h 15.0)
                       :font-align (logior gk.ffi:+gk-align-center+
                                           gk.ffi:+gk-align-middle+)
                       :tf-identity
                       :fill-color-rgba 255 255 255 255))

            text
            (cmd-text "Press Z" :x (/ w 2.0) :y (* h 7/8))

            im
            (make-instance 'image
              :tex (asset-title *assets*)
              :size (gk-vec2 512 288)
              :pos (gk-vec2 -8 -8)
              :anchor (gk-vec2 0 0)
              :key 0))
      (ui-add g im))))

(defmethod draw ((g title-screen) lists m)
  (with-slots (text fstyle im) g
    (with-slots (ui-list) lists
      (draw im lists m)
      (cmd-list-append ui-list fstyle text))))

(defmethod key-event ((s title-screen) key state)
  (when (and (eq state :keydown) (eq key :scancode-z))
    (map-change "untitled" "default" t)))
