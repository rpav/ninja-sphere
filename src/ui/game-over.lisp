(in-package :ninja-sphere)

(defclass game-over-screen (screen)
  ((fstyle :initform nil)
   (text :initform nil)))

(defmethod initialize-instance :after ((g game-over-screen) &key &allow-other-keys)
  (with-slots (fstyle text) g
    (multiple-value-bind (w h) (window-size)
      (declare (ignorable w h))
      (setf fstyle
            (cmd-path (list
                       :font-size (/ h 10.0)
                       :font-align (logior gk.ffi:+gk-align-center+
                                           gk.ffi:+gk-align-middle+)
                       :tf-identity
                       :fill-color-rgba 255 255 255 255))

            text
            (cmd-text "Game Over" :x (/ w 2.0) :y (/ h 2.0))))))

(defmethod draw ((g game-over-screen) lists m)
  (with-slots (text fstyle) g
    (with-slots (ui-list) lists
      (cmd-list-append ui-list fstyle text))))

(defmethod key-event ((s game-over-screen) key state)
  (when (eq state :keydown)
    (game-init)))
