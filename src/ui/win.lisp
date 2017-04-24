(in-package :ninja-sphere)

(defclass win-screen (screen)
  ((fstyle :initform nil)
   (text :initform nil)
   (ninja :initform nil)))

(defmethod initialize-instance :after ((g win-screen) &key &allow-other-keys)
  (with-slots (fstyle text ninja) g
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
            (cmd-text "Ninja Win!" :x (/ w 2.0) :y (/ h 2.0))

            ninja
            (make-instance 'sprite
              :pos (gk-vec2 (/ 512 2.0) (- (/ 288 2.0) 64))
              :name "ninja/victory_0.png"
              :size (gk-vec2 2.0 2.0)
              :key 9999)))))

(defmethod draw ((g win-screen) lists m)
  (with-slots (text fstyle ninja) g
    (with-slots (ui-list) lists
      (draw ninja lists m)
      (cmd-list-append ui-list fstyle text))))

(defmethod key-event ((s win-screen) key state)
  (when (and (eq state :keydown) (eq key :scancode-z))
    (game-init)))
