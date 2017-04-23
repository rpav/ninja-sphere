(in-package :ninja-sphere)

(defclass level-screen (screen)
  ((fstyle :initform nil)
   (fstyle2 :initform nil)
   (level-text :initform nil)
   (lives-text :initform nil)
   (name :initform nil :initarg :name)
   (start :initform nil :initarg :start)
   (anim :initform nil)
   (ninja :initform nil)))

(defmethod initialize-instance :after ((g level-screen) &key name &allow-other-keys)
  (with-slots (fstyle fstyle2 level-text lives-text ninja) g
    (multiple-value-bind (w h) (window-size)
      (declare (ignorable w h))
      (let* ((fsize (/ h 15.0))
             (line (* fsize 0.8)))
        (setf fstyle
              (cmd-path (list
                         :font-size fsize
                         :font-align (logior gk.ffi:+gk-align-center+
                                             gk.ffi:+gk-align-middle+)
                         :tf-identity
                         :fill-color-rgba 255 255 255 255))

              fstyle2
              (cmd-path (list
                         :font-size fsize
                         :font-align (logior gk.ffi:+gk-align-left+
                                             gk.ffi:+gk-align-middle+)))

              ninja
              (make-instance 'sprite
                :pos (gk-vec2 230.0 110.0)
                :name "ninja/run_1.png"
                :key 9999))
        (setf level-text
              (cmd-text (string+ "Level: " name) :x (/ w 2.0) :y (- (/ h 2.0) line))

              lives-text
              (cmd-text (format nil "x ~D" (game-value :lives))
                        :x (/ w 2.0) :y (+ (* 1.7 line) (/ h 2.0))))))))

(defmethod go-live ((g level-screen) &key &allow-other-keys)
  (with-slots (name start anim) g
    (let ((a (animation-instance (make-instance 'anim-delay
                                   :duration 0.7
                                   :function (lambda (o)
                                               (declare (ignore o))
                                               (map-change name start)))
                                 g)))
      (setf anim a)
      (anim-play *anim-manager* a))))

(defmethod draw ((g level-screen) lists m)
  (with-slots (level-text lives-text fstyle fstyle2 ninja) g
    (with-slots (ui-list) lists
      (draw ninja lists m)
      (cmd-list-append ui-list fstyle level-text fstyle2 lives-text))))

(defmethod key-event ((s level-screen) key state)
  (with-slots (name start anim) s
    (when (and (eq state :keydown) (eq key :scancode-z))
      (anim-stop *anim-manager* anim)
      (map-change name start))))
