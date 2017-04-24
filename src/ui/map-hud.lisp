(in-package :ninja-sphere)

(defclass map-hud ()
  ((text-cmds :initform nil)
   (text-style :initform nil)
   (text-fg :initform nil)
   (text-drop :initform nil)
   (score-text :initform nil)
   (lives-text :initform nil)
   (last-score :initform 0)
   (last-lives :initform 0)
   (ninja :initform nil)))

(defmethod initialize-instance :after ((h map-hud) &key map &allow-other-keys)
  (with-slots (text-cmds text-style text-fg text-drop score-text lives-text ninja) h
    (multiple-value-bind (w h) (window-size)
      (let* ((fsize (/ h 25.0))
             (m (* *scale* 2.0))
             #++(halfw (/ w 2.0))
             #++(quad (* fsize 1.5))
             (line (* fsize 0.8))
             (title (map-title map)))
        (setf text-style (cmd-font-style :size fsize)
              score-text (cmd-text "000000" :x m :y (* 2 line))
              lives-text (cmd-text (format nil "x~D" (game-value :lives))
                                   :x (+ m (* fsize 6.5)) :y (* 2 line))
              text-fg (cmd-path (list
                                 :fill-color-rgba 255 255 255 255
                                 :tf-identity))
              text-drop (cmd-path (list
                                   :fill-color-rgba 0 0 0 255
                                   :tf-translate (/ *scale* 1.5) (/ *scale* 1.5))))
        (setf ninja (make-instance 'sprite
                      :pos (gk-vec2 64 (- 288 20))
                      :name "ninja/run_1.png"
                      :key 9999))
        (appendf text-cmds
                 (list
                  (cmd-text "Score" :x m :y line)
                  score-text
                  lives-text
                  (cmd-text "Level"               :x (+ m (* w 0.25)) :y line)
                  (cmd-text (or title "?noname?") :x (+ m (* w 0.25)) :y (* 2 line))))))))

(defmethod draw ((h map-hud) lists m)
  (with-slots (text-cmds text-style text-fg text-drop
               score-text lives-text
               last-score last-lives
               ninja) h
    (with-slots (ui-list) lists
      (let ((score (game-value :score))
            (lives (game-value :lives)))
        (unless (eql score last-score)
          (setf (cmd-text-string score-text) (format nil "~6,'0D" score)
                last-score score))
        (unless (eql lives last-lives)
          (setf (cmd-text-string lives-text) (format nil "x~D" lives)
                last-lives lives))
        (cmd-list-append ui-list text-style text-drop)
        (apply 'cmd-list-append ui-list text-cmds)
        (cmd-list-append ui-list text-fg)
        (apply 'cmd-list-append ui-list text-cmds)))
    (draw ninja lists m)))
