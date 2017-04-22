(in-package :ninja-sphere)

(defclass map-hud ()
  ((text-cmds :initform nil)
   (text-style :initform nil)
   (score-text :initform nil)))

(defmethod initialize-instance :after ((h map-hud) &key &allow-other-keys)
  (with-slots (text-cmds text-style score-text) h
    (multiple-value-bind (w h) (window-size)
      (let* ((fsize (/ h 25.0))
             (m (* *scale* 2.0))
             (halfw (/ w 2.0))
             (quad (* fsize 1.5))
             (line (* fsize 0.8))
             (title (map-title (game-value :map))))
        (setf text-style (cmd-font-style :size fsize)
              score-text (cmd-text "000000" :x m :y (* 2 line)))
        (setf (cmd-text-string score-text)
              (format nil "~6,'0D" 111))
        (appendf text-cmds
                 (list
                  (cmd-text "Score" :x m :y line)
                  score-text
                  (cmd-text "Level" :x (+ m (* w 0.12)) :y line)
                  (cmd-text title :x (+ m (* w 0.12)) :y (* 2 line))))))))

(defmethod draw ((h map-hud) lists m)
  (with-slots (text-cmds text-style) h
    (with-slots (ui-list) lists
      (cmd-list-append ui-list text-style)
      (apply 'cmd-list-append ui-list text-cmds))))