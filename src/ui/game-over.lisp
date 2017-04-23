(in-package :ninja-sphere)

(defclass game-over-screen (screen)
  ((text :initform (cmd-text "Game Over" :x 20 :y 20))))

(defmethod initialize-instance :after ((g game-over-screen) &key &allow-other-keys))

(defmethod draw ((g game-over-screen) lists m)
  (with-slots (text) g
    (with-slots (ui-list) lists
      (cmd-list-append ui-list text))))
