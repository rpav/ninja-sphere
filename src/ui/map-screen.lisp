(in-package :ninja-sphere)

(defclass map-screen (screen)
  ((map :initform nil)
   (scroll :initform (gk-mat4))
   (bgscroll :initform (make-array 4))
   (delta :initform 1.0)
   (im :initform (make-array 4))
   (scroll-cmd :initform nil)
   (bgscroll-cmd :initform (make-array 4))
   (gktm :initform nil)))

(defmethod initialize-instance :after ((m map-screen) &key gk &allow-other-keys)
  (with-slots (map im gktm scroll scroll-cmd bgscroll bgscroll-cmd) m
    (with-bundle (b)
      (let* ((list (make-instance 'cmd-list :subsystem :config))
             (cmds (make-array (length im))))
        (loop for i from 0 below (length im)
              do (let ((cmd (cmd-image-create (get-path "assets" "images" "parallax-forest" (string+ (princ-to-string i) ".png"))
                                              :flags '(:repeatx)
                                              :min :linear :mag :nearest)))
                   (setf (aref cmds i) cmd)
                   (cmd-list-append list cmd)))
        (bundle-append b list)
        (gk:process gk b)

        (loop for i from 0 below (length im)
              do (setf (aref im i)
                       (make-instance 'image
                         :pos (gk-vec3 -512 -288 0)
                         :tex (image-create-id (aref cmds i))
                         :size (gk-vec3 (* 3 512) (* 3 288) 1.0)
                         :anchor (gk-vec2 0 0)
                         :uvrange #(-1.0 2.0))))))

    (let ((ortho (asset-proj *assets*)))
      (loop for i from 0 below (length bgscroll)
            do (setf (aref bgscroll i) (gk-mat4)
                     (aref bgscroll-cmd i) (cmd-tf-trs :prior ortho
                                                       :out (aref bgscroll i))))
      (setf scroll-cmd (cmd-tf-trs :prior ortho :out scroll)))

    (setf map (load-tilemap (get-path "assets" "maps" "untitled.json")))
    (setf gktm (make-instance 'gk-tilemap :tilemap map))))

(defmethod draw ((s map-screen) lists m)
  (with-slots (gktm im scroll scroll-cmd bgscroll bgscroll-cmd delta) s
    (with-slots (pre-list) lists
      (let ((v (vx (tf-trs-translate scroll-cmd))))
        #++
        (if (and (> delta 0) (>= v 128))
            (setf delta -1.0)
            (if (and (< delta 0.0) (<= v -128))
                (setf delta 1.0))))
      #++
      (incf (vx (tf-trs-translate scroll-cmd)) delta)
      (cmd-list-append pre-list scroll-cmd)

      (loop for i from 0 below (length bgscroll-cmd)
            do (setf (vx (tf-trs-translate (aref bgscroll-cmd i))) (* 0.1 (1+ i) 0))
               (cmd-list-append pre-list (aref bgscroll-cmd i))))

    (loop for i from 0 below (length im)
          do (draw (aref im i) lists (aref bgscroll i)))
    (draw gktm lists scroll)))
