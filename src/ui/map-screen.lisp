(in-package :ninja-sphere)

(defclass map-screen (screen)
  ((map :initform nil)
   (go :initform nil)
   (char :initform nil)
   (scroll :initform (gk-mat4))
   (bgscroll :initform (make-array 4))
   (delta :initform 1.0)
   (im :initform (make-array 4))
   (scroll-cmd :initform nil)
   (bgscroll-cmd :initform (make-array 4))))

(defmethod initialize-instance :after ((m map-screen) &key &allow-other-keys)
  (with-slots (map char
               im gktm scroll scroll-cmd bgscroll bgscroll-cmd) m
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
        (gk:process *gk* b)

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

    (setf map (make-instance 'game-map :map "untitled"))
    (setf char (game-map-char map))))

(defmethod physics ((s map-screen) lists)
  (with-slots (go map) s
    (when go
      (physics map lists))))

(defmethod post-physics ((s map-screen) lists)
  (with-slots (go map char) s
    (when go
      (post-physics map lists))))

(defmethod draw ((s map-screen) lists m)
  (with-slots (map im scroll scroll-cmd bgscroll bgscroll-cmd delta) s
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

    (draw map lists scroll)))

(defmethod key-event ((s map-screen) key state)
  (with-slots (go map char) s
    (if go
        (if (eq state :keydown)
            (progn
              (case key
                (:scancode-right (set-motion-bit char +motion-right+))
                (:scancode-left (set-motion-bit char +motion-left+))
                (:scancode-down (char-action char :ball))
                (:scancode-z (char-action char :jump))
                (:scancode-a (char-action char :attack))))
            (progn
              (case key
                (:scancode-right (clear-motion-bit char +motion-right+))
                (:scancode-left (clear-motion-bit char +motion-left+))
                (:scancode-up (clear-motion-bit char +motion-up+))
                (:scancode-down (char-action char :stand)))))
        (when (and (eq state :keydown) (eql key :scancode-space))
          (setf go t)))))
