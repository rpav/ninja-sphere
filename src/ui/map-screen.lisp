(in-package :ninja-sphere)

(defclass map-screen (screen)
  ((go :initform t)
   (map :initform nil)
   (hud :initform nil)
   (charpos :initform (gk-vec2 0 0))
   (scroll :initform (gk-mat4))
   (bgscroll :initform (make-array 5))
   (delta :initform 1.0)
   (im :initform (make-array 5))
   (scroll-cmd :initform nil)
   (bgscroll-cmd :initform (make-array 5))))

(defmethod initialize-instance :after ((m map-screen) &key name start &allow-other-keys)
  (with-slots (im gktm scroll scroll-cmd bgscroll bgscroll-cmd hud map) m
    (let ((ortho (asset-proj *assets*)))
      (loop for i from 0 below (length bgscroll)
            do (setf (aref bgscroll i) (gk-mat4)
                     (aref bgscroll-cmd i) (cmd-tf-trs :prior ortho
                                                       :out (aref bgscroll i))))
      (setf scroll-cmd (cmd-tf-trs :prior ortho
                                   :translate (gk-vec2 0 0)
                                   :out scroll)))

    (setf map (make-instance 'game-map :name name :start start))
    (setf hud (make-instance 'map-hud :map map))

    (with-bundle (b)
      (let* ((list (make-instance 'cmd-list :subsystem :config))
             (cmds (make-array (length im)))
             (bg-name (or (map-bg map) "parallax-forest")))
        (loop for i from 0 below (length im)
              do (let ((cmd (cmd-image-create (get-path "assets" "images" bg-name (string+ (princ-to-string i) ".png"))
                                              :flags '(:repeatx)
                                              :min :linear :mag :nearest)))
                   (setf (aref cmds i) cmd)
                   (cmd-list-append list cmd)))
        (bundle-append b list)
        (gk:process *gk* b)

        (loop for i from 0 below (length im)
              do (setf (aref im i)
                       (make-instance 'image
                         :pos (gk-vec3 -8 (- -288 8) 0)
                         :tex (image-create-id (aref cmds i))
                         :size (gk-vec3 (* 3 512) (* 3 288) 1.0)
                         :anchor (gk-vec2 0 0)
                         :uvrange #(-1.0 2.0))))))))

(defmethod go-live ((o map-screen) &key &allow-other-keys)
  (with-slots (map hud) o
    (go-live map)
    (go-live hud)))

(defmethod physics ((s map-screen) lists)
  (with-slots (map go) s
    (when go
      (physics map lists))))

(defmethod post-physics ((s map-screen) lists)
  (with-slots (map go) s
    (when go
      (post-physics map lists))))

(defmethod draw ((s map-screen) lists m)
  (with-slots (im scroll scroll-cmd go hud
               map bgscroll bgscroll-cmd delta) s
    (when go
      (with-slots (game-char) map
        (let ((charpos (game-char-pos game-char))
              (mapsize (game-map-size map)))

          (with-slots (pre-list) lists
            (let* ((v (abs (vx (tf-trs-translate scroll-cmd))))
                   (screen-x (+ (abs v) 512))
                   (cx (- screen-x (vx charpos)))
                   (margin 200)
                   (sx (abs (vx (tf-trs-translate scroll-cmd)))))

              (when (< cx margin)
                (incf (vx (tf-trs-translate scroll-cmd)) (- cx margin))
                (setf (vx (tf-trs-translate scroll-cmd))
                      (clamp (vx (tf-trs-translate scroll-cmd)) (+ 512.0 (- (* 16.0 (vx mapsize)))) 0)))

              (map-scroll map sx)
              (cmd-list-append pre-list scroll-cmd)

              (loop for i from 0 below (length bgscroll-cmd)
                    do (setf (vx (tf-trs-translate (aref bgscroll-cmd i))) (* 0.1 i (- sx)))
                       (cmd-list-append pre-list (aref bgscroll-cmd i))))))))

    (loop for i from 0 below (length im)
          do (draw (aref im i) lists (aref bgscroll i)))

    (draw map lists scroll)
    (draw hud lists m)))

(defmethod key-event ((s map-screen) key state)
  (let ((char (game-value :char)))
    (with-slots (go) s
      (if go
          (if (eq state :keydown)
              (progn
                (case key
                  ((:scancode-l :scancode-right) (set-motion-bit char +motion-right+))
                  ((:scancode-j :scancode-left) (set-motion-bit char +motion-left+))
                  ((:scancode-k :scancode-down) (char-action char :ball))
                  ((:scancode-i :scancode-up)
                   (when-let (name (game-value :door))
                     (unless (game-value :door-blocked)
                       (map-change name (game-value :start)))))
                  (:scancode-x (char-action char :jump))
                  (:scancode-z
                   (char-action char :attack)
                   (char-action char :run))))
              (progn
                (case key
                  ((:scancode-l :scancode-right) (clear-motion-bit char +motion-right+))
                  ((:scancode-j :scancode-left) (clear-motion-bit char +motion-left+))
                  ((:scancode-k :scancode-down) (char-action char :stand))
                  (:scancode-z (char-action char :slow)))))
          (when (and (eq key :scancode-z) (eq state :keydown))
            (setf go t))))))

(defmethod cleanup ((s map-screen))
  (with-slots (go map) s
    (cleanup map)
    (setf go nil)))
