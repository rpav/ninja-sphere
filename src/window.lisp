(in-package :ninja-sphere)

(defclass game-lists ()
  ((pass-list :initform (make-instance 'cmd-list :subsystem :config))
   (phys-list :initform (make-instance 'cmd-list-b2 :prealloc 16))
   (pre-list :initform (make-instance 'cmd-list :prealloc 100 :subsystem :config))
   (bg-list :initform (make-instance 'cmd-list :prealloc 4 :subsystem :gl))
   (sprite-list :initform (make-instance 'cmd-list :prealloc 100 :subsystem :gl))
   (ui-list :initform nil)
   (phys-list-dd :initform (make-instance 'cmd-list-b2 :prealloc 2))))

(defun game-lists-clear (game-lists)
  (with-slots (phys-list pre-list bg-list sprite-list ui-list phys-list-dd)
      game-lists
    (cmd-list-clear phys-list)
    (cmd-list-clear pre-list)
    (cmd-list-clear bg-list)
    (cmd-list-clear sprite-list)
    (cmd-list-clear ui-list)
    (cmd-list-clear phys-list-dd)))

(defclass game-window (kit.sdl2:gl-window)
  (gk assets
   (map :initform nil :accessor game-window-map)
   (map-screen :initform nil :accessor game-window-map-screen)
   (next-map :initform nil)
   (char :initform nil :accessor game-window-char)
   (game-state :initform (make-hash-table))
   (anim-manager :initform (make-instance 'anim-manager))
   (screen :initform nil :accessor game-window-screen)
   (next-screen :initform nil)
   (physics-bundle :initform (make-instance 'bundle))
   (render-bundle :initform (make-instance 'bundle))
   (render-lists :initform (make-instance 'game-lists))))

(defmacro with-game-state ((gamewin) &body body)
  (once-only (gamewin)
    `(let ((*gk* (slot-value ,gamewin 'gk))
           (*assets* (slot-value ,gamewin 'assets))
           (*window* ,gamewin)
           (*lists* (slot-value ,gamewin 'render-lists))
           (*time* (current-time))
           (*anim-manager* (slot-value ,gamewin 'anim-manager))
           (*scale* (/ (kit.sdl2:window-width ,gamewin) 256.0)))
       ,@body)))

(defmethod initialize-instance :after ((win game-window) &key w h &allow-other-keys)
  (with-slots (gk screen assets physics-bundle render-bundle render-lists) win
    (with-slots (pass-list phys-list pre-list bg-list sprite-list ui-list phys-list-dd)
        render-lists
      (setf gk (gk:create :gl3))
      (setf assets (load-assets gk))

      (let ((pre-pass (pass 1))
            (bg-pass (pass 2))
            (sprite-pass (pass 3 :asc))
            (ui-pass (pass 4))
            (phys-pass-dd (pass 5)))
        (setf ui-list (make-instance 'cmd-list-nvg :width w :height h))
        (cmd-list-append pass-list
                         pre-pass
                         bg-pass
                         sprite-pass
                         ui-pass
                         phys-pass-dd)
        (bundle-append physics-bundle
                       phys-list)
        (bundle-append render-bundle
                       pass-list        ; 0
                       pre-list         ; 1
                       bg-list          ; 2
                       sprite-list      ; 3
                       ui-list          ; 4
                       phys-list-dd     ; 5
                       ))
      (sdl2:gl-set-swap-interval 1)
      (setf (kit.sdl2:idle-render win) t)

      (with-game-state (win)
        (game-init)))))

(defmethod kit.sdl2:close-window :before ((w game-window))
  (with-slots (gk) w
    (gk:destroy gk)))

(defmethod kit.sdl2:render ((w game-window))
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit)
  (with-slots (gk assets physics-bundle render-bundle render-lists
               next-screen screen) w
    (with-game-state (w)
      (game-lists-clear render-lists)

      (when next-screen
        (when screen (cleanup screen))
        (setf screen next-screen)
        (setf next-screen nil)
        (go-live screen))

      (when screen
        (physics screen render-lists)
        (gk:process gk physics-bundle)
        (post-physics screen render-lists)

        (anim-update *anim-manager*)
        (draw screen render-lists (asset-proj *assets*))
        (gk:process gk render-bundle)))))

(defgeneric key-event (ob key state) (:method (ob key state)))

(defmethod kit.sdl2:keyboard-event ((window game-window) state ts repeat-p keysym)
  (with-game-state (window)
    (let ((scancode (sdl2:scancode keysym)))
      (when (or (eq :scancode-escape scancode))
        (if build:*binary*
            (progn
              (kit.sdl2:close-window window)
              (kit.sdl2:quit))
            (kit.sdl2:close-window window)))
      (unless repeat-p
        (when-let (screen (current-screen))
          (key-event screen scancode state))))))

(define-start-function run (&key (w 1536) (h 864))
  (static-startup)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)
  (sdl2:gl-set-attr :context-profile-mask 1)
  (sdl2:gl-set-attr :stencil-size 8)
  (make-instance 'game-window :w w :h h))

;;; (run)

 ;; Utility

(defun current-screen ()
  (and *window* (game-window-screen *window*)))

(defun (setf current-screen) (v)
  (with-slots (next-screen) *window*
    (setf next-screen v)))

(defun map-change (name &optional start with-level-screen)
  (if (or (equal name "")
          (equal name "WIN"))
      (setf (current-screen) (make-instance 'win-screen))
      (if with-level-screen
          (setf (current-screen) (make-instance 'level-screen :name name :start start))
          (setf (current-screen) (make-instance 'map-screen :name name :start start)))))

(defun window-size ()
  (kit.sdl2:window-size *window*))

(defun window-width ()
  (kit.sdl2:window-width *window*))

(defun window-height ()
  (kit.sdl2:window-height *window*))

(defun game-value (name)
  (with-slots (game-state) *window*
    (gethash name game-state)))

(defun (setf game-value) (v name)
  (with-slots (game-state) *window*
    (setf (gethash name game-state) v)))

(defun game-init ()
  (setf (game-value :score) 0
        (game-value :lives) 3)
  (setf (current-screen) (make-instance 'title-screen)))

(defun addscore (x)
  (let ((last (game-value :score)))
    (incf (game-value :score) x)
    (when (> (floor (game-value :score) 1000)
             (floor last 1000))
      (incf (game-value :lives)))))
