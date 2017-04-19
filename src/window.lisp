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
   (char :initform nil :accessor game-window-char)
   (game-state :initform (make-hash-table))
   (anim-manager :initform (make-instance 'anim-manager))
   (screen :initform nil :accessor game-window-screen)
   #++(phase-stack :initform (make-instance 'phase-stack))
   (render-bundle :initform (make-instance 'bundle))
   (render-lists :initform (make-instance 'game-lists))))

(defmacro with-game-state ((gamewin) &body body)
  (once-only (gamewin)
    `(let ((*gk* (slot-value ,gamewin 'gk))
           (*assets* (slot-value ,gamewin 'assets))
           (*window* ,gamewin)
           (*time* (current-time))
           (*anim-manager* (slot-value ,gamewin 'anim-manager))
           (*scale* (/ (kit.sdl2:window-width ,gamewin) 256.0))
           #++(*ps* (slot-value ,gamewin 'phase-stack)))
       ,@body)))

(defmethod initialize-instance :after ((win game-window) &key w h &allow-other-keys)
  (with-slots (gk screen assets render-bundle render-lists) win
    (with-slots (pass-list phys-list pre-list bg-list sprite-list ui-list phys-list-dd)
        render-lists
      (setf gk (gk:create :gl3))
      (setf assets (load-assets gk))

      (with-game-state (win)
        (setf screen (make-instance 'map-screen)))

      (let ((phys-pass (pass 1))
            (pre-pass (pass 2))
            (bg-pass (pass 3))
            (sprite-pass (pass 4 :asc))
            (ui-pass (pass 5))
            (phys-pass-dd (pass 6)))
        (setf ui-list (make-instance 'cmd-list-nvg :width w :height h))
        (cmd-list-append pass-list
                         phys-pass
                         pre-pass
                         bg-pass
                         sprite-pass
                         ui-pass
                         phys-pass-dd)
        (bundle-append render-bundle
                       pass-list        ; 0
                       phys-list        ; 1
                       pre-list         ; 2
                       bg-list          ; 3
                       sprite-list      ; 4
                       ui-list          ; 5
                       phys-list-dd     ; 6
                       ))
      (sdl2:gl-set-swap-interval 1)
      (setf (kit.sdl2:idle-render win) t))))

(defmethod kit.sdl2:close-window :before ((w game-window))
  (with-slots (gk) w
    (gk:destroy gk)))

(defmethod kit.sdl2:render ((w game-window))
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :stencil-buffer-bit)
  (with-slots (gk assets render-bundle render-lists) w
    (game-lists-clear render-lists)
    (with-game-state (w)
      (when-let (screen (current-screen))
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
      #++
      (unless repeat-p
        (when-let (screen (current-screen))
          (key-event screen scancode state))))))

(define-start-function run (&key (w 1280) (h 720))
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
  (setf (game-window-screen *window*) v))

(defun current-map ()
  (and *window* (game-window-map *window*)))

(defun (setf current-map) (v)
  (setf (game-window-map *window*) v))

(defun current-char ()
  (and *window* (game-window-char *window*)))

(defun (setf current-char) (v)
  (setf (game-window-char *window*) v))

(defun map-change (map &optional target)
  #++
  (let ((char (current-char)))
    (setf (current-map)
          (make-instance 'game-map
            :map (get-path "assets" "map" (string+ map ".json"))))
    (multiple-value-bind (map-target props)
        (map-find-start (current-map) target)
      (setf (entity-pos char) map-target)
      (setf (entity-motion char) +motion-none+)
      (map-add (current-map) char)
      (map-update (current-map))
      (when-let (text (aval :text props))
        (show-textbox text)))))

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
