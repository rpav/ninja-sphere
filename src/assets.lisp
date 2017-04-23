(in-package :ninja-sphere)

(defun get-path (&rest dirs)
  (let* ((argv0 (if build:*binary*
                    (first sb-ext:*posix-argv*)
                    (asdf:component-pathname
                     (asdf:find-system :ninja-sphere))))
         (path (merge-pathnames (string-join dirs "/") argv0)))
    (unless (probe-file path)
      (error "File not found: ~A" path))
    (format t "Find ~A~%" path)
    path))

(defclass asset-pack ()
  ((title :reader asset-title)
   (proj :initform (gk-mat4) :reader asset-proj)
   (font :reader asset-font)
   (spritesheet :reader asset-sheet)
   (anims :reader asset-anims)
   (props :reader asset-props)))

(defun load-assets (gk)
  (let ((pack (make-instance 'asset-pack)))
    (with-slots (proj scroll font title spritesheet anims tm props) pack
      (with-bundle (b)
        (let* ((tmp (gk-mat4))
               (config (make-instance 'cmd-list :subsystem :config))
               (ortho (cmd-tf-ortho tmp 0 512 0 288 -10000 10000))
               (shift (cmd-tf-trs :prior tmp :out proj :translate (gk-vec2 8 8)))
               (load-title (cmd-image-create (get-path "assets" "images" "title.png")
                                             :mag :nearest))
               (load-sprites (cmd-spritesheet-create
                              (get-path "assets" "ninja-sphere-spritesheet.json")
                              :gk-ssf-texturepacker-json
                              :flags '(:flip-y)))
               (load-font (cmd-font-create
                           "hardpixel"
                           (get-path "assets" "font" "hardpixel.ttf"))))
          (cmd-list-append config ortho shift load-title load-sprites load-font)
          (bundle-append b config)
          (gk:process gk b)

          (setf font (font-create-id load-font))
          (setf title (image-create-id load-title))
          (setf spritesheet (make-sheet load-sprites))
          (setf anims (make-instance 'sheet-animations :sheet spritesheet))

          (let ((json:*json-identifier-name-to-lisp* #'identity)
                (json:*identifier-name-to-key* #'property-name-to-key))
            (with-open-file (s (get-path "assets" "maps" "ninja-sphere-types.json"))
              (setf props (json:decode-json s)))))))
    pack))
