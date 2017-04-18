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
   (anims :reader asset-anims)))

(defun load-assets (gk)
  (let ((pack (make-instance 'asset-pack)))
    (with-slots (proj font title spritesheet anims tm) pack
      (with-bundle (b)
        (let* ((config (make-instance 'cmd-list :subsystem :config))
               (ortho (cmd-tf-ortho proj 0 256 0 144 -10000 10000))
               (load-title (cmd-image-create (get-path "assets" "image" "title.png")
                                             :mag :nearest))
               (load-sprites (cmd-spritesheet-create
                              (get-path "assets" "image" "spritesheet.json")
                              :gk-ssf-texturepacker-json
                              :flags '(:flip-y)))
               (load-font (cmd-font-create
                           "hardpixel"
                           (get-path "assets" "font" "hardpixel.ttf"))))
          (cmd-list-append config ortho load-title load-sprites load-font)
          (bundle-append b config)
          (gk:process gk b)

          (setf font (font-create-id load-font))
          (setf title (image-create-id load-title))
          (setf spritesheet (make-sheet load-sprites))
          (setf anims (make-instance 'sheet-animations :sheet spritesheet)))))
    pack))
