;;; Modified from the example in cl-gamekernel

(in-package :ninja-sphere)

(defclass sprite ()
  (qs trs
   (visiblep :initform t :accessor sprite-visible)))

(defmethod initialize-instance ((s sprite) &key sheet index name
                                pos size key)
  (call-next-method)
  (with-slots (qs trs scale) s
    (let* ((sheet (or sheet (asset-sheet *assets*)))
           (index (if name
                      (or (find-frame sheet name)
                          (find-frame sheet "nosprite.png"))
                      (or index
                          (find-frame sheet "nosprite.png")))))
      (setf qs (cmd-quadsprite sheet index :key key)))
    (setf trs (cmd-tf-trs :out (quadsprite-tfm qs)
                          :translate pos
                          :scale size))))

(defun sprite-pos (s)
  (tf-trs-translate (slot-value s 'trs)))

(defun (setf sprite-pos) (v s)
  (setf (tf-trs-translate (slot-value s 'trs)) v))

(defun sprite-index (s)
  (quadsprite-index (slot-value s 'qs)))

(defun (setf sprite-index) (v s)
  (with-slots (qs) s
    (setf (quadsprite-index qs) v)))

(defun (setf sprite-name) (name s)
  (let ((sheet (asset-sheet *assets*)))
    (setf (sprite-index s)
          (or (find-frame sheet name)
              (progn
                (format t "Warning: Can't find sprite \"~A\"~%" name)
                (find-frame sheet "nosprite.png"))))))

(defun sprite-key (sprite)
  (cmd-key (slot-value sprite 'qs)))

(defun (setf sprite-key) (v sprite)
  (with-slots (qs) sprite
    (setf (cmd-key qs) v)))

(defmethod draw ((sprite sprite) lists m)
  (with-slots (pre-list sprite-list) lists
    (with-slots (qs trs visiblep) sprite
      (when visiblep
        (setf (tf-trs-prior trs) m)
        (cmd-list-append pre-list trs)
        (cmd-list-append sprite-list qs)))))

 ;; Spritesheet animations

(defclass sprite-anim ()
  ((indexes :initform (make-array 2 :element-type '(unsigned-byte 16) :adjustable t :fill-pointer 0)
            :reader sprite-anim-indexes)))

(defun sprite-anim-append (sa index)
  (with-slots (indexes) sa
    (vector-push-extend index indexes)))

(defun sprite-anim-frame (sa frame)
  (with-slots (indexes) sa
    (aref indexes frame)))

(defun sprite-anim-length (sa)
  (with-slots (indexes) sa
    (length indexes)))

(defclass sheet-animations ()
  ((sheet :initarg :sheet :initform nil)
   (anims :initform (make-hash-table :test 'equalp))))

(defmethod initialize-instance :after ((s sheet-animations) &key &allow-other-keys)
  (with-slots (sheet anims) s
    (map-spritesheet
     (lambda (x i)
       (let* ((m (nth-value 1 (ppcre:scan-to-strings "(.*)_(\\d+)\\.png$" x))))
         (when m
           (let* ((name (ppcre:regex-replace-all "_" (aref m 0) "-"))
                  (anim (ensure-gethash (nstring-upcase name) anims
                                        (make-instance 'sprite-anim))))
             (sprite-anim-append anim i)))))
     sheet)))

(defun find-anim-frame (animations name frame)
  (with-slots (anims) animations
    (sprite-anim-frame (gethash name anims) frame)))

(defun find-anim (animations name)
  (with-slots (anims) animations
    (gethash name anims)))

(defun sheet-anim-length (sheet name)
  (with-slots (anims) sheet
    (with-slots (indexes)
        (or (gethash sheet name)
            (error "Can't find animation: ~S" name))
      (length indexes))))
