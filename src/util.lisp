(in-package :ninja-sphere)

(defvar +time-units+ (coerce internal-time-units-per-second 'float))

(declaim (inline time-to-float))
(declaim (ftype (function (integer) float) time-to-float))
(defun time-to-float (time)
  (/ time +time-units+))

(defun current-time ()
  (time-to-float (get-internal-real-time)))

 ;; State

(defun state (o)
  (slot-value o 'state))

(defun (setf state) (v o)
  (with-slots (state) o
    (change-state o v state)))

(defun state-is (o &rest states)
  (with-slots (state) o
    (find state states)))

(defun do-state (o)
  (let ((old-state (state o)))
    (run-state o old-state)
    (unless (eql (state o) old-state)
      (run-state o (state o)))))

(defgeneric change-state (o to-state from-state)
  (:documentation "Specialize on `O` and `TO-STATE` and optionally `FROM-STATE`.")
  (:method :around (o to-state from-state)
    (with-slots (state) o
      (setf state to-state)
      (call-next-method))))

(defgeneric run-state (o state)
  (:documentation "Specialize on `O` and `STATE` to do things during `STATE`")
  (:method (o state)))
