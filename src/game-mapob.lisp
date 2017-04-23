(in-package :ninja-sphere)

(defclass game-mapob ()
  ((name :initform nil :initarg :name :reader game-item-name)
   (type :initform nil :initarg :type :reader game-item-type)
   (properties :initform nil :initarg :properties)
   (body :initform nil)))

(defmethod initialize-instance :after ((g game-mapob) &key world pos &allow-other-keys)
  (declare (ignorable world pos))
  (with-slots (body) g
    (setf body (make-b2-body g))))

(defgeneric property (object name))
(defmethod property ((o game-mapob) name)
  (with-slots (properties) o
    (aval name properties)))

 ;; game-sensob

(defclass game-senseob (game-mapob) ())

(defmethod initialize-instance :after ((g game-senseob) &key world pos &allow-other-keys)
  (with-slots (body properties) g
    (let* ((bundle (make-instance 'bundle))
           (list (make-instance 'cmd-list-b2))
           (bodydef (b2-bodydef body :static
                                :position (gk-vec2 (f* (vx pos)) (f* (vy pos)))))
           (body-create (cmd-b2-body-create world bodydef))
           (fixture-create
             (cmd-b2-fixture-create
              body
              (list
               :begin
               :rect (f* -0.5) (f* -0.5) (f* 1.0) (f* 1.0)
               :sensor :fixture-id 1.0
               :fill))))
      (bundle-append bundle list)
      (cmd-list-append list body-create fixture-create)
      (gk:process *gk* bundle))))

(defmethod collide ((g game-senseob) b id-a id-b)
  (with-slots (type) g
    (when (eql 0 id-b)
      (on-enter (game-item-type g) b g))
    (call-next-method)))

(defmethod separate ((g game-senseob) b id-a id-b)
  (with-slots (type) g
    (when (eql 0 id-b)
      (on-leave (game-item-type g) b g))
    (call-next-method)))

(defgeneric on-enter (type actor item)
  (:method (type actor item)
    (:say "ON-ENTER " type " with no handler")))

(defgeneric on-leave (type actor item)
  (:method (type actor item)
    (:say "ON-LEAVE " type " with no handler")))

 ;; Types

(defmethod on-enter ((type (eql :door)) actor item)
  (setf (game-value :door) (game-item-name item))
  (setf (game-value :start) (property item :start)))

(defmethod on-leave ((type (eql :door)) actor item)
  (when (equal (game-item-name item)
               (game-value :door))
    (setf (game-value :door) nil)))
