(in-package :ninja-sphere)

(defparameter *default-map* "L1")

(defvar *gk* nil
  "Global GK context")

(defvar *assets* nil
  "This should be set to ASSETS when any game logic stuff is called")

(defvar *time* nil
  "This should be set to the current frame time during game logic")

(defvar *ps* nil
  "This should be set to the game's phase stack")

(defvar *window* nil
  "The current game-window")

(defvar *lists* nil
  "The render/physics lists")

(defvar *scale* nil
  "'Virtual resolution' scale; this represents 1px scaled.")

(defvar *ps* nil
  "The phase stack")

(defvar *anim-manager* nil
  "The animation manager")

(progn
  (defparameter *physics-scale* 64.0)
  (defparameter *f* (/ 16.0 *physics-scale*)))

(defun f* (&rest factors)
  (apply '* *f* factors))

(defgeneric physics (thing lists)
  (:documentation "Do physics for THING")
  (:method (thing lists)))

(defgeneric post-physics (thing lists)
  (:documentation "Do stuff after physics is processed")
  (:method (thing lists)))

(defgeneric draw (thing lists matrix)
  (:documentation "Draw `THING` given `LISTS` and prior transformation `MATRIX`"))

(defgeneric collide (a b id-a id-b)
  (:documentation "Called when A collides with B.  Some things may reverse
this and call COLLIDE again with (B A). ID-A and ID-B are fixture IDs.")
  (:method (a b id-a id-b))
  (:method :around (a b id-a id-b)
    #++(:say a id-a :br b id-b)
    (call-next-method)))

(defgeneric separate (a b id-a id-b)
  (:documentation "Called when A separates from B.  Some things may reverse
this and call SEPARATE again with (B A). ID-A and ID-B are fixture IDs.")
  (:method (a b id-a id-b)))

(defgeneric item-remove-p (o))
(defgeneric item-remove-cmd (o))

(defgeneric remove-sprite-p (o)
  (:documentation "On removal, remove sprite")
  (:method (o) t))

(defgeneric go-live (o &key &allow-other-keys)
  (:documentation "Called as an object goes 'live' in play.")
  (:method (o &key &allow-other-keys)))

(defgeneric cleanup (o)
  (:documentation "Clean up object. Called by e.g. MAP-CHANGE on the screen,
and similar.  Clean up resources (e.g. box2d world) here.")
  (:method (o)))

(defgeneric die (o actor)
  (:documentation "Called when something gets hit by ACTOR.")
  (:method (o a)))
