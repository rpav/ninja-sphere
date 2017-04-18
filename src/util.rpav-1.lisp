;; -*- lisp -*-
;;
;; Re-run this command to regenerate this file.  It'll overwrite
;; whatever is there, so make sure it's going to the right place:
#+(or)
(make-util:make-util '(ninja-sphere :src "util.rpav-1") :package "UTIL.RPAV-1"
                     :symbols
                     '(laconic:string-join laconic:string+ laconic:substr
                                           laconic:aval laconic:akey
                                           (setf laconic:aval))
                     :exportp t)

;; ===================================================================
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "UTIL.RPAV-1")
    (make-package "UTIL.RPAV-1" :use '(#:cl))))
(in-package "UTIL.RPAV-1")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'string-join)
    (defun string-join (list string)
      (labels ((zipper (c list)
                 (when list
                   (list* c (format nil "~A" (car list))
                          (zipper string (cdr list))))))
        (apply #'concatenate 'string (zipper "" list))))))

(export 'string-join)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'string+)
    (defun string+ (string &rest strings)
      "=> NEW-STRING
Concatenate string designators into a new string."
      (apply #'concatenate 'string (string string) (mapcar #'string strings)))))

(export 'string+)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'substr)
    (defun substr (str start &optional end)
      "=> DISPLACED-SUBSTRING
Make a shared substring of `STR` using `MAKE-ARRAY :displaced-to`"
      (let* ((end (or end (length str))) (len (- end start)))
        (make-array len :element-type (array-element-type str) :displaced-to
                    str :displaced-index-offset start)))))

(export 'substr)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'aval)
    (defun aval (akey alist &key (key 'identity) (test 'eq))
      "Get the value for key `KEY` in `ALIST`"
      (cdr (assoc akey alist :key key :test test)))))

(export 'aval)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'akey)
    (defun akey (val alist &key (key 'identity) (test 'eq))
      "Get the key for value `VAL` in `ALIST`"
      (car (rassoc val alist :key key :test test)))))

(export 'akey)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp '(setf aval))
    (defun (setf aval) (v val alist &key (key 'identity) (test 'eq))
      (setf (cdr (assoc val alist :key key :test test)) v))))
