(uiop:define-package #:reblocks-file-server/utils
  (:use #:cl)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:export #:allow
           #:deny
           #:filter-function
           #:deny-all))
(in-package #:reblocks-file-server/utils)


;; Because of this error:
;; 
;; > Function types are not a legal argument to TYPEP
;; 
;; we can not use this type definition in the compose-filters
;; function type declaration.
;; 
;; More information about why it is impossible to
;; allow runtime typechecks for functions is here:
;; https://stackoverflow.com/questions/58927489/is-it-possible-to-check-get-function-type-or-its-signature-at-runtime-in-sbcl-co
(deftype filter-function ()
  '(function (pathname)
    (values (member :allow :deny nil))))

;; This fails at least on CCL:
;; (-> compose-filters ((soft-list-of filter-function))
;;     (values filter-function &optional))

(-> compose-filters ((soft-list-of function))
    (values filter-function &optional))

(defun compose-filters (filters)
  "Returns a function which calls each filter and returns T when the first of them returns T.

   All filters should be the same arity."
  (flet ((composite-filter (pathname)
           (loop for filter in filters
                 for result = (funcall filter pathname)
                 when result
                   do (return result))))
    #'composite-filter))



(-> ensure-has-directory (pathname)
    (values pathname &optional))


(defun ensure-has-directory (pathname)
  (cond
    ((pathname-directory pathname)
     pathname)
    (t
     (merge-pathnames pathname
                      (make-pathname :directory '(:relative))))))


(-> match-pathnames (pathname (soft-list-of pathname) (member :allow :deny))
    (values (member :allow :deny nil)
            &optional))


(defun match-pathnames (pathname pathname-templates value-to-return)
  (loop with pathname = (ensure-has-directory pathname)
        for template-pathname in pathname-templates
        when (and (not (and (null (pathname-name template-pathname))
                            ;; We don't want pathname like foo/bar/test.lisp to be matched to
                            ;; wildcards like **/
                            (pathname-name pathname)))
                  (pathname-match-p pathname template-pathname))
        do (return value-to-return)))


(-> allow (pathname &rest pathname)
    (values filter-function
            &optional))

(defun allow (pathname &rest more-pathnames)
  "Returns a function of one argument which will check this argument against given pathnames and if there is match, returns :allow."
  (let ((pathname-templates
          (mapcar #'ensure-has-directory
                  (list* pathname more-pathnames))))
    (flet ((allow-pathname (f)
             (match-pathnames f pathname-templates
                              :allow)))
      #'allow-pathname)))


(-> deny (pathname &rest pathname)
    (values filter-function
            &optional))

(defun deny (pathname &rest more-pathnames)
  "Returns a function of one argument which will check this argument against given pathnames and if there is match, returns :deny."
  (let ((pathname-templates
          (mapcar #'ensure-has-directory
                  (list* pathname more-pathnames))))
    (flet ((deny-pathname (f)
             (match-pathnames f pathname-templates
                              :deny)))
      #'deny-pathname)))



(-> deny-all ()
    (values filter-function
            &optional))

(defun deny-all ()
  "Returns a function which will deny all files."
  (flet ((deny-all-function (pathname)
           (declare (ignore pathname))
           :deny))
    #'deny-all-function))
