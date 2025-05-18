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


(deftype filter-function ()
  '(function (pathname)
    (values (member :allow :deny nil))))


(-> compose-filters ((soft-list-of filter-function))
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


(-> allow (pathname &rest pathname)
    (values filter-function
            &optional))


(-> match-pathnames (pathname (soft-list-of pathname) (member :allow :deny))
    (values (member :allow :deny nil)
            &optional))


(defun match-pathnames (pathname pathname-templates value-to-return)
  (loop for template-pathname in pathname-templates
        when (and (not (and (null (pathname-name template-pathname))
                            ;; We don't want pathname like foo/bar/test.lisp to be matched to
                            ;; wildcards like **/
                            (pathname-name pathname)))
                  (pathname-match-p pathname template-pathname))
          do (return value-to-return)))


(defun allow (pathname &rest more-pathnames)
  "Returns a function of one argument which will check this argument against given pathnames and if there is match, returns :allow."
  (flet ((allow-pathname (f)
           (match-pathnames f (list* pathname more-pathnames)
                            :allow)))
    #'allow-pathname))


(-> deny (pathname &rest pathname)
    (values filter-function
            &optional))

(defun deny (pathname &rest more-pathnames)
  "Returns a function of one argument which will check this argument against given pathnames and if there is match, returns :deny."
  (flet ((allow-pathname (f)
           (match-pathnames f (list* pathname more-pathnames)
                            :deny)))
    #'allow-pathname))



(-> deny-all ()
    (values filter-function
            &optional))

(defun deny-all ()
  "Returns a function which will deny all files."
  (flet ((deny-all-function (pathname)
           (declare (ignore pathname))
           :deny))
    #'deny-all-function))
