(defpackage #:weblocks-file-server/core
   (:nicknames #:weblocks-file-server)
   (:use #:cl)
   (:export #:foo
            #:bar))
(in-package weblocks-file-server/core)


(defclass static-files-route (route)
  ((root :type pathname
         :initarg :root
         :reader get-root)
   (uri :type pathname
        :initarg :uri
        :reader get-uri)))


(defun make-route (&key
                     (route-class 'static-files-route)
                     (uri "/dist/")
                     (root "./build/dist/"))
  (log:info "Making a route for a serving an Ultralisp distribution in DEV environment.")
  (let ((route (make-instance route-class
                              :uri (pathname uri)
                              :template (parse-template (concatenate 'string uri "*"))
                              :root (uiop:truename* root))))
    (add-route route)
    (values route)))


(defgeneric render-directory (route path full-path)
  (:documentation "Renders a list of files in a directory"))


(defgeneric serve-file (route full-path)
  (:documentation "Returns content of the file."))


(defun list-directory (full-path)
  "Returns a list of files in the directory.
   All items of the list are relative."
  (loop for file in (cl-fad:list-directory full-path)
        for relative-file = (weblocks/utils/misc:relative-path file full-path)
        collect relative-file))


(defmethod render-directory ((route t) path childrens)
  (let* ((route-root (get-uri route))
         (parent-directory-uri
           (unless (equal path
                          (princ-to-string route-root))
             (cl-fad:pathname-parent-directory path)))
         
         (html (weblocks/html:with-html-string
                 (:div :class "directories"
                       (:h1 :class "current-directory"
                            (princ-to-string path))
                       (:ul
                        (when parent-directory-uri
                          (:li :class "parent-directory"
                               (:a :href parent-directory-uri
                                   "..")))
                        (loop for relative-file in childrens
                              for uri = (merge-pathnames relative-file path)
                              do (:li :class "file-or-directory"
                                      (:a :href (princ-to-string uri)
                                          relative-file))))))))
    (list 200
          (list :content-type "text/html")
          (list html))))


(defmethod serve-file ((route t) full-path)
  (list 200
        (list :content-type "application/binary")
        full-path))


(defun make-full-path (root route-uri request-path)
  "Returns a pathname pointing to the file on the filesystem.

   Root, is a base directory of the route, route-uri is a path
   on the webserver, where files should be served from
   and request-path is a requested path from the webrowser.
   Request-path is always have a route-uri as a prefix.

   For example, if:

   root = \"/app/build/dist/\"
   route-uri = \"/dist/\"
   request-path = \"/dist/the-file.txt\"

   Then this function should return a pathname
   pointing to \"/app/build/dist/the-file.txt\""

  (let* ((relative (weblocks/utils/misc:relative-path request-path route-uri))
         (new-path (merge-pathnames relative root)))
    new-path))


(defmethod serve ((route static-files-route) env)
  "Returns a robots of the site."
  (declare (ignorable env))
  
  (restart-case
      (let* ((path (weblocks/request:get-path))
             (full-path (make-full-path (get-root route)
                                        (get-uri route)
                                        path))
             (is-directory (cl-fad:directory-pathname-p full-path)))
        (if is-directory
            (render-directory route
                              path
                              (list-directory full-path))
            (serve-file route full-path)))
    (abort ()
      :report "Ignore error and return HTTP 500"
      (log:error "Unhandled error")

      (list 500
            (list :content-type "text/html")
            ;; Use method to render an Error page
            (list "Unhandled error")))))
