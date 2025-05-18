(uiop:define-package #:reblocks-file-server/core
  (:nicknames #:reblocks-file-server)
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:trivial-mimes
                #:mime)
  (:import-from #:reblocks/request)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/utils/misc
                #:relative-path)
  (:import-from #:reblocks/routes
                #:page-route
                #:serve)
  (:import-from #:routes
                #:parse-template)
  (:import-from #:cl-fad)
  (:import-from #:cl-ppcre)
  (:import-from #:40ants-routes/route
                #:current-route
                #:route)
  (:import-from #:40ants-routes/matched-route
                #:original-route
                #:matched-route-p)
  (:import-from #:serapeum
                #:soft-list-of
                #:fmt
                #:->)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:defwidget
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:import-from #:str
                #:starts-with-p
                #:ensure-prefix)
  (:import-from #:reblocks-ui2/tables/table
                #:column)
  (:import-from #:reblocks-ui2/html
                #:html)
  (:import-from #:local-time
                #:format-timestring
                #:universal-to-timestamp)
  (:import-from #:reblocks-ui2/card
                #:card)
  (:import-from #:reblocks-file-server/utils
                #:filter-function
                #:compose-filters)
  (:import-from #:alexandria
                #:last-elt)
  (:export #:file-server-route
           #:list-directory
           #:get-dir-listing
           #:get-root
           #:file-server
           #:filename-filter
           #:directories-first-p))
(in-package #:reblocks-file-server/core)



(defclass file-server-route (page-route)
  ((root :type pathname
         :initarg :root
         :reader get-root)
   (dir-listing :type boolean
                :initform t
                :initarg :dir-listing
                :documentation "When nil, directory contents is not shown."
                :reader get-dir-listing)
   (directories-first-p :type boolean
                        :initform t
                        :initarg :directories-first-p
                        :reader directories-first-p)
   (filter :type filter-function
           :initarg :filter
           :initform (lambda (pathname)
                       (declare (ignore pathname))
                       t)
           :documentation "A regular expression to show only selected files."
           :reader filename-filter)))


(-> file-server (string
                 &key
                 (:name string)
                 (:route-class symbol)
                 (:root pathname)
                 (:dir-listing boolean)
                 (:filter (or filter-function
                              (soft-list-of filter-function))))
    (values file-server-route &optional))


(defun file-server (uri-path
                    &key
                      name
                      (route-class 'file-server-route)
                      (root (uiop:ensure-directory-pathname
                             *default-pathname-defaults*))
		      (dir-listing t)
		      (filter nil))
  "Returns a FILE-SERVER-ROUTE object suitable for including into Reblocks routes hierarchy.

   FILTER argument should be a NIL or a list of filter functions which accept a pathname
   and return :ALLOW :DENY or NIL."
  (log:info "Making a route for serving files from a directory" root)
  (let* ((real-root (uiop:truename* root))
         (route (make-instance route-class
                               :name name
                               :pattern (40ants-routes/url-pattern:parse-url-pattern
                                         (format nil "~A<.*:path>"
                                                 (str:ensure-suffix "/"
                                                                    uri-path)))
                               :handler #'file-server-handler
                               :root (or real-root
                                         (error "Directory ~S does not exist."
                                                root))
	                       :dir-listing dir-listing
			       :filter (compose-filters (uiop:ensure-list filter)))))
    (values route)))


(defwidget directory-widget (ui-widget)
  ((path :initarg :path
         :type pathname
         :documentation "A path extracted from current URL."
         :reader directory-widget-path)
   (route-root-path :initarg :route-root-path
                    :type pathname
                    :documentation "An original path of the reblocks file server's route."
                    :reader directory-widget-route-root-path)
   (directory-relative-path :initarg :directory-relative-path
                            :type pathname
                            :documentation "Path of the current directory relative to the `route-root-path`."
                            :reader directory-widget-directory-relative-path)
   (directories-first-p :type boolean
                        :initform t
                        :initarg :directories-first-p
                        :reader directories-first-p)
   (filter :initarg :filter
           :type function
           :initform (lambda (f)
                       (declare (ignore f))
                       t)
           :documentation "Regex filter to show only entries matched by the filter.."
           :reader directory-widget-filter)))


(defwidget file-widget (ui-widget)
  ((path :initarg :path
         :type pathname
         :documentation "A path extracted from current URL."
         :reader file-widget-path)
   (full-path :initarg :full-path
              :type pathname
              :documentation "File's pathname on the disk."
              :reader file-widget-full-path)))


(defwidget file-not-found-widget (ui-widget)
  ((path :initarg :path
         :type pathname
         :documentation "A path extracted from current URL."
         :reader file-not-found-path)))


(-> sort-pathnames-dirs-first (pathname pathname)
    (values boolean &optional))


(defun sort-pathnames-dirs-first (left right)
  (let ((left-name (pathname-name left))
        (right-name (pathname-name right)))
    (cond
      ((and (null left-name)
            (null right-name))
       (let ((left-dir-name (last-elt (pathname-directory left)))
             (right-dir-name (last-elt (pathname-directory right))))
         (when (string< left-dir-name
                        right-dir-name)
           t)))
      ((and (null left-name)
            right-name)
       t)
      ((and left-name
            (null right-name))
       nil)
      ((and left-name
            right-name)
       (when (string< left-name
                      right-name)
         t)))))


(defun list-directory (route-path directory-relative-path filter &key directories-first-p)
  "Returns a list of files in the directory.
   All items of the list are relative."
  (loop with full-path = (merge-pathnames directory-relative-path route-path)
        for file in (cl-fad:list-directory full-path)
        ;; Here we get a path relative to the route's path,
        ;; because filter function is defined to
        ;; work with paths relative to this root, and
        ;; when user goes deeper we have to recalcuate
        ;; current dir's content relative to the file-server-route's root:
        for relative-file-to-filter = (relative-path file route-path)
        ;; but we will collect a filename relative to the curren directory:
        for relative-file-to-collect = (relative-path file full-path)
	for allowed-p = (not (eql (funcall filter relative-file-to-filter)
                                  :deny))
	when allowed-p
          collect relative-file-to-collect into results
        finally (return (cond
                          (directories-first-p
                           (stable-sort results
                                        #'sort-pathnames-dirs-first))
                          (t
                           results)))))


(-> get-parent (pathname)
    (values pathname &optional))


(defun get-parent (path)
  "Returns the parent directory of the given PATH as a pathname.
Returns NIL if the path does not have a parent directory (e.g., root)."
  (let* ((dir (pathname-directory path))
         (parent-dir (when (rest dir)   ; Check if there are components to remove
                       (butlast dir))))
    (when parent-dir
      (make-pathname :directory parent-dir :defaults path))))


(defmethod render ((widget directory-widget) (theme tailwind-theme))
  (let* ((path (directory-widget-path widget))
         (show-parent-directory-uri-p (not (str:emptyp (namestring path))))
         ;; The filter is designed to work with pathnames relative to
         ;; the route's URL, that is why we need to pass
         ;; a route-path and directory-relative paths separately:
         (children (list-directory (directory-widget-route-root-path widget)
                                   (directory-widget-directory-relative-path widget)
                                   (directory-widget-filter widget)
                                   :directories-first-p (directories-first-p widget)))
         (file-href-class "text-blue-600 dark:text-blue-400 hover:underline")
         (columns (list
                   (column "Filename"
                           :align :left)
                   (column "Created At"
                           :align :right)))
         (datetime-format
           (append local-time:+iso-8601-date-format+
                   (list #\Space)
                   (butlast ;; we don't want to see dot and milliseconds
                    (butlast
                     local-time:+iso-8601-time-format+))))
         (table-rows
           (flet ((filename (filename)
                    (let ((filename-as-str (princ-to-string filename)))
                      (html (:a :class file-href-class
                                :href filename-as-str
                              filename-as-str)))))
             (append
              (when show-parent-directory-uri-p
                (list
                 (list (filename "..") 
                       "")))
              (loop with current-directory-path = (merge-pathnames
                                                   (directory-widget-directory-relative-path widget)
                                                   (directory-widget-route-root-path widget))
                    for relative-file in children
                    for full-path = (merge-pathnames relative-file
                                                     current-directory-path)
                    for created-at = (file-write-date full-path)
                    collect (list (filename relative-file)
                                  (format-timestring nil
                                                     (universal-to-timestamp created-at)
                                                     :format datetime-format)))))))
    (with-html ()
      (:div :class "flex flex-col gap-4 dark:bg-gray-900"
            (:h1 :class "text-xl p-4 bg-gray-100 dark:bg-gray-800"
                 (fmt "Directory ~S"
                      (ensure-prefix "/"
                                     (namestring path))))
            
            (render
             (reblocks-ui2/tables/table:make-table columns table-rows)
             theme)))))


(defun image-to-base64 (path)
  "Returns an image as base64 encoded string like this \"data:image/png;base64,iVBOR...\"."
  (format nil
          "data:image/png;base64,~A"
          (base64:usb8-array-to-base64-string
           (alexandria:read-file-into-byte-vector path))))


(defmethod render ((widget file-widget) (theme tailwind-theme))
  (let* ((full-path (file-widget-full-path widget))
         (content-type (mime full-path))
         (text-content-p (starts-with-p "text/" content-type))
         (image-content-p (starts-with-p "image/" content-type))
         (file-title-widget
           (html (:h1 :class "text-xl p-4 bg-gray-100 dark:bg-gray-800"
                   (fmt "File ~S"
                        (ensure-prefix "/"
                                       (namestring (file-widget-path widget)))))))
         (file-content-widget
           (card
            (html ((cond
                     (text-content-p
                      (:pre
                       (:code
                        (:raw (uiop:read-file-string full-path)))))
                     (image-content-p
                      (:img :src (image-to-base64 full-path)))
                     (t
                      (:p (fmt "Unable to render file of type ~S"
                               content-type))))))
            :horizontal-align :left
            :view :raised)))
    (render file-title-widget theme)
    (render file-content-widget theme)))


(defmethod render ((widget file-not-found-widget) (theme tailwind-theme))
  (with-html ()
    (:div :class "file-server-body"
          (:h1 :class "file-not-found"
               (fmt "File \"~A\" not found!"
                    (file-not-found-path widget))))))


(-> file-server-handler (&key (:path string))
    (values t &optional))

(defun file-server-handler (&key path)
  (let* ((path
           ;; PATH argument is extracted from the URL
           ;; and have / as delimiter. That is why
           ;; it is better to parse it as a unix namestring:
           (uiop:parse-unix-namestring path))
         (route (progn
                  (unless (matched-route-p (current-route))
                    (error "Unexpected curren route type: ~S"
                           (type-of (current-route))))
                  (original-route (current-route))))
	 (dir-listing (get-dir-listing route))
         ;; A path to the file on the hard drive
         (original-full-path
           (merge-pathnames path
                            (get-root route)))
         ;; Here cl-fad will add a missing / if
         ;; full-path is pointing to a directory but
         ;; does not contains / on the end
         (full-path (cl-fad:file-exists-p original-full-path))
         (is-directory (when full-path
                         (cl-fad:directory-pathname-p full-path)))
         (exists-p full-path)
         ;; For security reason we need to check if directory
         ;; is allowed to be listed:
	 (allowed-p (when full-path
                      (and (not (eql (funcall (filename-filter route)
                                              path)
                                     :deny))
                           (or (not is-directory)
                               dir-listing)))))

    (cond ((or (not exists-p)
	       (not allowed-p))
           (log:warn "File not found: ~A" path)
           (values ;; We need this to suppress linter's warning about function returning value
            (reblocks/response:not-found-error
             (make-instance 'file-not-found-widget
                            :path path))))
          (is-directory
           (make-instance 'directory-widget
                          :path path
                          :route-root-path (get-root route)
                          :directory-relative-path (relative-path full-path
                                                                  (get-root route))
                          :directories-first-p (directories-first-p route)
                          ;; When rendering a directory we will
                          ;; apply filter again:
                          :filter (filename-filter route)))
          (t
           (make-instance 'file-widget
                          :path path
                          :full-path full-path)))))


#|
example:
(reblocks-file-server:make-route :uri "/static/" :root "/tmp/" :dir-listing nil :filter ".*.txt")
; now access 127.0.0.1/static/1.txt
(reblocks-file-server:make-route :uri "/static/" :root "/tmp/" :dir-listing t :filter ".*.gif")
; now access 127.0.0.1/static/2.gif
  and 127.0.0.1/static/
; in this example, we display and give access to all files except for .txt :
(reblocks-file-server:make-route :uri "/static/" :root "/tmp/" :dir-listing t :filter ".*.txt" :filter-type nil)
|#
