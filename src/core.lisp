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
  (:export #:file-server-route
           #:list-directory
           #:get-dir-listing
           #:get-root
           #:file-server
           #:filename-filter))
(in-package #:reblocks-file-server/core)



(defclass file-server-route (page-route)
  ((root :type pathname
         :initarg :root
         :reader get-root)
   (dir-listing :type t
                :initform t
                :initarg :dir-listing
                :documentation "When nil, directory contents is not shown."
                :reader get-dir-listing)
   (filter :type string
           :initarg :filter
           :initform ""
           :documentation "A regular expression to show only selected files."
           :reader filename-filter)))


(defun file-server (uri-path
                    &key
                      name
                      (route-class 'file-server-route)
                      (root "./")
		      (dir-listing t)
		      (filter ".*"))
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
			       :filter filter)))
    (values route)))


(defwidget directory-widget (ui-widget)
  ((path :initarg :path
         :type string
         :documentation "A path extracted from current URL."
         :reader directory-widget-path)
   (full-path :initarg :full-path
              :type pathname
              :documentation "Directory's pathname on the disk."
              :reader directory-widget-full-path)
   (filter :initarg :filter
           :type string
           :initform ""
           :documentation "Regex filter to show only entries matched by the filter.."
           :reader directory-widget-filter)))


(defwidget file-widget (ui-widget)
  ((path :initarg :path
         :type string
         :documentation "A path extracted from current URL."
         :reader file-widget-path)
   (full-path :initarg :full-path
              :type pathname
              :documentation "File's pathname on the disk."
              :reader file-widget-full-path)))


(defwidget file-not-found-widget (ui-widget)
  ((path :initarg :path
         :type string
         :documentation "A path extracted from current URL."
         :reader file-not-found-path)))


(defun list-directory (full-path filter)
  "Returns a list of files in the directory.
   All items of the list are relative."
  (loop for file in (cl-fad:list-directory full-path)
        for relative-file = (relative-path file full-path)
	for filtered-p = (ppcre:scan filter (namestring file))
	if filtered-p
          collect relative-file))


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
         (show-parent-directory-uri-p (not (str:emptyp path)))
         (children (list-directory (directory-widget-full-path widget)
                                   (directory-widget-filter widget)))
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
                    (html (:a :class file-href-class
                              :href filename
                            filename))))
             (append
              (when show-parent-directory-uri-p
                (list
                 (list (filename "..") 
                       "")))
              (loop for relative-file in children
                    for full-path = (merge-pathnames relative-file
                                                     (directory-widget-full-path widget))
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
                                     path)))
            
            (render
             (reblocks-ui2/tables/table:make-table columns table-rows)
             theme)))))


(defmethod render ((widget file-widget) (theme tailwind-theme))
  (let* ((full-path (file-widget-full-path widget))
         (content-type (mime full-path))
         (text-content-p (starts-with-p "text/" content-type)))
    (render
     (card
      (html ((cond
               (text-content-p
                (:pre
                 (:code
                  (:raw (uiop:read-file-string full-path)))))
               (t
                (:p (fmt "Unable to render file of type ~S"
                         content-type))))))
      :horizontal-align :left
      :view :raised)
     theme)))


(defmethod render ((widget file-not-found-widget) (theme tailwind-theme))
  (with-html ()
    (:div :class "file-server-body"
          (:h1 :class "file-not-found"
               (fmt "File \"~A\" not found!"
                    (file-not-found-path widget))))))


(defun file-server-handler (&key path)
  (let* ((route (progn
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
         (not-exists-p (null full-path))
	 filtered-p)

    ;; For security reason we need to check
    (when full-path
      (setf filtered-p (ppcre:scan (filename-filter route)
                                   (namestring full-path))))

    
    (cond ((or not-exists-p
	       filtered-p
	       (and is-directory
                    (null dir-listing)))
           (log:warn "File not found: ~A" path)
           (reblocks/response:not-found-error
            (make-instance 'file-not-found-widget
                           :path path)))
          (is-directory
           (make-instance 'directory-widget
                          :path path
                          :full-path full-path
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
