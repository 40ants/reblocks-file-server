(defsystem weblocks-file-server
           :version (:read-file-form "version.lisp-expr")
           :author "Alexander Artemenko"
           :license ""
           :class :package-inferred-system
           :pathname "src"
           :depends-on (
                                   "weblocks-file-server/core")
           :description ""
           :long-description
           #.(with-open-file (stream (merge-pathnames
                                      #p"README.rst"
                                      (or *load-pathname* *compile-file-pathname*))
                                     :if-does-not-exist nil
                                     :direction :input
                                     :external-format :utf-8)
               (when stream
                 (let ((seq (make-array (file-length stream)
                                        :element-type 'character
                                        :fill-pointer t)))
                   (setf (fill-pointer seq)
                         (read-sequence seq stream))
                   seq))))

