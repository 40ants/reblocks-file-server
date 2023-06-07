(uiop:define-package #:reblocks-file-server-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:reblocks-file-server-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:reblocks-file-server-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "reblocks-file-server-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "reblocks-file-server - A Reblocks extension allowing to create routes for serving static files from disk."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "TODO"
                                   "HTML"
                                   "Unlicense"
                                   "REPL"
                                   "GIT"))
  (reblocks-file-server system)
  "
[![](https://github-actions.40ants.com/40ants/reblocks-file-server/matrix.svg?only=ci.run-tests)](https://github.com/40ants/reblocks-file-server/actions)

![Quicklisp](http://quickdocs.org/badge/reblocks-file-server.svg)
"
  (@installation section)
  (@usage section)
  (@api section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :reblocks-file-server)
```
""")


(defsection @usage (:title "Usage"
                    :ignore-words ("ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"))
  "
Here is a few examples on how this library can be used. These lines can be added into the code
which starts your Reblocks web application.

This is how to serve all `*.txt` files from the `/var/www` folder:

```lisp
(reblocks-file-server:make-route :uri \"/static/\"
                                 :root \"/var/www/\"
                                 :dir-listing nil
                                 :filter \".*.txt\")
```

You also can provide a DIR-LISTING argument to repond on /static/ route with a rendered directory listing:

```lisp
(reblocks-file-server:make-route :uri \"/static/\"
                                 :root \"/var/www/\"
                                 :dir-listing t
                                 :filter \".*.txt\")
```

In case if you want to serve all files except `*.txt`, you can negate filter expression by giving NIL in FILTER-TYPE argument:

```lisp
(reblocks-file-server:make-route :uri \"/static/\"
                                 :root \"/var/www/\"
                                 :dir-listing t
                                 :filter \".*.txt\"
                                 :filter-type nil)
```

")


(defautodoc @api (:system "reblocks-file-server"))
