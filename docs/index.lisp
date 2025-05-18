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
  (:import-from #:REBLOCKS-FILE-SERVER/CORE
                #:FILE-SERVER)
  (:import-from #:REBLOCKS-FILE-SERVER/UTILS
                #:DENY-ALL
                #:DENY
                #:ALLOW
                #:ALLOW-IF
                #:DENY-IF)
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
                                   "GIT")
                    :external-docs ("https://40ants.com/reblocks/"))
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
  """
A small demo of what this library does:

![](https://storage.yandexcloud.net/40ants-public/reblocks-file-server/reblocks-file-server.gif)

Here is an example on how this library can be used. All you need is to add a results of call to
FILE-SERVER function call to you Reblocks application's routes.

```lisp
(defapp app
  :prefix "/"
  :routes ((page ("/" :name "index")
             (make-landing-page))
           
           ;; On /documents/
           (file-server "/sources/"
                        :name "sources"
                        :root (asdf:system-relative-pathname :my-app "./"))))
```

In this example, we will show sources of the `my-app` ASDF library starting
from path `/sources/`, ie if user opens your site like https://example.com/sources/,
he will see content of the directory returned by `(asdf:system-relative-pathname :my-app "./")`.

By default, all files and directories are shown. If you want to hide something from user,
you might provide a list of functions which accepts a pathname and returns :ALLOW :DENY or NIL.

Here is a relatively complex example of filtering:

```
(file-server "/sources/"
             :name "sources"
             :root (asdf:system-relative-pathname :my-app "./")
             :filter (list
                                ;; This is how to hide a file inside the current
                                ;; directory
                      (deny #P"main.lisp")
                      ;; Or inside some particular directory
                      (deny #P"pages/*.fasl")
                      ;; Also you might whitelist some directories
                      ;; and files:
                      (allow #P"**/" ;; Allow to show any directory
                             #P"**/*.lisp"
                             #P"favicons/*.png"
                             #P"favicons/*.ico")
                      ;; and then deny the rest.
                      ;; 
                      ;; We need this part because by default
                      ;; all files are allowed:
                      (deny-all)))
```

Here we've used these helped functions provided by utils package:

- ALLOW
- DENY
- DENY-ALL

Read more about how filtering work in the @FILTERING section.
""")


(defsection @usage (:title "Filtering")
  """
FILTER argument of FILE-SERVER function accepts a function or a list of functions
where each function should accept a pathname relative to the ROOT argument given
to the FILE-SERVER function and return :ALLOW, :DENY or NIL.

When function returns :ALLOW or :DENY, processing is stopped and user see either
content or 404 error page. If filter function returns NIL, then other filter functions
are checked.

If no functions matched the current path, then reblocks file server consider it to
be allowed. So we have a black-list mode as a default - if you want something to
be denied - deny it!

In this example we deny access to a `config.lisp` file and fasl files in all directories.
All other files and directories will be allowed:

```
(list
 (deny #P"config.lisp")
 (deny #P"**/*.fasl"))
```

Here I've used DENY helper which accepts a template pathname and returns a function which
checks given pathname to this template pathname.

To switch to the white-list mode, you need to add a last rule which will deny access to
any file or directory. Use DENY-ALL helper for this.

In the next example we deny access to all files and directories but allow listing of
any directory and files with `lisp` extension:

```
(list
 (allow ;; Allow to show any directory
        #P"**/"
        ;; Allow any lisp file
        #P"**/*.lisp")
 (deny-all))
```

If you need some special filtering, then you can use ALLOW-IF or DENY-IF
functions:

```
(deny-if (lambda (path)
           (and
            (cl-fad:directory-pathname-p path)
            (uiop:emptyp
             (cl-fad:list-directory
              (merge-pathnames path
                               (asdf:system-relative-pathname :my-app "./")))))))
```

"""
  )

(defautodoc @api (:system "reblocks-file-server"))
