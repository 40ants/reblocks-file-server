(uiop:define-package #:reblocks-file-server-tests/utils
  (:use #:cl)
  (:import-from #:rove
                #:testing
                #:ng
                #:ok
                #:deftest)
  (:import-from #:reblocks-file-server/utils
                #:compose-filters
                #:deny
                #:allow))
(in-package #:reblocks-file-server-tests/utils)


(deftest test-allow-func ()
  (testing "One path" 
    (ok (eql (funcall (allow #P"foo/bar.txt")
                      #P"foo/bar.txt")
             :allow))
    (ok (eql (funcall (allow #P"*.txt")
                      #P"foo/bar.txt")
             :allow))
    (ok (eql (funcall (allow #P"foo/*.txt")
                      #P"foo/bar.txt")
             :allow))
    (ok (null (funcall (allow #P"bar/*.txt")
                      #P"foo/bar.txt")))
    (ok (null (funcall (allow #P"foo/*.lisp")
                       #P"foo/bar.txt"))))

  (testing "Multiple templates"
    (ok (eql (funcall (allow #P"docs/*.md"
                             #P"src/*.lisp"
                             #P"foo/*.txt")
                      #P"foo/bar.txt")
             :allow)))
  
  (testing "Nested dirs"
    (ok (eql (funcall (allow #P"*.lisp")
                      #P"src/foo/bar.lisp")
             :allow))
    (ok (eql (funcall (allow #P"**.lisp")
                      #P"src/foo/bar.lisp")
             :allow))
    (ok (eql (funcall (allow #P"**/*.lisp")
                      #P"src/foo/bar.lisp")
             :allow))

    (ok (null (funcall (allow #P"*.lisp")
                      #P"src/foo/bar.txt")))
    (ok (null (funcall (allow #P"**.lisp")
                       #P"src/foo/bar.txt")))
    (ok (null (funcall (allow #P"**/*.lisp")
                       #P"src/foo/bar.txt"))))

  (testing "Allow directories"
    (ok (null (funcall (allow #P"**/")
                      #P"src/foo/bar.lisp")))
    (ok (eql (funcall (allow #P"**/")
                      #P"src/foo/")
             :allow))

    (ok (null (funcall (allow #P"*/")
                       #P"src/bar.lisp")))
    (ok (null (funcall (allow #P"*/")
                       #P"src/foo/")))
    (ok (eql (funcall (allow #P"*/")
                      #P"src/")
             :allow))))


(deftest test-deny-filter
  (testing "Deny all with */*"
    ;; This files is not in a subdirectory and will be allowed
    (ok (null (funcall (deny #P"*/*")
                       #P"foo.lisp")))
    (ok (eql (funcall (deny #P"*/*")
                      #P"foo/bar.lisp")
             :deny))
    ;; This file is two level depth and will be allowed
    (ok (null (funcall (deny #P"*/*")
                       #P"foo/bar/blah.lisp"))))
  
  (testing "Deny all with **"
    (ok (eql (funcall (deny #P"**/*")
                      #P"foo.lisp")
             :deny))
    (ok (eql (funcall (deny #P"**/*")
                      #P"foo/")
             :deny))
    (ok (eql (funcall (deny #P"**/*")
                      #P"foo/bar.lisp")
             :deny))
    (ok (eql (funcall (deny #P"**/*")
                      #P"foo/bar/")
             :deny))
    (ok (eql (funcall (deny #P"**/*")
                      #P"foo/bar/blah.lisp")
             :deny)))
  
  (testing "Nested deny"
    (ok (eql (funcall (deny #P"foo/*.fasl")
                      #P"foo/bar.fasl")
             :deny))
    (ok (null (funcall (deny #P"foo/*.fasl")
                       #P"foo/bar.lisp")))))


(deftest test-combined-filter
  (let ((filter (compose-filters
                 (list
                  (deny #P"main.lisp")
                  (allow #P"**/" ;; Allow to show any directory
                         #P"*.lisp"
                         #P"favicons/*.png"
                         #P"favicons/*.ico")
                  ;; deny-all
                  (deny #P"**/*")))))
    (ok (eql (funcall filter
                      #P"foo.lisp")
             :allow))
    (ok (eql (funcall filter
                      #P"main.lisp")
             :deny))
    (ok (eql (funcall filter
                      #P"src/foo.lisp")
             :allow))))
