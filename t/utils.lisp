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
  (testing "Current directory"
    ;; This filter should allow match only on files in the
    ;; current directory
    (ok (eql (funcall (allow #P"*.txt")
                      #P"bar.txt")
             :allow))
    (ok (eql (funcall (allow #P"**.txt")
                      #P"bar.txt")
             :allow))
    ;; Thus here it should return NIL
    (ok (null (funcall (allow #P"*.txt")
                       #P"foo/bar.txt")))
    ;; Here we has two wildcards but both in the filename
    (ok (null (funcall (allow #P"**.txt")
                       #P"foo/bar.txt"))))
  
  (testing "One path" 
    (ok (eql (funcall (allow #P"foo/bar.txt")
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
    ;; This pattern matches only on lisp files in current dir:
    (ok (null (funcall (allow #P"*.lisp")
                       #P"src/foo/bar.lisp")))
    ;; This one too, because **.lisp has wildcards only in the filename not in the directory name:
    (ok (null (funcall (allow #P"**.lisp")
                       #P"src/foo/bar.lisp")))
    
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
                       #P"foo/bar.lisp"))))
  
  (testing "Partial matching"
    (ok (null (funcall (deny #P"src/f*.lisp")
                       #P"src/bar.lisp")))
    (ok (eql (funcall (deny #P"src/f*.lisp")
                      #P"src/forms.lisp")
             :deny))))


(deftest test-combined-filter
  (let ((filter (compose-filters
                 (list
                  (deny #P"main.lisp")
                  (allow #P"**/" ;; Allow to show any directory
                         ;; And any lisp file in any directory
                         #P"**/*.lisp"
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
