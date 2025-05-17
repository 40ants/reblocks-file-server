(uiop:define-package #:reblocks-file-server-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:reblocks-file-server-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "CI"
                              "HTTP"))
  (0.4.0 2025-05-16
         "* Updated code to work with latest Reblocks and it's new routes subsystem.")
  (0.3.0 2023-06-07
         "* Added documentation and CI.")
  (0.2.0 2021-12-06
         "* Renamed to reblocks-file-server.")
  (0.1.0 2018-12-26
         "* Initial version."))
