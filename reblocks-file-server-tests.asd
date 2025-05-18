(defsystem "reblocks-file-server-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/reblocks-file-server/"
  :class :package-inferred-system
  :description "Provides tests for reblocks-file-server."
  :source-control (:git "https://github.com/40ants/reblocks-file-server")
  :bug-tracker "https://github.com/40ants/reblocks-file-server/issues"
  :pathname "t"
  :depends-on ("reblocks-file-server-tests/core"
               "reblocks-file-server-tests/utils")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
