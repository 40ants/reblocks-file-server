#-asdf3.1 (error "reblocks-file-server requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "reblocks-file-server"
  :description "A Reblocks extension allowing to create routes for serving static files from disk."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/reblocks-file-server/"
  :source-control (:git "https://github.com/40ants/reblocks-file-server")
  :bug-tracker "https://github.com/40ants/reblocks-file-server/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("reblocks-file-server/core")
  :in-order-to ((test-op (test-op "reblocks-file-server-tests"))))


(asdf:register-system-packages "log4cl" '("LOG"))
