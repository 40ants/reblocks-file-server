(defsystem "reblocks-file-server-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/reblocks-file-server/"
  :class :package-inferred-system
  :description "Provides documentation for reblocks-file-server."
  :source-control (:git "https://github.com/40ants/reblocks-file-server")
  :bug-tracker "https://github.com/40ants/reblocks-file-server/issues"
  :pathname "docs"
  :depends-on ("reblocks-file-server"
               "reblocks-file-server-docs/index"))
