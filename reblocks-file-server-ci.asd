(defsystem "reblocks-file-server-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/reblocks-file-server/"
  :class :package-inferred-system
  :description "Provides CI settings for reblocks-file-server."
  :source-control (:git "https://github.com/40ants/reblocks-file-server")
  :bug-tracker "https://github.com/40ants/reblocks-file-server/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "reblocks-file-server-ci/ci"))
