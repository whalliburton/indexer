(defsystem :indexer
  :serial t
  :components ((:static-file "indexer.asd")
               (:file "package")
               (:file "magic")
               (:file "index-directory")
               (:file "initialize"))
  :depends-on (:helpers :cffi :cxml))
