
(defpackage indexer
  (:use common-lisp iterate helpers cffi)
  (:import-from fad list-directory directory-pathname-p)
  (:import-from split-sequence split-sequence)
  (:export index-directory))

