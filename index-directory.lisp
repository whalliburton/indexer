(in-package :indexer)

(defvar *index-cache* (make-hash-table :test 'equal))

;; to remove indexes: find . -name .file-index -print0 | xargs -0 rm

(defun ensure-trailing-slash (pathname)
  (let ((name (namestring pathname)))
    (if (string-ends-with name "/")
      name
      (concatenate 'string name "/"))))

(defun file-index-name (pathname)
  (format nil "~A.file-index" (ensure-trailing-slash pathname)))

(defun load-directory-index (pathname)
  (let ((index-filename (file-index-name pathname)))
    (when (probe-file index-filename)
      (iter (for (k v) in (let ((*read-eval* nil))
                            (read-from-string (slurp-file index-filename))))
            (setf (gethash k *index-cache*) v)))))

(defun index-directory (pathname)
  (load-directory-index pathname)
  (iter (for filename in (list-directory pathname))
        (let ((index (index-file filename)))
          (format t "~A~%" (first index))
          (if (directory-pathname-p filename)
            (collect index into subdirs)
            (collect index into files)))
        (finally
         (with-output-to-file (stream (file-index-name pathname) :if-exists :supersede)
           (prin1 files stream))
         (return (append files subdirs)))))

(defun unescape-pathname (pathname)
  (ppcre:regex-replace-all "\\" (namestring pathname) ""))

(defun sha256-file (pathname)
  (with-timeout-action (5 nil)
    (first (split-sequence #\space (run-program-to-string "sha256sum" (list (unescape-pathname pathname)))))))

(defun file-size (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun mediainfo (pathname)
  (let ((name (unescape-pathname pathname)))
    (iter (for track in
               (cdddr
                (fourth
                 (cxml:parse
                  (handler-case
                      (run-program-to-string "mediainfo" `("--output=XML" ,name))
                    (error (c) (warn "Error in mediainfo ~S." c)
                      (return-from mediainfo nil)))
                  (cxml-xmls:make-xmls-builder)))))
          (when (consp track)
            (destructuring-bind (name params . properties) track
              (cond
                ((string= name "track")
                 (collect (append
                           (list (ksymb (string-upcase (car (assoc-value params "type" :test 'equal)))))
                           (iter (for prop in properties)
                                 (when (consp prop)
                                   (collect (cons
                                             (ksymb (substitute #\- #\_ (string-upcase (first prop))))
                                             (third prop)))
                                   )))))
                (t (warn "Invalid track ~S." track))))))))

(defun index-file (pathname)
  (or
   (gethash (namestring pathname) *index-cache*)
   (cond
     ((directory-pathname-p pathname)
      (list pathname :directory (index-directory pathname)))
     (t
      (setf (gethash (namestring pathname) *index-cache*)
            (multiple-value-bind (magic mame) (magic pathname)
              (list
               pathname
               (file-size pathname)
               (sha256-file pathname)
               mame
               magic
               (cond
                 ((equal mame "application/pdf")
                  (multiple-value-list (pdf-info pathname)))
                 ((member (pathname-type pathname) '("jpg" "png" "gif" "avi" "mp3" "mp4" "ogg") :test 'string=)
                  (mediainfo pathname))))))))))

(defun pdf-info (pathname)
  (let* ((name (unescape-pathname pathname))
         (raw
          (iter (for line in
                     (handler-case
                         (run-program-to-string "pdfinfo" (list name) :split-lines t)
                       (error (c) (warn "Error in pdfinfo ~S." c))))
                (when-let (pos (position #\: line))
                  (collect
                      (list
                       (subseq line 0 pos)
                       (string-left-trim '(#\space) (subseq line (1+ pos)))))))))
    (values
     (car (assoc-value raw "Title" :test 'equal))
     (car (assoc-value raw "Author" :test 'equal))
     (when-let (string (car (assoc-value raw "Pages" :test 'equal)))
       (parse-integer string)))))
