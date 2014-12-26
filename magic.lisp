(in-package :indexer)

(define-foreign-library libmagic (t (:or "libmagic.so.1" "libmagic.so")))

(load-foreign-library 'libmagic)

(defctype magic_t :pointer)

(defbitfield magic-flags
  (:none              #x000000)
  (:debug             #x000001)
  (:symlink           #x000002)
  (:compress          #x000004)
  (:devices           #x000008)
  (:mime-type         #x000010)
  (:continue          #x000020)
  (:check             #x000040)
  (:preserve-atime    #x000080)
  (:raw               #x000100)
  (:error             #x000200)
  (:mime-encoding     #x000400)
  (:no-check-compress #x001000)
  (:no-check-tar      #x002000)
  (:no-check-soft     #x004000)
  (:no-check-apptype  #x008000)
  (:no-check-elf      #x010000)
  (:no-check-ascii    #x020000)
  (:no-check-troff    #x040000)
  (:no-check-tokens   #x100000))

(defcfun "magic_open" magic_t (flags magic-flags))
(defcfun "magic_load" :int (cookie magic_t) (filename :pointer))
(defcfun ("magic_buffer" %magic-buffer) :string (cookie magic_t) (buffer :pointer) (size :int))
(defcfun ("magic_file" %magic-file) :string (cookie magic_t) (filename :string))

(defparameter *magic-cookie* nil)
(defparameter *magic-mime-cookie* nil)

(defun initialize-magic ()
  (setf *magic-cookie* (magic-open '())
        *magic-mime-cookie* (magic-open '(:mime-type)))
  (unless (zerop (magic-load *magic-cookie* (null-pointer)))
    (error "magic-load failure."))
  (unless (zerop (magic-load *magic-mime-cookie* (null-pointer)))
    (error "magic-load failure.")))

(defun $magic-buffer (cookie buffer)
  (cffi:with-pointer-to-vector-data (buf buffer)
    (%magic-buffer cookie buf (length buffer))))

(defun magic-buffer (data)
  (values ($magic-buffer *magic-cookie* data)
          ($magic-buffer *magic-mime-cookie* data)))

(defun magic-file (filename)
  (values (%magic-file *magic-cookie* filename)
          (%magic-file *magic-mime-cookie* filename)))

(defun magic (el)
  (typecase el
    (pathname (magic-file (namestring el)))
    (vector (magic-buffer el))))
