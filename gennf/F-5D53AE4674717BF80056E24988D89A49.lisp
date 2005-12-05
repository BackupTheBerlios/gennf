;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(push :clisp-unix-funcs *features*)

(defpackage :unix-funcs 
  (:use :common-lisp)
  (:shadowing-import-from :ffi 
    :def-call-out :def-c-struct :c-array-max :c-pointer :c-ptr :c-string :int
    :uint :ulong :boolean :character :c-array-ptr)
  (:shadow
    :open :close)
  (:intern
    :def-c-call-out)
  (:export 
    :null-pointer-p :errno :strerror :eperm :enoent :esrch :eintr :eio
    :enxio :e2big :enoexec :ebadf :echild :eagain :enomem :eacces
    :efault :enotblk :ebusy :eexist :exdev :enodev :enotdir :eisdir
    :einval :enfile :emfile :enotty :etxtbsy :efbig :enospc :espipe
    :erofs :emlink :epipe :edom :erange :edeadlk :enametoolong :enolck
    :enosys :enotempty :eloop :ewouldblock :dirent :opendir :closedir
    :readdir :ino :name :open :close :chdir :fchdir :link :symlink
    :readlink :unlink :rmdir :stat :stat :lstat :fstat :chmod
    :mode :nlink :uid
    :gid :rdev :blksize :blocks :atime :mtime :ctime :s-ifmt :s-ifdir
    :s-ifchr :s-ifblk :s-ifreg :s-ififo :s-iflnk :s-ifsock :s-isdir
    :s-ischr :s-isblk :s-isreg :s-isfifo :s-islnk :s-issock :s-isuid
    :s-isgid :s-isvtx :s-iread :s-iwrite :s-iexec :s-irusr :s-iwusr
    :s-ixusr :s-irwxu :s-irgrp :s-iwgrp :s-ixgrp :s-irwxg :s-iroth
    :s-iwoth :s-ixoth :s-irwxo :accessperms :deffilemode :o-accmode
    :o-rdonly :o-wronly :o-rdwr :o-creat :o-excl :o-noctty :o-trunc
    :o-append :o-nonblock :o-sync :o-async :o-ndelay :o-fsync :getcwd
    :run-program default-sigchld ctermid))

(in-package :unix-funcs)

(defmacro def-c-call-out (sym &body args)
  `(def-call-out ,sym (:language :stdc) ,@args))

#.(when (string> (lisp-implementation-version) "2.30" :end1 4 :end2 4)
    (push :clisp-newer-than-2.30 *features*)
    (values))

#+clisp-newer-than-2.30
  (eval-when (:compile-toplevel)
    (setf ffi:*output-c-functions* t)
    (setf ffi:*output-c-variables* t))

;;;
;;; Null pointer test, needed in CLISP 2.27 and older.
;;;

(def-c-call-out null-pointer-p
  (:name "mcvs_null_pointer_p")
  (:arguments (pointer c-pointer))
  (:return-type boolean))

;;;
;;; <errno.h>
;;;

(def-c-call-out get-errno
  (:name "mcvs_get_errno")
  (:arguments)
  (:return-type int))

(def-c-call-out set-errno
  (:name "mcvs_set_errno")
  (:arguments (value int))
  (:return-type int))

(defsetf get-errno set-errno)
(define-symbol-macro errno (get-errno))

(def-c-call-out strerror
  (:arguments (errnum int))
  (:return-type c-string :none))

(defconstant eperm 1)
(defconstant enoent 2)
(defconstant esrch 3)
(defconstant eintr 4)
(defconstant eio 5)
(defconstant enxio 6)
(defconstant e2big 7)
(defconstant enoexec 8)
(defconstant ebadf 9)
(defconstant echild 10)
(defconstant eagain 11)
(defconstant enomem 12)
(defconstant eacces 13)
(defconstant efault 14)
(defconstant enotblk 15)
(defconstant ebusy 16)
(defconstant eexist 17)
(defconstant exdev 18)
(defconstant enodev 19)
(defconstant enotdir 20)
(defconstant eisdir 21)
(defconstant einval 22)
(defconstant enfile 23)
(defconstant emfile 24)
(defconstant enotty 25)
(defconstant etxtbsy 26)
(defconstant efbig 27)
(defconstant enospc 28)
(defconstant espipe 29)
(defconstant erofs 30)
(defconstant emlink 31)
(defconstant epipe 32)
(defconstant edom 33)
(defconstant erange 34)
(defconstant edeadlk 35)
(defconstant enametoolong 36)
(defconstant enolck 37)
(defconstant enosys 38)
(defconstant enotempty 39)
(defconstant eloop 40)
(defconstant ewouldblock eagain)

;;;
;;; <dirent.h>
;;;

(def-c-struct dirent
  (ino ulong)
  (name (c-array-max character 1024)))

(def-c-call-out opendir 
  (:arguments (name c-string))
  (:return-type c-pointer))

(def-c-call-out closedir 
  (:arguments (dirp c-pointer))
  (:return-type int))

(def-c-call-out readdir
  (:name "mcvs_readdir")
  (:arguments (dirp c-pointer))
  (:return-type (c-ptr dirent)))

;;;
;;; <unistd.h> -- open, close
;;;

(def-c-call-out open 
  (:arguments (name c-string) 
	      (flags int) 
	      (mode uint))
  (:return-type int))

(def-c-call-out close 
  (:arguments (fd int))
  (:return-type int))

;;;
;;; <unistd.h> -- chdir, fchdir
;;;

(def-c-call-out chdir
  (:arguments (path c-string))
  (:return-type int))

(def-c-call-out fchdir
  (:arguments (fd int))
  (:return-type int))

;;;
;;; <unistd.h> -- link, symlink, readlink, unlink, rmdir


(def-c-call-out link 
  (:arguments (from c-string)
	      (to c-string))
  (:return-type int))

(def-c-call-out symlink 
  (:arguments (from c-string)
	      (to c-string))
  (:return-type int))

(def-c-call-out readlink
  (:name "mcvs_readlink")
  (:arguments (path c-string))
  (:return-type c-string :malloc-free))

(def-c-call-out unlink
  (:arguments (path c-string))
  (:return-type int))

(def-c-call-out rmdir 
  (:arguments (path c-string))
  (:return-type int))

;;;
;;; <unistd.h> -- stat, lstat, chmod
;;;

(def-c-struct stat
  (dev ulong)
  (ino ulong)
  (mode ulong)
  (nlink uint)
  (uid uint)
  (gid uint)
  (rdev ulong)
  (blksize ulong)
  (blocks ulong)
  (atime ulong)
  (mtime ulong)
  (ctime ulong))

(def-c-call-out stat 
  (:name "mcvs_stat")
  (:arguments (name c-string) 
	      (buf (c-ptr stat) :out))
  (:return-type int))

(def-c-call-out lstat 
  (:name "mcvs_lstat")
  (:arguments (name c-string)
	      (buf (c-ptr stat) :out))
  (:return-type int))

(def-c-call-out fstat
  (:name "mcvs_fstat")
  (:arguments (fd int) 
	      (buf (c-ptr stat) :out))
  (:return-type int))

(def-c-call-out chmod
  (:arguments (name c-string)
	      (mode uint))
  (:return-type int))
	    
(defconstant s-ifmt   #o170000)
(defconstant s-ifdir  #o040000)
(defconstant s-ifchr  #o020000)
(defconstant s-ifblk  #o060000)
(defconstant s-ifreg  #o100000)
(defconstant s-ififo  #o010000)
(defconstant s-iflnk  #o120000)
(defconstant s-ifsock #o140000)

(defmacro s-isdir (m) `(= (logand ,m s-ifmt) s-ifdir))
(defmacro s-ischr (m) `(= (logand ,m s-ifmt) s-ifchr))
(defmacro s-isblk (m) `(= (logand ,m s-ifmt) s-ifblk))
(defmacro s-isreg (m) `(= (logand ,m s-ifmt) s-ifreg))
(defmacro s-isfifo (m) `(= (logand ,m s-ifmt) s-iffifo))
(defmacro s-islnk (m) `(= (logand ,m s-ifmt) s-iflnk))
(defmacro s-issock (m) `(= (logand ,m s-ifmt) s-ifsock))

(defconstant s-isuid  #o004000)
(defconstant s-isgid  #o002000)
(defconstant s-isvtx  #o001000)

(define-symbol-macro s-iread s-irusr)
(define-symbol-macro s-iwrite s-iwusr)
(define-symbol-macro s-iexec s-ixusr)

(defconstant s-irusr  #o000400)
(defconstant s-iwusr  #o000200)
(defconstant s-ixusr  #o000100)
(defconstant s-irwxu  (logior s-irusr s-iwusr s-ixusr))
(defconstant s-irgrp  #o000040)
(defconstant s-iwgrp  #o000020)
(defconstant s-ixgrp  #o000010)
(defconstant s-irwxg  (logior s-irgrp s-iwgrp s-ixgrp))
(defconstant s-iroth  #o000004)
(defconstant s-iwoth  #o000002)
(defconstant s-ixoth  #o000001)
(defconstant s-irwxo  (logior s-iroth s-iwoth s-ixoth))

(defconstant accessperms (logior s-irwxu s-irwxg s-irwxo))
(defconstant deffilemode (logior s-irusr s-iwusr s-irgrp s-iwgrp s-iroth s-iwoth))

;;;
;;; <unistd.h> -- getcwd
;;;

(def-c-call-out getcwd
  (:name "mcvs_getcwd")
  (:arguments)
  (:return-type c-string :malloc-free))

;;;
;;; <unistd.h> -- fork, wait*, exec*
;;;

(def-c-call-out default-sigchld
  (:name "mcvs_default_sigchld"))

(def-c-call-out spawn
  (:name "mcvs_spawn")
  (:arguments (name c-string)
	      (argv (c-array-ptr c-string) :in :malloc-free))
  (:return-type int))

(defun run-program (name &key arguments)
  (push name arguments)
  (spawn name (coerce arguments 'vector)))

;;;
;;; Terminal related functions
;;;

(def-c-call-out ctermid
  (:name "mcvs_ctermid")
  (:arguments)
  (:return-type c-string :malloc-free))

;;;
;;; <fcntl.h>
;;;

(defconstant o-accmode  #o00003)
(defconstant o-rdonly   #o00000)
(defconstant o-wronly   #o00001)
(defconstant o-rdwr     #o00002)
(defconstant o-creat    #o00100)
(defconstant o-excl     #o00200)
(defconstant o-noctty   #o00400)
(defconstant o-trunc    #o01000)
(defconstant o-append   #o02000)
(defconstant o-nonblock #o04000)
(defconstant o-sync     #o10000)
(defconstant o-async    #o20000)
(defconstant o-ndelay o-nonblock)
(defconstant o-fsync o-sync)
