;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

#+(or clisp cmu18c) (require "posix")

#+(and clisp clisp-unix-funcs) 
  (require "clisp-unix")

#+(and clisp (not clisp-unix-funcs)) 
  (require "clisp-linux")

#+cmu18c 
  (require "cmucl-unix")
