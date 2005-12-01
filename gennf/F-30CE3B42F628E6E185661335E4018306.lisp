;; Common Lisp gpgme-wrapper
;; Author: Hannes Mehnert, Florian Lorenz, Fabian Otto
;; Date: Thu Dec  1 17:26:32 MET 2005


;; This wrapper is used in our meta-cvs patch, for signinig code hunks/sets. 

;; Example FFI call
;; (ffi:def-call-out dot-product
;;          (:name "dot_product")
;;          (:language :stdc)
;;          (:arguments
;;           (a (ffi:c-ptr (ffi:c-array single-float 3)))
;;           (b (ffi:c-ptr (ffi:c-array single-float 3))))
;;          (:return-type single-float))


