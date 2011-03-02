(asdf:operate 'asdf:load-op :asdf-objects)
(asdf:operate 'asdf:load-op :smarkup)

(defpackage #:opticl-examples-system (:use #:cl #:asdf #:asdf-objects #:smarkup))
(in-package #:opticl-examples-system)

(defclass opticl-examples-system (asdf:system) ())

(asdf:defsystem :opticl-examples
  :class opticl-examples-system
  :name "opticl-examples"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :depends-on (opticl smarkup)
  :components
  ((:smarkup-object-from-file :opticl-examples-sexp :pathname #p"opticl-examples.sexp")
   (:filtered-object :opticl-examples-filtered-sexp
                        :filters (:lisp :smarkup-metadata :ref)
                        :depends-on (:opticl-examples-sexp)
                        :input-object :opticl-examples-sexp)
     
   (:filtered-object :opticl-examples-pdf-filtered-sexp
                     :filters (:html-metadata)
                     :depends-on (:opticl-examples-filtered-sexp)
                     :input-object :opticl-examples-filtered-sexp)
   (:object-cl-pdf-file :opticl-examples-pdf
                           :pathname #p"opticl-examples.pdf"
                           :depends-on (:opticl-examples-pdf-filtered-sexp)
                           :input-object :opticl-examples-pdf-filtered-sexp)

   (:filtered-object :opticl-examples-html-filtered-sexp
                        :filters (:html-metadata)
                        :depends-on (:opticl-examples-filtered-sexp)
                        :input-object :opticl-examples-filtered-sexp)
   (:object-xhtml-file :opticl-examples-xhtml
                       :pathname #p"opticl-examples.xhtml"
                       :depends-on (:opticl-examples-html-filtered-sexp)
                       :input-object :opticl-examples-html-filtered-sexp)
   (:module "images")))


