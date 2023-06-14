;;;; Hey, Emacs! This is a -*- Lisp -*- file!
(defsystem cl-id3
    :author "Øyvin Halfdan Thuv"
    :description "A Common Lisp implementation of the ID3 machine learning algorithm by R. Quinlan."
    :license "BSD-2-Clause"
    :name "CL-ID3"
    :version 1.0
    :components
    ((:file "defpackage")
     (:file "cl-id3" :depends-on ("defpackage"))))
