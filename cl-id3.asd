;;;; Hey, Emacs! This is a -*- Lisp -*- file!
(defsystem cl-id3
    :name "CL-ID3"
    :version 1.0
    :components 
    ((:file "defpackage")
     (:file "cl-id3" :depends-on ("defpackage"))))

