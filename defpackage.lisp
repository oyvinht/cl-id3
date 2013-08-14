;;; Package definition
;;;-------------------
(defpackage #:INDUCTION-OF-DECISION-TREES
  (:use "COMMON-LISP")
  (:nicknames "ID3")
  (:export "*TEST-FUNCTION*" "ID3" "CLASSIFY" "BUILD-ID3-CLASSIFIER"))

(in-package "ID3")

