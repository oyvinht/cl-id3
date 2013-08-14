;; ********************
;; * Example usage 1: *
;; ********************
;; ID3> (setf (symbol-function 'classification-of)
;;            (build-id3-classifier *examples* :play-tennis))
;; ID3> #<CCL:COMPILED-LEXICAL-CLOSURE #x300041465D6F>
;; ID3> (classification-of '(:wind strong :outlook sunny :humidity high))
;;
;; ********************
;; * Example usage 2: *
;; ********************
;; ID3> (setf *dt* (id3 *examples* :play-tennis))
;; ID3> #<list>
;; ID3> (classify '(:outlook rain :wind weak) *dt*)
;; ID3> YES
;;
;;;----------------------------------------------------------------------------


(defpackage #:id3-examples
  (:use "ID3" "COMMON-LISP" "CL-USER"))
(in-package :id3-examples)


(defparameter
    *examples*
 '((:outlook sunny    :temp hot  :humidity high   :wind weak   :play-tennis no)
   (:outlook sunny    :temp hot  :humidity high   :wind strong :play-tennis no)
   (:outlook overcast :temp hot  :humidity high   :wind weak   :play-tennis yes)
   (:outlook rain     :temp mild :humidity high   :wind weak   :play-tennis yes)
   (:outlook rain     :temp cool :humidity normal :wind weak   :play-tennis yes)
   (:outlook rain     :temp cool :humidity normal :wind strong :play-tennis no)
   (:outlook overcast :temp cool :humidity normal :wind strong :play-tennis yes)
   (:outlook sunny    :temp mild :humidity high   :wind weak   :play-tennis no)
   (:outlook sunny    :temp cool :humidity normal :wind weak   :play-tennis yes)
   (:outlook rain     :temp mild :humidity normal :wind weak   :play-tennis yes)
   (:outlook sunny    :temp mild :humidity normal :wind strong :play-tennis yes)
   (:outlook overcast :temp mild :humidity high   :wind strong :play-tennis yes)
   (:outlook overcast :temp hot  :humidity normal :wind weak   :play-tennis yes)
   (:outlook rain     :temp mild :humidity high   :wind strong :play-tennis no))
  "Examples for testing. Play tennis or not? That's the question.")


(defun example-1 ()
  "Build and use a classifier function."
  ;; Create the classifier function
  (setf (symbol-function 'classification-of) 
	(build-id3-classifier *examples* :play-tennis
			      :positive-value 'yes :negative-value 'no))
  ;; Make a couple of test instances
  (let ((instances '((:wind strong :outlook sunny :humidity high)
		     (:outlook sunny :humidity normal :wind weak))))
    ;; Try to classify them
    (loop for instance in instances
       do (format t "Adding classification of instance ~A~%" instance)
       collect (classification-of instance))))


(defun example-2 ()
  "Build a tree. Then traverse it with a tree-traverser."
  ;; Build the tree
  (let ((decision-tree (id3 *examples* :play-tennis
			    :positive-value 'yes :negative-value 'no))
	;; Make a couple of test instances
	(instances '((:wind strong :outlook sunny :humidity high)
		     (:outlook sunny :humidity normal :wind weak))))
    ;; Try to classify the instances using the tree
    (loop for instance in instances
       do (format t "Adding classification of instance ~A~%" instance)
       collect (classify instance decision-tree))))
