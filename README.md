cl-id3
======

A Common Lisp implementation of the ID3 machine learning algorithm by R. Quinlan.

It generates decision trees from example instances that have discrete variables. Further, it can generate a function that traverses that tree to classify new instances from outside the training data (e.g. where one variable is missing).

Here's an example of how to use it:

```common-lisp
CL-USER> 
(defpackage #:id3-examples
  (:use "ID3" "COMMON-LISP" "CL-USER"))
Package "ID3-EXAMPLES"

CL-USER> (in-package :id3-examples)
Package "ID3-EXAMPLES"

ID3-EXAMPLES> 
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

*EXAMPLES*

ID3-EXAMPLES> 
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

EXAMPLE-1

ID3-EXAMPLES> 
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

EXAMPLE-2

ID3-EXAMPLES> (example-1)
Adding classification of instance (WIND STRONG OUTLOOK SUNNY HUMIDITY HIGH)
Adding classification of instance (OUTLOOK SUNNY HUMIDITY NORMAL WIND WEAK)
(NO YES)

ID3-EXAMPLES> (example-2)
Adding classification of instance (WIND STRONG OUTLOOK SUNNY HUMIDITY HIGH)
Adding classification of instance (OUTLOOK SUNNY HUMIDITY NORMAL WIND WEAK)
(NO YES)

ID3-EXAMPLES>
```
