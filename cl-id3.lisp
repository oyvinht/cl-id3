;;; The ID3 algorithm implemented in Common Lisp.
;;
;; Author: Øyvin Halfdan Thuv - oyvinht@pvv.ntnu.no
;;
;; Copyright: See attached (BSD) license
;;
;;;----------------------------------------------------------------------------

(in-package :id3)


;; Parameters
;;-----------
(defparameter *test-function* #'equalp "Used for doing all value comparisons")


;; Utilities
;;----------
(defun subset (examples attribute value)
  "Return a list of the 'examples' that have 'value' for 'attribute'."
  (loop for example in examples
     if (funcall *test-function* (getf example attribute) value)
     collect example))


(defun attribute-value-counts (attribute examples &key values-only)
  "Returns an alist of attribute values and their count (or just the values)."
  (let ((counts))
    (dolist (example examples)
      (let ((value (getf example attribute)))
        (if values-only
            (unless (member value counts :test *test-function*)
              (push value counts))
	    (if (assoc value counts :test *test-function*)
		(incf (cdr (assoc value counts :test *test-function*)))
		(push (cons value 1) counts)))))
    counts))


(defun plist-keys (list-of-plists)
  "Collect all possible plist keys from list-of-plists."
  (let ((keys))
    (dolist (plist list-of-plists)
      (dolist (element plist)
	(when (keywordp element)
	  (pushnew element keys))))
    keys))
	   


;; ID3 functions
;;--------------
(defun entropy (examples target-attribute)
  "A number between 0 and 1 that descr. the hetereogenity of examples
   with regard to the target-attribute."
  (let ((distinct-values (attribute-value-counts
			  target-attribute examples :values-only t))
        (num-examples (length examples)))
    ;; Sum together [- [proport. of i] times log_2 [proport. i]] 
    (loop for value in distinct-values
          sum (let ((prop-i (/ (length (subset examples target-attribute value))
                               num-examples)))
                (- (* prop-i (log prop-i 2)))))))


(defun info-gain (attribute examples target-attribute)
  "Calculates the expected reduction in entropy by classifying on 'attribute'."
  (- (entropy examples target-attribute)
     (loop for value in (attribute-value-counts 
			 attribute examples :values-only t)
	sum (let ((value-subset (subset examples attribute value)))
	      (* (/ (length value-subset) (length examples))
		 (entropy value-subset target-attribute))))))


(defun most-info-gaining-attribute (attributes examples target-attribute)
  "Return the attribute that best separates examples."
  (first ; Return the value ...
   (first ; who is ordered first ...
    (sort ; when sorting ...
     (loop for attribute in attributes ; the gain of attributes ...
	collect (cons attribute 
		      (info-gain attribute examples target-attribute)))
     #'(lambda (one two) (> (cdr one) (cdr two))))))) ; by best gain.


(defun most-common-attribute-value (attribute examples)
  "Return the most common value of 'attr' in 'examples'."
  (first ; Return the value ...
   (first ; who is ordered first ...
    (sort ; when sorting ...
     (attribute-value-counts attribute examples)
     #'(lambda (one two) (> (cdr one) (cdr two))))))) ; ... by count.


(defun id3 (examples target-attribute
	    &key attributes (positive-value 'yes) (negative-value 'no))
  "Induce tree for deciding the target-attribute (only for 'attrs', if given)."
  (let* ;; Unless specific 'attributes' are provided, include all but target
      ((attributes (if attributes
		       attributes
		       (remove target-attribute (plist-keys examples))))
       ;; The amount of positive examples
       (num+ (length (subset examples target-attribute positive-value))))
    (cond 
     ;; If all examples are positive -> return the root labeled 'positive-value'
     ((= num+ (length examples)) positive-value)
     ;; -- no  -- || --                                         'negative-value'
     ((= num+ 0) negative-value)
     ;; If attrs is empty -> return the root labeled with
     ;; the most common value of 'target-attr'
     ((null attributes) (most-common-attribute-value target-attribute examples))
     ;; Otherwise ...
     (t
      (let* (;; Pick the value with highest info gain.
             (attribute (most-info-gaining-attribute attributes
						     examples target-attribute))
	     ;; Fetch all possible values for it
             (attribute-values (attribute-value-counts 
				attribute examples :values-only t)))
        ;; Create a node labeled by this attribute ...
        (cons
	 attribute
	 (loop for value in attribute-values
	    collect 
	      (let ((subset (subset examples attribute value)))
		(cons value ; with subtrees for each value.
		      (if (null subset)
			  (most-common-attribute-value
			   target-attribute examples)
			  (list (id3 subset target-attribute 
				     :attributes (remove attribute attributes)
				     :positive-value positive-value
				     :negative-value negative-value))))))))))))


;; Easy to use interfaces for utilizing the ID3 tree
;;--------------------------------------------------
(defun classify (instance decision-tree)
  "Classify instance given decision tree."
  (cond 
    ;; At a leaf -> Return the leaf (answer)
    ((atom (cadr decision-tree))
     (cadr decision-tree))
    ;; At a question? -> Continue with subtree below correct answer
    ((keywordp (first decision-tree))
     (classify 
      instance
      (assoc (getf instance (first decision-tree)) (rest decision-tree))))
    ;; At an answer ->
    ((listp (rest decision-tree))
     (classify
      instance
      (assoc (getf instance (caar (rest decision-tree)))
	     (rest (first (rest decision-tree))))))))


(defun build-id3-classifier (examples target-attribute &key attributes
			     positive-value negative-value)
  "Returns a function that classifies an instance using the decision tree"
  ;; Encapsulate 'classify' within a closure with decision tree given
  (let ((decision-tree (id3 examples target-attribute :attributes attributes
			    :positive-value positive-value
			    :negative-value negative-value)))
    (lambda (instance)
      (classify instance decision-tree))))
