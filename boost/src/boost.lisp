(in-package :clml.boost)

(defstruct model
  (sub-models nil :type list)
  (sub-model-evaluators nil :type list)
  (sub-model-weights nil :type list)
  (variable-index-hash nil :type (or null hash-table)))

(defmethod print-object ((m model) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (m stream :type t :identity nil)
			 (format stream "WITH ~d SUB-MODEL~:*~:@(~P~)" (length (model-sub-models m))))))

(defmacro make-dataset-answer-extractor (objective-column-index)
  `(lambda (dataset) (map 'list (lambda (x) (svref x ,objective-column-index)) (dataset-points dataset))))

(defun dataset-answer-extractor (objective-column-index dataset &key (data-vector (dataset-points dataset)) &allow-other-keys)
  (map 'list (lambda (x) (svref x objective-column-index)) data-vector))

(defun evaluate-model (model query &key &allow-other-keys)
  (declare (type model model))
  (reduce #'+ (mapcar (lambda (e m w) (* w (funcall e m query :variable-index-hash (model-variable-index-hash model)))) (model-sub-model-evaluators model) (model-sub-models model) (model-sub-model-weights model))))

(defun p-evaluate-model (model query &key &allow-other-keys)
  (declare (type model model))
  (reduce #'+ (pmapcar (lambda (e m w) (* w (funcall e m query :variable-index-hash (model-variable-index-hash model)))) (model-sub-model-evaluators model) (model-sub-models model) (model-sub-model-weights model))))

(defun map-model (model dataset)
  (declare (type model model))
  (map 'list (lambda (v) (evaluate-model model v)) (dataset-points dataset)))

(defun squared-error-functional-derivative (correct-value model-result)
  (declare (type number model-result correct-value))
  (-
   correct-value
   model-result))

(defun one-dimensional-gradient-descent (&key (model-results nil model-results-p) new-sub-model new-sub-model-evaluator dataset data-vector dataset-answers (loss-functional-derivative #'squared-error-functional-derivative) (initial-guess 1.0) (precision 1.0f-5) (step-size 1.0f-3) (model nil) &allow-other-keys)
  (declare (ignore loss-functional-derivative) (type model model))
  (let ((model-results (if model-results-p model-results (map-model model dataset)))
	(new-model-results (map 'list (lambda (v) (funcall new-sub-model-evaluator new-sub-model v :variable-index-hash (model-variable-index-hash model))) (if data-vector data-vector (dataset-points dataset)))))
    (iter
      (for old previous new initially initial-guess)
      (for new = (- old (* step-size (reduce #'+ (mapcar (lambda (y mr nmr) (* -1 nmr (- y mr (* old nmr)))) dataset-answers model-results new-model-results)))))
      (if (< (abs (- new old)) precision) (return new))
      (finally (return new)))))

(defmacro make-base-learner-method (&body body)
  `(lambda (&key model-results dataset dataset-answers data-vector objective-column-index loss-functional-derivative model &allow-other-keys)
     (declare (list model-results dataset-answers)
	      (function loss-functional-derivative)
	      (type model model)
	      (ignorable dataset model))
     (let ((data-vector
	    (map 'vector
		 (lambda (v correct-value model-result)
		   (let ((new-v (copy-seq v)))
		     (setf (svref new-v objective-column-index) (funcall loss-functional-derivative correct-value model-result))
		     new-v))
		 data-vector
		 dataset-answers
		 model-results)))
       ,@body)))

(defmacro gradient-boost (model dataset base-learner-method objective-column-index &key (data-vector nil) (dataset-answers nil) (loss-functional-derivative #'squared-error-functional-derivative) (one-dimensional-optimizer #'one-dimensional-gradient-descent) (loss-functional nil loss-functional-p) (shrinkage nil))
  (assert (or loss-functional-derivative loss-functional-p))
  `(let* ((data-vector ,(if data-vector data-vector `(dataset-points ,dataset)))
	  (model-results (map-model ,model ,dataset))
	  (dataset-answers ,(if dataset-answers dataset-answers `(dataset-answer-extractor ,objective-column-index ,dataset :data-vector data-vector))))
     (multiple-value-bind (base-model base-model-evaluator)
	 (funcall ,base-learner-method :model-results model-results :dataset ,dataset :dataset-answers dataset-answers :data-vector data-vector :objective-column-index ,objective-column-index :loss-functional-derivative ,loss-functional-derivative :loss-functional ,loss-functional :model ,model)
       (let ((base-model-weight (funcall ,one-dimensional-optimizer :model-results model-results :new-sub-model base-model :new-sub-model-evaluator base-model-evaluator :dataset ,dataset :data-vector data-vector :dataset-answers dataset-answers :loss-functional-derivative ,loss-functional-derivative :loss-functional ,loss-functional :model ,model)))
	 (push base-model (model-sub-models ,model))
	 (push base-model-evaluator (model-sub-model-evaluators ,model))
	 (push ,(if shrinkage `(* ,shrinkage base-model-weight) 'base-model-weight) (model-sub-model-weights ,model))))
     ,model))

(defun make-adaboost-weight-vector (&key dataset data-vector len &allow-other-keys)
  (let ((len (if len len (if data-vector (length data-vector) (length (dataset-points dataset))))))
    (make-array len :initial-element (/ 1.0 len))))

(defun adaboost-total-weighted-error (classifier classifier-evaluator weight-vector dataset-answers &key variable-index-hash &allow-other-keys)
  (reduce #'+
	  (map 'list
	       (lambda (weight answer)
		 (if (=
		      (signum answer)
		      (signum (funcall classifier-evaluator classifier :variable-index-hash variable-index-hash)))
		     0.0
		     weight))
	       weight-vector
	       dataset-answers)))

(defun adaboost-total-weighted-error-precompute (classifier-answers weight-vector dataset-answers)
  (reduce #'+
	  (map 'list
	       (lambda (ca w da)
		 (if (=
		      (signum da)
		      (signum ca))
		     0.0
		     w))
	       classifier-answers
	       weight-vector
	       dataset-answers)))

(defun adaboost-error-rate->weight (error-rate)
  (/ (log (/ (- 1 error-rate) error-rate)) 2))

(defun normalize-vector (vector)
  (let ((norm-constant (reduce #'+ vector)))
    (map-into vector (lambda (v) (/ v norm-constant)) vector)))

(defun adaboost-update-weight-vector (weight-vector classifier classifier-evaluator weight dataset-answers &key variable-index-hash &allow-other-keys)
  (normalize-vector
   (map-into weight-vector (lambda (w answer) (* w (exp (* -1 answer weight (funcall classifier-evaluator classifier :variable-index-hash variable-index-hash))))) weight-vector dataset-answers)))

(defun adaboost-update-weight-vector-precompute (weight-vector classifier-answers weight dataset-answers)
  (normalize-vector
   (map-into weight-vector (lambda (w ca da)
			     (* w (exp (* -1 da weight ca)))) weight-vector classifier-answers dataset-answers)))

(defun adaboost-from-list->model (weak-classifier-list weak-classifier-evaluator-list dataset objective-column-index &key variable-index-hash (dataset-answer-remap nil))
  (let* ((data-vector (dataset-points dataset))
	 (weight-vector (make-adaboost-weight-vector :data-vector data-vector :dataset dataset))
	 (model (make-model :variable-index-hash variable-index-hash))
	 (dataset-answers (if dataset-answer-remap (funcall dataset-answer-remap #1=(dataset-answer-extractor objective-column-index dataset)) #1#))
	 (wcl (pmapcar
	       (lambda (wc wce)
		 (list
		  (map 'list
		       (lambda (v)
			 (funcall wce wc v :variable-index-hash variable-index-hash))
		       data-vector)
		  wc
		  wce))
	       weak-classifier-list
	       weak-classifier-evaluator-list)))
    (do () ((null wcl) model)
      (iter
	(with best-wc = (first wcl))
	(with best-wc-e = (adaboost-total-weighted-error-precompute (first (first wcl)) weight-vector dataset-answers))
	(for c in (rest wcl))
	(let ((wc-e (adaboost-total-weighted-error-precompute (first c) weight-vector dataset-answers)))
	  (if (< wc-e best-wc-e)
	      (setf best-wc-e wc-e best-wc c)))
	(finally (let ((weight (adaboost-error-rate->weight best-wc-e)))
		   (push (second best-wc) (model-sub-models model))
		   (push (third best-wc) (model-sub-model-evaluators model))
		   (push weight (model-sub-model-weights model))
		   (setf weight-vector (adaboost-update-weight-vector-precompute weight-vector (first best-wc) weight dataset-answers))
		   (setf wcl (delete best-wc wcl :test #'equal :count 1))))))))

(defmacro make-dataset-answer-remap (&rest remaps)
  `(lambda (y)
     (map 'list
	  (lambda (x)
	    (second (find x ',remaps :key #'first :test #'equal))))))
