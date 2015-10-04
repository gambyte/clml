(defpackage :clml.boost
  (:use :cl
	:clml.hjs.read-data
	:iterate
	:lparallel)
  (:documentation "Boost package")
  (:export
   #:make-model
   #:model-sub-models
   #:model-sub-model-evaluators
   #:model-sub-model-weights
   #:model-variable-index-hash
   #:evaluate-model
   #:p-evaluate-model
   #:map-model
   #:squared-error-functional-derivative
   #:one-dimensional-gradient-descent
   #:make-base-learner-method
   #:data-vector
   #:generic-boost
   #:adaboost-from-list->model
   #:make-dataset-answer-remap))
