;; association-rule algorithm

(in-package :clml.association-rule)

(defclass assoc-result-dataset ()
  ((rules :initarg :rules
          :accessor rules
          :accessor assoc-result-rules)
   (thresholds :initarg :thresholds :accessor thresholds)
   (rule-length :initarg :rule-length :accessor rule-length)
   (header :allocation :class
           :accessor header
           :accessor assoc-result-header
           :initform
           #("premise" "conclusion" "support" "confidence" "lift" "conviction")))
  (:documentation "- accessor:
  - rules :       extracted results      <list vector>
  - thresholds :  (support confidence lift conviction)
  - rule-length : maximum length of a rule  <integer>
- note: the vectors of extracted results are rules, they contain the following elements
  - "premise": the premise part of the rule, a list of unit rules
  - "conclusion": the conclusion part of the rule, a list of unit rules
  - "support", "confidence", "lift", "conviction": some helpfulness indices of the rule
  - unit rule (where length is 1), is represented as string "<column name> = <value>".")
  )

(defmethod print-object ((d assoc-result-dataset) stream)
  (print-unreadable-object (d stream :type t :identity nil))
  (format stream "~&THRESHOLDS: ~{~A ~A~^~T| ~}~%"
          (loop for index in '("SUPPORT" "CONFIDENCE" "LIFT" "CONVICTION")
              for val in (thresholds d)
              append `(,index ,val)))
  (format stream "~&RULE-LENGTH: ~A~%" (rule-length d))
  (format stream "~&RESULT: ~A RULES~%" (length (rules d))))
(defun make-assoc-result (rules support confidence lift conviction rule-length)
  (assert (notany #'minusp `(,support ,confidence ,lift ,conviction)))
  (assert (> rule-length 1))
  (make-instance 'assoc-result-dataset
    :rules rules :thresholds `(,support ,confidence ,lift ,conviction)
    :rule-length rule-length))

(defgeneric assoc-data-out (d stream
                           &optional control-string))
(defmethod assoc-data-out ((d assoc-result-dataset) stream
                           &optional (control-string "~S"))
  (let ((*print-level* nil)
        (*print-length* nil))
    (let ((header (header d))
          (rules-list (rules d)))
      (format stream control-string (cons header rules-list))))
  d)

;; this iterater use append for save "reversed normal order" made by above.
;; if you optimize this application, please think about ordering sequence.
;; i think, they are equal that <use reverse before pass to this> and <use append here>.
(defun map-separated-two-groups (bag fn &optional passed-1 passed-2)
  (if (null bag)
      (funcall fn passed-1 passed-2)
    (progn (map-separated-two-groups (cdr bag) fn (append passed-1 (list (car bag))) passed-2)
      (map-separated-two-groups (cdr bag) fn passed-1 (append passed-2 (list (car bag)))))))

;; atom-rule == (<label-string> . <category-value>)
;; rule == (<atom-rule> {<atom-rule>}*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(inline make-rule finalize-rule)))
(defun finalize-rule (atom-rule) ;; optional -- for visualize
  atom-rule
  ;(format nil "~A=~A" (car atom-rule) (cdr atom-rule))
  )
(defun finalize-rules (rule)
  (mapcar #'(lambda (x) (finalize-rule x)) rule))
(defun make-rule (conc pre sup conf lif conv)
  (make-array 6 :initial-contents (list (finalize-rules pre)
                                        (finalize-rules conc)
                                        sup conf lif conv)))

(defun rule-indexes (conc pre rule rule-occur total)
  (let ((conc-count (gethash conc rule-occur))
        (rule-count (gethash rule rule-occur))
        (pre-count  (gethash pre  rule-occur)))
    (values (* (/ rule-count total) 100.0) ;; support
            (* (/ rule-count pre-count) 100.0) ;; confident
            (float (/ (/ rule-count pre-count) (/ conc-count total))) ;; lift
            (let ((negative (- pre-count rule-count)))
              (if (zerop negative)
                  most-positive-single-float
                (float (/ (* pre-count (- 1 (/ conc-count total))) (- pre-count rule-count))))) ;; conviction
            )))

(defun apriori-itemset-counting (transactions item-order support rule-length)
  (let* ((itemset-hash (make-hash-table :test #'equal))
         (total-transaction (hash-table-count transactions))
         (count-threshold (max 1 (* total-transaction (/ support 100)))))
    (flet ((prune-itemset-hash (itemsets)
             (loop for itemset in itemsets
                 as num = (gethash itemset itemset-hash)
                 if (and (numberp num) (>= num count-threshold))
                 collect itemset
                 else 
                 do (remhash itemset itemset-hash))))
      ;; first step, generate 1-itemsets
      (loop for trans being the hash-value in transactions
          do (loop for item in trans
                 as itemset = (cons item nil)
                 do (incf (gethash itemset itemset-hash 0))))
      ;; frequent itemset generation loop
      (loop for k from 2 to rule-length
          as itemsets =
            (if (= k 2)
                (gen-next-itemsets
                 (sort 
                  (loop for itemset being the hash-key in itemset-hash
                      for num being the hash-value in itemset-hash
                      if (>= num count-threshold)
                      collect itemset
                      else 
                      do (remhash itemset itemset-hash))
                  #'< :key #'(lambda (itemset)
                               (gethash (car itemset) item-order))))
              (gen-next-itemsets itemsets))
          do
            (loop for trans being the hash-value in transactions
                do (loop for itemset in itemsets
                       when (find-in-sorted-list itemset trans)
                       do (incf (gethash itemset itemset-hash 0))))
            (setq itemsets (prune-itemset-hash itemsets))
          finally (return itemset-hash)))))

(defun find-in-sorted-list (sorted-items sorted-list &key (test #'equal))
  (loop for item in sorted-items
      as search-result = (member item sorted-list :test test)
      if search-result
      do (setq sorted-list (cdr search-result))
      else return nil
      finally (return t)))

(defun match-except-tail (list1 list2 &key (test #'eql))
  (loop for sub1 on list1
      for sub2 on list2
      while (and (cdr sub1) (cdr sub2))
      unless (funcall test (car sub1) (car sub2))
      do (return)
      finally (return t)))

(defun gen-next-itemsets (pre-itemsets)
  (let (next-itemsets)
    (do ((itemsets pre-itemsets (cdr itemsets)))
	((null (cdr itemsets)) (nreverse next-itemsets))
      (loop for itemset in (cdr itemsets)
          as last-i = (car (last itemset))
          with target = (car itemsets)
          with last-t = (car (last target))
          when (and (not (equal last-i last-t))
		    (match-except-tail itemset target :test #'equal))
          do (push `(,@target ,last-i) next-itemsets)))))

;; fcn. for obtaining the transactions and the order of items
(defun scan-input-data (unsp-dataset target-variables key-variable)
  (let ((transactions (make-hash-table :test #'equal))
        (item-order (make-hash-table :test #'equal))
        (order 0)
        (targets (choice-dimensions
                  target-variables unsp-dataset))
        (keys (choice-a-dimension key-variable unsp-dataset)))
    (do-vecs ((target targets)
              (k keys))
      (loop for x across target
          for l in target-variables
          for item = (cons l x) do
            (unless (gethash item item-order)
              (setf (gethash item item-order) (incf order)))
            (pushnew item (gethash k transactions nil) :test #'equal)))
    (maphash #'(lambda (key items)
                 (setf (gethash key transactions)
                   (sort items #'< :key #'(lambda (item)
                                             (gethash item item-order)))))
             transactions)
    (values transactions item-order)))


;; ap-genrule
;; pass fn such that push rule into some variable to this ap-maprule
;; count-lookup-fn: lookup itemset count
(defun ap-maprule (fn parent-itemset itemset-length count-lookup-fn max-precount
		   &optional (set-of-itemsets (mapcar #'(lambda (x) (list x)) parent-itemset))
			     (set-of-itemsets-length 1))
  (dolist (conc set-of-itemsets)
    (let ((pre (ordered-set-difference parent-itemset conc)))
      (if (<= (funcall count-lookup-fn pre) max-precount)
	  (funcall fn conc pre)
	(setf set-of-itemsets (delete conc set-of-itemsets)))))
  (when (and (> itemset-length (1+ set-of-itemsets-length)) set-of-itemsets)
    (ap-maprule fn parent-itemset itemset-length count-lookup-fn max-precount
		(gen-next-itemsets set-of-itemsets) (1+ set-of-itemsets-length))))

(defun ordered-set-difference (sorted-item1 sorted-item2 &key (test #'equal))
  (loop for item in sorted-item1
      as search-result = (member item sorted-item2 :test test)
      unless search-result
	     collect item))

;; if confident == 0, then max-count is most-positive-fixnum
(defun confident->max-precount (rule-count confident)
  (if (zerop confident)
      most-positive-fixnum
    (* (/ rule-count confident) 100.0)))

(defun %association-analyze-ap-genrule (unsp-dataset target-variables key-variable rule-length
					&key (support 0) (confident 0) (lift 0) (conviction 0))
  "association analyze with ap-genrule algorithm.
- return: assoc-result-dataset
- arguments:
  - unsp-dataset: <unspecialized-dataset>
  - target-variables : (list of string) column names
  - key-variable : <string> column name for determining identities
  - rule-length : <integer> >= 2, maximum length for a rule
  - support : <number> for percentage
  - confident : <number> for percentage
  - lift : <number> beyond 0
  - conviction : <number> beyond 0
"
  (assert (and (<= 0 support 100) (<= 0 confident 100) (<= 0 lift) (<= 0 conviction)))
  (assert (and (integerp rule-length) (<= 2 rule-length)))
  (multiple-value-bind (transactions item-order)
      (scan-input-data unsp-dataset target-variables key-variable)
    (let ((rule-occur (apriori-itemset-counting
                       transactions item-order support rule-length)))
      (let ((ans nil)
	    (count (hash-table-count transactions)))
	(maphash #'(lambda (rule rule-count)
		     (let ((rule-length (length rule)))
		       (when (> rule-length 1)
			 (ap-maprule
			  #'(lambda (conc pre)
			      (multiple-value-bind (sup conf lif conv) (rule-indexes conc pre rule
										 rule-occur count)
				(when (and (>= sup support) (>= conf confident) (>= lif lift) (>= conv conviction))
				  (push (make-rule conc pre
						   sup conf lif conv) ans))))
			  rule (length rule)
			  #'(lambda (itemset) (gethash itemset rule-occur))
			  (confident->max-precount rule-count confident)))))
		 rule-occur)
        (make-assoc-result ans support confident 
			   lift conviction rule-length)))))

(defun gen-next-itemset-trie (pre-itemsets)
  (let ((next-trie (cons nil nil)))
    (do ((itemsets pre-itemsets (cdr itemsets)))
	((null (cdr itemsets)) next-trie)
      (loop for itemset in (cdr itemsets)
	  as last-i = (car (last itemset))
	  with target = (car itemsets)
	  with last-t = (car (last target))
	  when (and (not (equal last-i last-t))
		    (match-except-tail itemset target :test #'equal))
	  do (assign-trie target last-i next-trie)))))

;; apriori counting trie has leaf at only last-i
(defun assign-trie (target last-i root)
  (loop for i in target do
	(setf root (let ((found (find
				 i (cdr root) :key #'car :test #'equal)))
		     (if found
			 found
		       (let ((new (cons i nil)))
			 (push new (cdr root))
			 new)))))
  ;; never found last-i in last-leaf
  (push (cons last-i 0) (cdr root)))

(defun update-trie-count-apriori (trie transaction)
  (let ((remain (member (car trie) transaction :test #'equal)))
    (when remain
      (if (consp (cdr trie))
	  (loop for branch in (cdr trie) do
		(update-trie-count-apriori branch remain))
	(incf (cdr trie))))))

;; push itemset into some variable by fn
;; reversed twice (1: building counting-trie, 2: accumrating by fn)
;; so order of itemsets is protected as a result.
(defun dump-itemset-hash (trie itemset-hash minimum-count fn
			  &optional (passed nil))
  (if (consp (cdr trie))
      (loop for branch in (cdr trie) do
	    (dump-itemset-hash branch itemset-hash minimum-count fn
			       (cons (car trie) passed)))
    (let ((count (cdr trie)))
      (when (>= count minimum-count)
	(let ((new-itemset (reverse (cons (car trie) passed))))
	  (funcall fn new-itemset)
	  (setf (gethash new-itemset itemset-hash) count))))))

(defun apriori-itemset-counting-trie (transactions item-order support rule-length)
  (let* ((itemset-hash (make-hash-table :test #'equal))
	 (total-transaction (hash-table-count transactions))
	 (count-threshold (max 1 (* total-transaction (/ support 100)))))
    ;; first setp, generate 1-itemsets
    (loop for trans being the hash-value in transactions
	do (loop for item in trans
	       as itemset = (list item)
	       do (incf (gethash itemset itemset-hash 0))))
    ;; frequent itemset generation loop
    (let ((itemsets (sort
		     (loop for itemset being the hash-key in itemset-hash
			 for num being the hash-value in itemset-hash
			 if (>= num count-threshold)
			 collect itemset
			 else
			 do (remhash itemset itemset-hash))
		     #'< :key #'(lambda (itemset)
				  (gethash (car itemset) item-order)))))
      (loop for k from 2 to rule-length
	  as counting-trie = (gen-next-itemset-trie itemsets)
	  do
	    (loop for trans being the hash-value in transactions do
		  (loop for branch in (cdr counting-trie) do
			(update-trie-count-apriori branch trans)))
	    (setf itemsets nil)
	    (loop for branch in (cdr counting-trie) do
		  (dump-itemset-hash branch itemset-hash count-threshold
				     #'(lambda (itemset)
					 (push itemset itemsets))))
	  finally (return itemset-hash)))))

(defun %association-analyze-apriori (unsp-dataset target-variables key-variable rule-length
				     &key (support 0) (confident 0) (lift 0) (conviction 0))
  "association analyze with apriori algorithm.
- return: assoc-result-dataset
- arguments:
  - unsp-dataset: <unspecialized-dataset>
  - target-variables : (list of string) column names
  - key-variable : <string> column name for determining identities
  - rule-length : <integer> >= 2, maximum length for a rule
  - support : <number> for percentage
  - confident : <number> for percentage
  - lift : <number> beyond 0
  - conviction : <number> beyond 0
"
  (assert (and (<= 0 support 100) (<= 0 confident 100) (<= 0 lift) (<= 0 conviction)))
  (assert (and (integerp rule-length) (<= 2 rule-length)))
  (multiple-value-bind (transactions item-order)
      (scan-input-data unsp-dataset target-variables key-variable)
    (let ((rule-occur (apriori-itemset-counting-trie
                       transactions item-order support rule-length)))
      (let ((ans nil)
            (count (hash-table-count transactions)))
        (maphash #'(lambda (rule rule-count)
                     (let ((rule-length (length rule)))
                       (when (> rule-length 1)
                         (ap-maprule
                          #'(lambda (conc pre)
                              (multiple-value-bind (sup conf lif conv) (rule-indexes conc pre rule
                                                                                     rule-occur count)
                                (when (and (>= sup support) (>= conf confident) (>= lif lift) (>= conv conviction))
                                  (push (make-rule conc pre
                                                   sup conf lif conv) ans))))
                          rule (length rule)
                          #'(lambda (itemset) (gethash itemset rule-occur))
                          (confident->max-precount rule-count confident)))))
                 rule-occur)
        (make-assoc-result ans support confident 
                           lift conviction rule-length)))))


;; interface
(defun association-analyze (unsp-dataset target-variables key-variable rule-length
                            &key (algorithm :lcm)
                              (support 0) (confident 0) (lift 0) (conviction 0))
  "Preform association rule analysis on an unspecialized dataset. The dataset should contain an 'key' collumn with a unique string identifier that groups all rows belonging to the same transaction. For example:
    id     item
    ------|----------
    \"1\"   milk
    \"1\"   bread
    \"2\"   pork chops
    \"2\"   pepper
In the above example the 'id' column would be the key-varable and 'item' would be a target variable.

- return: assoc-result-dataset
- arguments:
- unsp-dataset: <unspecialized-dataset>
- target-variables : <list string> column names
- key-variable : <string> column name for determining identities
- rule-length : <integer> >= 2, maximum length for a rule
- support : <number> for percentage
- confident : <number> for percentage
- lift : <number> beyond 0
- conviction : <number> beyond 0
- algorithm : :apriori | :da | :fp-growth | :eclat | :lcm (default)
"

  (assert (member algorithm `(:apriori :da :fp-growth :eclat :lcm)))
  (case algorithm
            (:apriori
             (%association-analyze-apriori 
              unsp-dataset
              target-variables key-variable rule-length
              :support support :confident confident :lift lift :conviction conviction))
            (:da
            (%association-analyze-da-ap-genrule
             unsp-dataset
             target-variables key-variable rule-length
             :support support :confident confident :lift lift :conviction conviction))
           (:fp-growth
            (%association-analyze-fp-growth
             unsp-dataset
             target-variables key-variable rule-length
             :support support :confident confident :lift lift :conviction conviction))
           (:eclat
            (%association-analyze-eclat
             unsp-dataset
             target-variables key-variable rule-length
             :support support :confident confident :lift lift :conviction conviction))
           (:lcm
            (%association-analyze-lcm
             unsp-dataset
             target-variables key-variable rule-length
             :support support :confident confident :lift lift :conviction conviction))))

(defun association-analyze-file (infile outfile target-variables key-variable rule-length
                            &key (support 0) (confident 0) (lift 0) (conviction 0)
                              (external-format :default) (file-type :sexp)
                              (csv-type-spec '(string double-float))
                              (algorithm :lcm))
  "- return: assoc-result-dataset
- arguments:
- infile : <string>
- outfile : <string>
- target-variables : <list string> column names
- key-variable : <string> column name for determining identities
- rule-length : <integer> >= 2, maximum length for a rule
- support : <number> for percentage
- confident : <number> for percentage
- lift : <number> beyond 0
- conviction : <number> beyond 0
- file-type : :sexp | :csv
- external-format : <acl-external-format>
- csv-type-spec : <list symbol>, type conversion of each column when reading lines from CSV file, e.g. '(string integer double-float double-float)
- algorithm : :apriori | :da | :fp-growth | :eclat | :lcm
"

  (assert (member algorithm `(:apriori :da :fp-growth :eclat :lcm)))
  (let ((assoc-result
          (case algorithm
            (:apriori
             (%association-analyze-apriori
              (read-data-from-file infile :external-format external-format
                                          :type file-type :csv-type-spec csv-type-spec)
              target-variables key-variable rule-length
              :support support :confident confident :lift lift :conviction conviction))
            (:da
             (%association-analyze-da-ap-genrule
              (read-data-from-file infile :external-format external-format
                                          :type file-type :csv-type-spec csv-type-spec)
              target-variables key-variable rule-length
              :support support :confident confident :lift lift :conviction conviction))
            (:fp-growth
             (%association-analyze-fp-growth
              (read-data-from-file infile :external-format external-format
                                          :type file-type :csv-type-spec csv-type-spec)
              target-variables key-variable rule-length
              :support support :confident confident :lift lift :conviction conviction))
            (:eclat
             (%association-analyze-eclat
              (read-data-from-file infile :external-format external-format
                                          :type file-type :csv-type-spec csv-type-spec)
              target-variables key-variable rule-length
              :support support :confident confident :lift lift :conviction conviction))
            (:lcm
             (%association-analyze-lcm
              (read-data-from-file infile :external-format external-format
                                          :type file-type :csv-type-spec csv-type-spec)
              target-variables key-variable rule-length
              :support support :confident confident :lift lift :conviction conviction)))))
    (with-open-file (stream outfile :direction :output :if-exists :supersede
                                    :external-format external-format)
      (with-standard-io-syntax
        (let ((*read-default-float-format* 'double-float))
          (assoc-data-out assoc-result stream))))))

(defgeneric apply-rules (result-dataset premises &key)
  (:documentation "
Applies rules `assoc-result-dataset` to premises in premesises` and returns conclusions.

-return: List of vectors where each vector contains alist of field values of conclusion, support,
         confidence,lift and conviction
- arguments:
    - result-dataset : assoc-result-dataset
    - premises : alist of field and values for premises to match
    - support : <optional keyword> if non nul support of result must be equal or greater than
                supplied value
    - confidence : <optional keyword> if non nul support of result must be equal or greater than
                supplied value
    - lift : <optional keyword> if non nul support of result must be equal or greater than
                supplied value
    - conviction : <optional keyword> if non nul support of result must be equal or greater than
                supplied value

"))
(defmethod apply-rules ((rules assoc-result-dataset) premises
                        &key support confidence lift conviction)
  (let ((np (length premises)))
    (loop
      for rule in (assoc-result-rules rules)
      when (and (= np (loop for p in premises
                            sum (loop for r in (elt rule 0)
                                      when (and (equal (car r) (car p)) (equal (cdr r) (cdr p)))
                                        count 1
                                      )))
                (or (not support) (>= (elt rule 2) support))
                (or (not confidence) (>= (elt rule 3) confidence))
                (or (not lift) (>= (elt rule 4) lift))
                (or (not conviction) (>= (elt rule 2) conviction)))
        collect (subseq rule 1)
      ))
  )
