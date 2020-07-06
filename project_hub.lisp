;; A simple database to track extra curricular projects
;; A project consists of a topic, description, URL, author, and category

(defvar *db* nil)

(defun make-project (topic category &optional description url author)
  (list :topic topic :category category :description description :url url :author author))

(defun add-project (project) (push project *db*))

(defun print-projects (&optional projects)
  (format t "~{~{~a:~10t~a~%~}~%~}" (or projects *db*)))

(defun save-projects (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))

(defun load-projects (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax (setf *db* (read in)))))

(defun prompt-read (prompt)
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-project ()
  (make-project
   (prompt-read "Topic ")
   (prompt-read "Category ")))

(defun add-projects ()
  (loop (add-project (prompt-for-project))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun get-projects-by-category (category)
  (remove-if-not
   #'(lambda (project) (equal (getf project :category) category)) *db*))  

;; returns a list of all categories
(defun list-categories ()
  (mapcar
   #'(lambda (project) (getf project :category)) *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun list-projects (selector-fn)
  (mapcar selector-fn *db))

(defun where (&key topic category author (url nil url-p))
  #'(lambda (project)
      (and
       (if topic (equal (getf project :topic) topic) t)
       (if category (equal (getf project :category) category) t)
       (if author (equal (getf project :author) author) t)
       (if url-p (equal (getf project url) url) t))))

(defun update (selector-fn &key topic category description author url)
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if topic (setf (getf row :topic) topic))
	       (if category (setf (getf row :category) category))
	       (if description (setf (getf row :description) description))
	       (if author (setf (getf row :author) author))
	       (if url (setf (getf row :url) url)))
	     row) *db*)))

(defun make-comparison-expr (field value)
  `(equal (getf project ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (project) (and ,@(make-comparisons-list clauses))))
