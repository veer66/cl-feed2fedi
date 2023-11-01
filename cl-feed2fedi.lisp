;;;; cl-feed2fedi.lisp

(in-package #:cl-feed2fedi)

(defstruct config
  (feed "")
  (fedi-url "")
  (fedi-secret "")
  (fedi-key "")
  (fedi-access-token "")
  (max-entries 3)
  (db-pathname #P"data/feed2fedi.db")
  (duration-hours 24))

(defparameter *conf* (read-from-string
		      (with-open-file (f #P"data/config.lisp")
			(let ((lines '()))
			  (loop for line = (read-line f nil :EOF)
				until (eq :EOF line)
				do (push line lines))
			  (format nil "~{~a~^~%~}" (reverse lines))))))

(defun load-feed (feed-url)
  (feedparser:parse-feed (drakma:http-request feed-url)))

(defun remove-known-entries (db-pathname entries)
  (let ((opt (create-options)))
    (set-create-if-missing opt t)
    (with-open-db (db db-pathname opt)
      (remove-if (lambda (entry)
		   (let ((entry-id (gethash :id entry)))
		     (get-kv-str db entry-id)))
		 entries))))

(defun mark-as-known (db-pathname entries)
  (let ((opt (create-options)))
    (set-create-if-missing opt t)
    (with-open-db (db db-pathname opt)
      (dolist (entry entries)
	(let ((val (format nil "(:TIMESTAMP ~a)" (get-universal-time)))
	      (entry-id (gethash :id entry) ))
	  (put-kv-str db entry-id val))))))

(defun select-entries (entries max-entries)
  (loop with selected-entries = '()
	with e = entries
	for i from 1
	until (or (null e) (> i max-entries))
	do
	   (push (car e) selected-entries)
	   (setq e (cdr e))
	finally
	   (return (reverse selected-entries))))

(defun create-poster (conf)
  (lambda (content)
    (let ((client (make-instance 'tooter:client
				 :base (config-fedi-url conf)
				 :name "cl-feed2fedi"
				 :key (config-fedi-key conf)
				 :secret (config-fedi-secret conf)
				 :access-token (config-fedi-access-token conf))))
      (format t "POST: ~a~%" content)
      (tooter:make-status client content))))

(defun entry-to-content (entry)
  (format nil "~a~%~%~a~%" (gethash :TITLE entry) (gethash :LINK entry)))

(defun fetch-and-post (conf post)
  (let* ((feed (load-feed (config-feed conf)))
	 (entries (gethash :ENTRIES feed))
	 (db-pathname (config-db-pathname conf))
	 (new-entries (remove-known-entries db-pathname entries))
	 (max-entries (config-max-entries conf))
	 (selected-entries (select-entries new-entries max-entries)))
    (dolist (entry selected-entries)
      (let ((content (entry-to-content entry)))
	(funcall post content)))
    (mark-as-known db-pathname entries)))

(defun main ()
  (loop do
    (format t "FETCH-AND-POST ~a~%" (get-universal-time))
    (fetch-and-post *conf* (create-poster *conf*))
    (let ((sleep-duration (* 60 60 (config-duration-hours *conf*))))
      (format t "SLEEP ~a seconds~%" sleep-duration)
      (sleep sleep-duration))))


