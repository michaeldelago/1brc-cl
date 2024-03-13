
(defpackage :1brc
  (:use :cl)
  (:import-from :parse-number #:parse-number))
(in-package 1brc)

(defparameter *cities* (make-hash-table :test #'equal :size 1000))

(defstruct city 
  (min-temp 0.0 :type float)
  (max-temp 0.0 :type float)
  (avg-temp 0.0 :type float)
  (sum-temp 0.0 :type float)
  (measurement-count 1 :type real))

(serapeum:-> record-measurement ((or null city) float) city)
(defun record-measurement (city temp)
  (declare (optimize (speed 3) (safety 0)))
  (if city
      (progn 
        (with-slots (min-temp max-temp avg-temp sum-temp measurement-count)
          city
          (progn
            (setf min-temp (min min-temp temp))
            (setf max-temp (max max-temp temp))
            (setf sum-temp (+ sum-temp temp))
            (incf measurement-count)))
        city)
      (make-city :min-temp temp 
                 :max-temp temp 
                 :sum-temp temp
                 :avg-temp temp)))

(defun hash-table-sorted ()
  "Returns the *cities* hash table sorted by key"
  (sort (alexandria:hash-table-alist *cities*) #'string-lessp :key #'car))

(defun process-row (line)
  (let* ((parts (uiop:split-string line :separator ";"))
         (city (car parts))
         (temp (parse-number (cadr parts)))) 
      (setf (gethash city *cities*) (record-measurement (gethash city *cities*) temp))))

(defun format-cities ()
  (format nil "{~{~A~^, ~}}~%"
          (loop for (k . v) in (hash-table-sorted)
                collect (with-slots (min-temp max-temp sum-temp measurement-count)
                          v
                          (format nil "~A=~,1f/~,1f/~,1f" k min-temp (/ sum-temp measurement-count) max-temp)))))

(defun main (&rest argv)
  (declare (ignorable argv))
  (let ((input-file (car argv)))
    (with-open-file (input-stream input-file)
      (do ((line (read-line input-stream nil)
                 (read-line input-stream nil)))
        ((null line))
        (process-row line))))
  (princ (format-cities)))
;;; vim: set ft=lisp lisp:
