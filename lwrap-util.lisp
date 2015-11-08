(in-package :cl-user)

(provide 'lwrap.util)

(defpackage :lwrap.util
  (:use :cl)
  (:export make-keyword split-string make-map))

(in-package :lwrap.util)

(defun make-keyword (name)
  (values
   (intern 
    (string-upcase name)
    :keyword)))

(defun split-string (word string)
  (let ((i (search word string))
        (retval '()))
    (if i
        (progn
          (setq retval (list (subseq string 0 i)))
          (let* ((retval2 (split-string word
                                        (subseq string (+ i 1)))))
            (setq  retval `(,@retval ,@retval2))))
      (setq retval (list string)))
    retval))

(defun make-map (params)
  (if (= 0
         (mod (list-length params) 2))
      (dotimes (i (/ (list-length params) 2))
        (setf (nth (* i 2) params)
              (make-keyword (nth (* i 2) params)))))
  params)

