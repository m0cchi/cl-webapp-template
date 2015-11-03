(provide 'woo.lwrap)

(defpackage :woo.lwrap
  (:use :cl)
  (:export parse-uri-params parse-query response defroutes))

(in-package :woo.lwrap)

(defvar *special-request* '(:notfound))
(defvar +notfound+ :notfound)

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

(defun parse-params (request-param)
  (let ((params '()))
    (dolist (param (split-string "&" request-param))
      (setq params `(,@params ,@(split-string "=" param))))
    (make-map params)))

(defun parse-query (request-uri)
  (let ((i (search "?" request-uri)))
    (if i
        (parse-params (subseq request-uri (+ 1 i))))))

(defun parse-uri-params (env)
  (parse-query (getf env :request-uri)))

(defun response (body &key status content-type)
  (list 
   (or status 200)
   (list :content-type (or content-type "text/html"))
   (list body)))

(defmacro defroutes (env &rest routes)
  (let ((routing-list '()))
    (dolist (route routes)
      (push
       (cond
        ((equal +notfound+ (nth 0 route))
         `(T
           ,(nth 2 route)))
        (T
         `((and (string= (getf ,env :path-info)
                         ,(nth 1 route))
                (string= (getf ,env :request-method)
                         ,(string (nth 0 route))))
           ,(nth 2 route))))
       routing-list))
    `(cond ,@routing-list)))
