(require 'woo.lwrap.util "woo-lwrap-util")
(provide 'woo.lwrap)

(defpackage :woo.lwrap
  (:use :cl)
  (:export parse-uri-params parse-query response defroutes))

(in-package :woo.lwrap)

(defvar *special-request* '(:notfound))
(defvar +notfound+ :notfound)
(defvar +static-file+ :static-file)

(defun parse-params (request-param)
  (let ((params '()))
    (dolist (param (woo.lwrap.util:split-string "&" request-param))
      (setq params `(,@params ,@(woo.lwrap.util:split-string "=" param))))
    (woo.lwrap.util:make-map params)))

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
        ((equal +static-file+ (nth 0 route))
         `((funcall ,(nth 1 route) (getf ,env :path-info))
           ,(nth 2 route)))
        ((equal +notfound+ (nth 0 route))
         `(T
           ,(nth 1 route)))
        (T
         `((and (string= (getf ,env :path-info)
                         ,(nth 1 route))
                (string= (getf ,env :request-method)
                         ,(string (nth 0 route))))
           ,(nth 2 route))))
       routing-list))
    `(cond ,@routing-list)))
