(in-package :cl-user)

(provide 'lwrap)
(require 'lwrap.util "lwrap-util")
(ql:quickload '(:babel
                :trivial-mimes
                :local-time))

(defpackage :lwrap
  (:use :cl)
  (:import-from :lwrap.util
                :split-string
                :make-map)
  (:import-from :babel
                :string-to-octets)
  (:import-from :trivial-mimes
                :mime)
  (:import-from :local-time
                :universal-to-timestamp
                :format-rfc1123-timestring)
  (:export parse-uri-params parse-query response-with-text response-with-file defroutes))

(in-package :lwrap)

(defvar *special-request* '(:notfound))
(defvar +notfound+ :notfound)
(defvar +static-file+ :static-file)

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

(defun response-with-text (text &key status content-type)
  (list 
   (or status 200)
   (list :content-type (or content-type "text/html")
         :content-length (length (string-to-octets text)))
   (list text)))

(defun response-with-file (file)
  (let ((content-type (format nil "~A~:[~;~:*; charset=~A~]"
                              (mime file) "utf-8"))
        (timestamp (format-rfc1123-timestring
                    nil
                    (universal-to-timestamp (get-universal-time)))))
    (with-open-file (stream file
                            :direction :input
                            :if-does-not-exist nil)
                    `(200
                      (:content-type ,content-type
                       :content-length ,(file-length stream)
                       :last-modified ,timestamp)
                      ,file))))

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
