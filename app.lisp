(ql:quickload '(:uiop
                :djula
                :cl-ppcre))

(require `lwrap "lwrap")

(defpackage :app
  (:use :cl)
  (:import-from :lwrap
                :defroutes
                :parse-uri-params
                :response-with-file
                :response-with-text)
  (:export app))
(in-package :app)

(defvar *views* "./views")
(defvar *static* "./static")

(defun render (path)
  (djula:render-template*
   (djula:compile-template* (probe-file (format nil "~A/~A" *views* path)))))

(defun read-file (name)
  (let* ((path (format nil "~A/~A" *static* name))
         (file (uiop:file-exists-p path)))
    (if file
        (response-with-file file)
      (response-with-text "NotFound" :status 404))))

(defun is-static-file (path)
  (not (equal NIL
              (ppcre:scan "^(?:/images/|/css/|.*\\.html$|/js/)"
                          path))))

(defun app (env)
  (defroutes
   env
   (:notfound (response-with-text "NotFound" :status 404))
   (:static-file #'is-static-file (read-file (getf env :path-info)))
   (:get "/env" (response-with-text (format nil "~a"
                                            (parse-uri-params env))))
   (:get "/get" (response-with-text "get-hello"))
   (:get "/" (response-with-text (render "top.html")))))
