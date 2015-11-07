(ql:quickload '(:djula
                :cl-ppcre))

(require `woo.lwrap "woo-lwrap")

(defvar *views* "./views")
(defvar *static* "./static")

(defun render (path)
  (djula:render-template*
   (djula:compile-template* (probe-file (format nil "~A/~A" *views* path)))))

(defun read-file (name)
  (let* ((ret "")
         (path (format nil "~A/~A" *static* name))
         (in (open path :if-does-not-exist nil)))
    (if in
        (progn
          (loop for line = (read-line in nil)
                while line do (setf ret (format nil "~a~a~%" ret line)))
          (close in)
          (woo.lwrap:response ret))
      (woo.lwrap:response "NotFound" :status 404))))

(defun is-static-file (path)
  (unless (equal NIL
                 (ppcre:scan "^(?:/images/|/css/|.*\\.html$|/js/)"
                             path))
    NIL
    T))

(defun app (env)
  (woo.lwrap:defroutes
   env
   (:notfound (woo.lwrap:response "NotFound" :status 404))
   (:static-file #'is-static-file (read-file (getf env :path-info)))
   (:get "/env" (woo.lwrap:response (format nil "~a"
                                            (woo.lwrap:parse-uri-params env))))
   (:get "/get" (woo.lwrap:response "get-hello"))
   (:get "/" (woo.lwrap:response (render "top.html")))))
