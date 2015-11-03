(ql:quickload :djula)
(require `woo.lwrap "woo-lwrap")

(defvar *views* "./views")
(defvar *static-file* "./static")

(defun render (path)
  (djula:render-template*
   (djula:compile-template* (probe-file (format nil "~A/~A" *views* path)))))

(defun app (env)
  (woo.lwrap:defroutes
   env
   (:get "/env" (woo.lwrap:response (format nil "~a"
                                            (woo.lwrap:parse-uri-params env))))
   (:get "/get" (woo.lwrap:response "get-hello"))
   (:get "/" (woo.lwrap:response (render "top.html")))))
