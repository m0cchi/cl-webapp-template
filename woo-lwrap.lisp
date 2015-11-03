(provide 'woo.lwrap)

(defpackage :woo.lwrap
  (:use :cl)
  (:export response defroutes))

(in-package :woo.lwrap)

(defun response (body &key status content-type)
  (list 
   (or status 200)
   (list :content-type (or content-type "text/html"))
   (list body)))

(defun get-query (env &rest keys)
  (format t "ENV:~A~%KEY:~A~%" env keys)
  '("a"))

(defun hook (mes obj)
  (format t "~%~A:~A~%" mes obj)
  obj)

(defmacro apply-args (func args)
  (format t "apply-args:~A:~A~%" func args)
  (setq func (reverse func))
  (dolist (arg args)
    (push arg func))
  (setq func (reverse func)))

(defmacro defroutes (name &rest routes)
  (let ((routing-list '()))
    (dolist (route routes)
      (push
       `((and (string= (getf env :path-info)
                       ,(nth 1 route))
              (string= (getf env :request-method)
                       ,(string (nth 0 route))))
         ((lambda ()
            ,(nth 3 route))))
       routing-list))
    (list 'defun name '(env)
          `(cond ,@routing-list))))
#|
(defun m (mes obj)
  (format t "~A:~A~%" mes obj)
  obj)

(defmacro cc (name)
  (let ((cli '()) (num '(1 2 3 4 5)))
    (dolist (nnum num)
      (push
       `((= n ,nnum) (format nil "~a~a" n n))
       cli))
    (format t "~A~%" cli)
    (m "return" (list 'defun name '(n)
          `(cond ,@cli)))))


(defun cc2 (n)
  (cond
   ((= n 1) (* n n))
   ((= n 2) (* n n))
   ((= n 3) (* n n))))

(defroutes app
  (:get "/" () (response "hello"))
  (:get "/index" () (response "hello")))
|#
