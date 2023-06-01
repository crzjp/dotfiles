(defvar *crz/request-resource-handlers* '())

(load-after-system :nx-freestance-handler
                   (nyxt-init-file "freestance.lisp"))

(define-configuration web-buffer
  ((request-resource-hook
    (reduce #'hooks:add-hook
            (mapcar #'make-handler-resource
		            *crz/request-resource-handlers*)
            :initial-value %slot-default%))))

(define-nyxt-user-system-and-load "nyxt-user/dark-reader"
  :depends-on (:nx-dark-reader) :components ("dark-reader.lisp"))
