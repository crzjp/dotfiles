;; (in-package #:nyxt-user)

;; (define-configuration :web-buffer
;;   ((request-resource-hook
;;     (hooks:add-hook %slot-value% 'nx-freestance-handler:*freestance-handlers*))))

;; to add all handlers/redirectors (youtube to invidious, reddit to teddit,
;; instagram to bibliogram, medium to scribe, twitter to nitter)
(setq *crz/request-resource-handlers*
     (nconc *crz/request-resource-handlers*
            nx-freestance-handler:*freestance-handlers*))

;; alternatively, you may add each separately
;; (push #'nx-freestance-handler:invidious-handler *my-request-resource-handlers*)
;; (push #'nx-freestance-handler:nitter-handler *my-request-resource-handlers*)
;; (push #'nx-freestance-handler:bibliogram-handler *my-request-resource-handlers*)
;; (push #'nx-freestance-handler:teddit-handler *my-request-resource-handlers*)
;; (push #'nx-freestance-handler:scribe-handler *my-request-resource-handlers*)

;; to set your preferred instance, either invoke SET-PREFERRED-[name of website]-INSTANCE
;; command in Nyxt (the effect lasts until you close Nyxt), or write something like this:
;; (setf nx-freestance-handler:*preferred-invidious-instance* "https://invidious.snopyta.org")
