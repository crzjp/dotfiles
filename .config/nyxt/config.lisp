(in-package #:nyxt-user)

;;; basic modes setup for web-buffer
(defvar *web-buffer-modes*
  '(:emacs-mode
    :blocker-mode :force-https-mode
    :reduce-tracking-mode))

(define-configuration :web-buffer
  ((default-modes `(,@*web-buffer-modes* ,@%slot-value%))))

;;; emacs
(define-configuration (:modable-buffer :prompt-buffer :editor-buffer)
  ((default-modes `(:emacs-mode ,@%slot-value%))))

;;; webkit settings
;; (defmethod ffi-buffer-make :after ((buffer nyxt/renderer/gtk::gtk-buffer))
;;   "See https://webkitgtk.org/reference/webkit2gtk/stable/WebKitSettings.html"
;;   (when (slot-boundp buffer 'nyxt/renderer/gtk::gtk-object)
;;     (let* ((settings (webkit:webkit-web-view-get-settings
;;                       (nyxt/renderer/gtk::gtk-object buffer))))
;;       (setf
;;        ;; Write console errors/warnings to the shell, to ease debugging.
;;        (webkit:webkit-settings-enable-write-console-messages-to-stdout settings) t
;;        ;; Enable WebRTC.
;;        (webkit:webkit-settings-enable-media-stream settings) t
;;        ;; Use Iosevka-Etoile as the default font.
;;        (webkit:webkit-settings-default-font-family settings) "Iosevka Etoile"
;;        (webkit:webkit-settings-default-font-size settings) 13
;;        ;; Use Iosevka-Slab as the monospace font.
;;        (webkit:webkit-settings-monospace-font-family settings) "Iosevka Slab"
;;        (webkit:webkit-settings-default-monospace-font-size settings) 13)))
;;   ;; Set the view background to black.
;;   (cffi:foreign-funcall
;;    "webkit_web_view_set_background_color"
;;    :pointer (g:pointer (nyxt/renderer/gtk:gtk-object buffer))
;;    ;; GdkRgba is simply an array of four doubles.
;;    :pointer (cffi:foreign-alloc
;;              :double
;;              :count 4
;;              ;; red green blue alpha
;;              :initial-contents '(0d0 0d0 0d0 1d0))))

;;; search engine
(defvar *crz/search-engines*
  (list
   '("ddg" "https://lite.duckduckgo.com/lite/search?q=~a" "https://lite.duckduckgo.com/")
   '("sx" "https://searx.neocities.org/?q=~a" "https://searx.neocities.org/")))

(define-configuration context-buffer
  ((search-engines
    (append
     (mapcar (lambda (engine) (apply 'make-search-engine engine))
             *crz/search-engines*)
     %slot-default%))))

;;; theme
(define-configuration browser
  ((theme
    (make-instance 'theme:theme :background-color "black"
                   :on-background-color "#ffffff" :accent-color
                   "#37a8e4" :on-accent-color "black" :primary-color
                   "gray" :on-primary-color "white" :secondary-color
                   "darkgray" :on-secondary-color "black"))))

;;; extensions
;; (defvar *crz/request-resource-handlers* '())

;; (load-after-system :nx-freestance-handler
;;                    (nyxt-init-file "freestance.lisp"))

;; (define-configuration web-buffer
;;   ((request-resource-hook
;;     (reduce #'hooks:add-hook
;;             (mapcar #'make-handler-resource
;; 		            *crz/request-resource-handlers*)
;;             :initial-value %slot-default%))))

;; (define-nyxt-user-system-and-load "nyxt-user/dark-reader"
;;   :depends-on (:nx-dark-reader) :components ("dark-reader.lisp"))
