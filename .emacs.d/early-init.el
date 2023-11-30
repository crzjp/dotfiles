;; -*- lexical-binding: t; -*-

(let ((last-file-name-handler-alist file-name-handler-alist)
      (last-vc-handled-backends vc-handled-backends)
      (last-mode-line-format mode-line-format))
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6
        file-name-handler-alist nil
        vc-handled-backends nil
        mode-line-format nil)
  (add-hook 'after-init-hook
            #'(lambda ()
                (setq gc-cons-threshold (* 2 1000 1000)
                      gc-cons-percentage 0.1
                      file-name-handler-alist last-file-name-handler-alist
                      vc-handled-backends last-vc-handled-backends
                      mode-line-format last-mode-line-format))))

(setq user-emacs-directory "~/.cache/emacs/")

(unless (file-directory-p user-emacs-directory)
  (make-directory user-emacs-directory t))

(setq package-native-compile t
      native-comp-async-report-warnings-errors nil)

(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache" user-emacs-directory))

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      server-client-instructions nil
      inhibit-x-resources t
      frame-inhibit-implied-resize t)
