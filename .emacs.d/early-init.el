;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          #'(lambda () (setq gc-cons-threshold (* 2 1000 1000))))

(setq user-emacs-directory "~/.cache/emacs/")

(when (not (file-directory-p user-emacs-directory))
  (make-directory user-emacs-directory t))

(setq package-native-compile t
      native-comp-deferred-compilation t
      native-comp-async-report-warnings-errors nil)

(setq native-comp-eln-load-path
      (list (expand-file-name "eln-cache" user-emacs-directory)))

(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      server-client-instructions nil)
