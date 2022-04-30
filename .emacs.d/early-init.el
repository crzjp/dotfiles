;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq package-native-compile t
      native-comp-deferred-compilation t
      native-comp-async-report-warnings-errors nil)

(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      server-client-instructions nil)

(setq user-emacs-directory "~/.cache/emacs/")

(when (not (file-directory-p user-emacs-directory))
  (make-directory user-emacs-directory t))
