;; -*- lexical-binding: t; -*-

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t))

(global-set-key (kbd "M-&") 'with-editor-async-shell-command)

(setq global-auto-revert-non-file-buffers t)

(global-auto-revert-mode 1)

(cd "~/")

(setq delete-by-moving-to-trash t
      trash-directory (expand-file-name "emacs-trash" temporary-file-directory))

(delete-selection-mode 1)

(setq kill-do-not-save-duplicates t)

(setq disabled-command-function nil)

(use-package browse-url
  :ensure nil
  :config
  (when (getenv "BROWSER")
    (setq browse-url-generic-program (executable-find (getenv "BROWSER"))
          browse-url-browser-function 'browse-url-generic)))

(setq auto-save-list-file-prefix (expand-file-name "autosaves/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "autosaves/" user-emacs-directory) t)))

(setq backup-directory-alist `((".*" . ,(expand-file-name "backups" user-emacs-directory)))
      backup-by-copying t
      version-control t
      delete-old-versions t
      vc-make-backup-files t
      kept-old-versions 10
      kept-new-versions 10)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq recentf-max-saved-items 100
      recentf-auto-cleanup (* 5 60))

(recentf-mode 1)

(setq-default vc-follow-symlinks)

(use-package help
  :ensure nil
  :config
  (setq help-window-select t))

(use-package woman
  :ensure nil
  :config
  (setq woman-fill-frame t))

(setenv "PAGER" "cat")
(setenv "MANPAGER" "cat")

(setq use-short-answers t)

(setq large-file-warning-threshold nil)

(setq-default indent-tabs-mode nil
              tab-width 4)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(setq user-full-name "João Paulo da Cruz"
      user-mail-address "crzjp@tutanota.com")

(use-package cape
  :defer 1
  :config
  (add-to-list 'completion-at-point-functions 'cape-file))

(use-package corfu
  :defer 1
  :bind (:map corfu-map
         ("M-m" . corfu-move-to-minibuffer))
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (setq corfu-preview-current nil)
  (global-corfu-mode 1))

(use-package consult
  :after vertico
  :bind (("C-c r" . consult-recent-file)
         :map minibuffer-mode-map
         ("C-r" . consult-history))
  :config
  (consult-customize consult-recent-file :preview-key nil)
  (consult-customize consult-org-heading :preview-key nil)
  :init
  (setq-default completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     'consult-completion-in-region
                   'completion--in-region)
                 args))))

(use-package orderless
  :after vertico
  :demand nil
  :config
  (setq completion-styles '(orderless)
        orderless-matching-styles '(orderless-initialism orderless-flex)))

(use-package vertico
  :defer 1
  :bind (:map vertico-map
         ("DEL" . vertico-directory-delete-char))
  :config
  (vertico-mode 1))

(setq read-extended-command-predicate 'command-completion-default-include-p)

(setq history-length 50
      history-delete-duplicates t)

(savehist-mode 1)

(setq enable-recursive-minibuffers t)

(minibuffer-depth-indicate-mode 1)

(use-package diredfl)

(use-package dired
  :ensure nil
  :bind (("C-x C-d" . dired-jump)
         :map dired-mode-map
         ("f" . dired-create-empty-file))
  :config
  (setq dired-listing-switches "-agGh --group-directories-first"
        dired-kill-when-opening-new-dired-buffer t)
  (diredfl-global-mode))

(use-package dwim-shell-command
  :defer 2
  :bind (([remap shell-command] . dwim-shell-command)
         ("C-c k" . dwim-shell-commands-kill-process)
         :map dired-mode-map
         ("!" . dwim-shell-command))
  :config
  (setq dwim-shell-command-default-command nil)
  (require 'dwim-shell-commands))

(use-package dwim-shell-command
  :config
  (defun dwim-shell-commands-flac-to-mp3 ()
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert flac to mp3"
     "ffmpeg -stats -n -i '<<f>>' -ab 320k -map_metadata 0 -id3v2_version 3 '<<fne>>.mp3'"
     :utils "ffmpeg")))

(use-package dwim-shell-command
  :config
  (defun dwim-shell-commands-wallpaper-set-fill ()
    (interactive)
    (dwim-shell-command-on-marked-files
     "Set wallpaper with fill parameter"
     "hsetroot -fill '<<f>>'"
     :utils "hsetroot")))

(use-package ibuffer
  :ensure nil
  :bind ([remap list-buffers] . ibuffer)
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :config
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 20 20 :left :elide)
                " "
                (size-h 11 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))))

(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "Default")))
  :config
  (setq ibuffer-saved-filter-groups
        '(("Default"
           ("Modified" (and (modified . t)
                            (visiting-file . t)))
           ("Term" (or (mode . vterm-mode)
                       (mode . eshell-mode)
                       (mode . term-mode)
                       (mode . shell-mode)))
           ("Debug" (mode . debugger-mode))
           ("Agenda" (filename . "agenda.org"))
           ("Org" (mode . org-mode))
           ("Magit" (name . "magit.*"))
           ("Book" (or (mode . pdf-view-mode)
                       (mode . nov-mode)))
           ("Dired" (mode . dired-mode))
           ("Chat" (mode . erc-mode))
           ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*")
                       (mode . help-mode)
                       (mode . woman-mode)
                       (mode . Man-mode)
                       (mode . Custom-mode)))
           ("Image" (mode . image-mode))
           ("Music" (or (mode . mingus-help-mode)
                        (mode . mingus-playlist-mode)
                        (mode . mingus-browse-mode)))
           ("Torrent" (or (mode . transmission-mode)
                          (mode . transmission-files-mode)))
           ("Games" (mode . gomoku-mode))
           ("Internal" (name . "^\*.*$"))
           ("Misc" (name . "^.*$"))))
        ibuffer-show-empty-filter-groups nil))

(use-package ibuffer
  :ensure nil
  :config
  
  (defun crz/human-readable-file-sizes-to-bytes (string)
    "Convert a human-readable file size into bytes."
    (cond
     ((string-suffix-p "G" string t)
      (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
     ((string-suffix-p "M" string t)
      (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
     ((string-suffix-p "K" string t)
      (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
     (t
      (string-to-number (substring string 0 (- (length string) 1))))))
  
  (defun crz/bytes-to-human-readable-file-sizes (bytes)
    "Convert number of bytes to human-readable file size."
    (cond
     ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
     ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
     ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
     ((> bytes 100000) (format "%10.0fK" (/ bytes 1000.0)))
     ((> bytes 1000) (format "%10.1fK" (/ bytes 1000.0)))
     (t (format "%10d" bytes))))
  
  (define-ibuffer-column size-h
    (:name "Size"
           :inline t
           :summarizer
           (lambda (column-strings)
             (let ((total 0))
               (dolist (string column-strings)
                 (setq total
                       (+ (float (crz/human-readable-file-sizes-to-bytes string))
                          total)))
               (crz/bytes-to-human-readable-file-sizes total))))
    (crz/bytes-to-human-readable-file-sizes (buffer-size))))

(use-package sly
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

(use-package eglot)

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package flymake-shellcheck
  :ensure nil
  :hook (sh-mode . flymake-shellcheck-load))

(use-package magit
  :bind ("C-c g" . magit-status))

(use-package pinentry
  :after magit
  :config
  (setq epg-pinentry-mode 'loopback)
  (pinentry-start))

;(use-package esxml)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package erc-hl-nicks)

(use-package erc
  :ensure nil
  :config
  (setq erc-accidental-paste-threshold-seconds nil
        erc-nick "crzjp"
        erc-fill-column (- (window-width) 1)
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 20
        erc-image-inline-rescale 200
        erc-prompt (lambda () (concat "[" (buffer-name) "]")))
  (add-to-list 'erc-modules 'autojoin)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'hl-nicks))

(use-package mingus
  :config
  (setq mingus-use-mouse-p nil
        mingus-mode-line-show-elapsed-time nil
        mingus-mode-line-show-volume nil))

(use-package pdf-tools
  :ensure nil
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :config
  (setq pdf-view-continuous nil)
  (pdf-loader-install))

(use-package pdf-view-restore
  :hook (pdf-view-mode . pdf-view-restore-mode))

(use-package transmission
  :config
  (setq transmission-refresh-modes
        '(transmission-mode
          transmission-files-mode
          transmission-info-mode
          transmission-peers-mode)))

(use-package org
  :ensure nil
  :bind ("C-c a" . org-agenda)
  :config
  (setq org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-agenda-files '("~/media/docs/notas/agenda.org")))

(use-package org
  :ensure nil
  :mode ("\\.org$" . org-mode)
  :bind (:map org-mode-map
         ("C-c o" . consult-org-heading))
  :config
  (setq org-files-directory "~/media/docs/org"
        org-return-follows-link t)
  (add-to-list 'org-export-backends 'md))

(use-package org
  :ensure nil
  :config
  (setq org-src-window-setup 'current-window
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh")))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '(9673 9675 10040)))

(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-startup-indented t
        org-startup-with-inline-images t
        org-image-actual-width '(600)
        org-startup-folded t
        org-hide-emphasis-markers t
        org-ellipsis " ▾"))

(use-package proced
  :ensure nil
  :bind ("C-c p" . proced)
  :hook (proced-mode . (lambda () (proced-toggle-auto-update 1)))
  :config
  (setq proced-auto-update-interval 2))

(use-package em-alias
  :ensure nil
  :config
  (setq eshell-aliases-file "~/.emacs.d/eshell-aliases")
  (eshell-read-aliases-list))

(use-package pcmpl-args
  :after eshell
  :demand nil
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package eshell
  :ensure nil
  :config
  (defun corfu-send-shell (&rest _)
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode) (fboundp 'comint-send-input))
      (comint-send-input))))
  (advice-add 'corfu-insert :after 'corfu-send-shell))

(use-package esh-mode
  :ensure nil
  :bind (("C-c e" . eshell)
         :map eshell-mode-map
         ("C-l" . (lambda ()
                    (interactive)
                    (let ((input (eshell-get-old-input)))
                      (eshell/clear t)
                      (eshell-emit-prompt)
                      (insert input)))))
  :config
  (setq eshell-buffer-maximum-lines 1000
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer))

(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
         ("C-r" . consult-history))
  :hook (eshell-pre-command . eshell-save-some-history)
  :config
  (setq eshell-history-size 1000
        eshell-hist-ignoredups t))

(use-package em-prompt
  :ensure nil
  :bind (:map eshell-mode-map
         ("C-c s" . consult-outline))
  :hook (eshell-mode . (lambda () (setq-local outline-regexp eshell-prompt-regexp)))
  :config
  (setq eshell-prompt-regexp "^[^$\n]*\\\$ "
        eshell-prompt-function (lambda ()
                                 (concat
                                  "[" (abbreviate-file-name (eshell/pwd)) "]"
                                  (propertize "$" 'invisible t) " "))))

(use-package vterm
  :ensure nil
  :bind (("C-c t" . vterm)
         :map vterm-mode-map
         ("C-q" . vterm-send-next-key))
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-clear-scrollback-when-clearing t))

(use-package rainbow-mode)

(column-number-mode 1)

(setq-default cursor-type 'hbar
              cursor-in-non-selected-windows nil)

(setq use-dialog-box nil)

(use-package time
  :ensure nil
  :defer 2
  :config
  (setq display-time-default-load-average nil
        display-time-24hr-format t)
  (display-time-mode 1))

(defvar crz/font "Iosevka 10")

(defun crz/set-font-faces ()
  (set-face-attribute 'default nil :font crz/font)
  (set-face-attribute 'fixed-pitch nil :font crz/font)
  (set-face-attribute 'variable-pitch nil :font crz/font))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (crz/set-font-faces))))
  (crz/set-font-faces))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq frame-resize-pixelwise t)

(use-package tab-bar
  :ensure nil
  :bind (("C-<tab>" . tab-recent)
         ("C-x t b" . tab-switch))
  :config
  (setq tab-bar-new-button nil
        tab-bar-close-button nil
        tab-bar-back-button nil
        tab-bar-border nil
        tab-bar-tab-name-function 'tab-bar-tab-name-truncated
        tab-bar-tab-name-truncated-max 15
        tab-bar-show 1))

(use-package modus-themes
  :ensure nil
  :init
  (setq modus-themes-subtle-line-numbers t
        modus-themes-org-blocks 'gray-background
        modus-themes-mode-line '(borderless))
  (load-theme 'modus-vivendi t))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-scope 'frame
        aw-ignore-current t))

(use-package popper
  :defer 2
  :bind (("M-'" . popper-toggle-latest)
         ("C-'" . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :config
  (setq popper-window-height 17
        popper-reference-buffers '("\\*Async Shell Command\\*"
                                   "\\*DWIM shell command\\* done"
                                   grep-mode
                                   debugger-mode))
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package xterm-color)

(use-package compile
  :ensure nil
  :config
  (defun crz/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'crz/advice-compilation-filter)
  (setq compilation-environment '("TERM=xterm-256color")))

(use-package esh-mode
  :ensure nil
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))
  (setq xterm-color-use-bold-for-bright t)
  (setenv "TERM" "xterm-256color"))
