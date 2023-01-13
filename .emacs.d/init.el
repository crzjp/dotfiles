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
  :custom
  (auto-package-update-delete-old-versions t))

(setq user-full-name "João Paulo da Cruz"
      user-mail-address "crzjp@riseup.net")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file nil t))

(setq backup-directory-alist `((".*" . ,(expand-file-name "backups" user-emacs-directory)))
      backup-by-copying t
      version-control t
      delete-old-versions t
      vc-make-backup-files t
      kept-old-versions 10
      kept-new-versions 10)

(setq auto-save-list-file-prefix (expand-file-name "autosaves/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "autosaves/" user-emacs-directory) t)))

(setq recentf-max-saved-items 50
      recentf-auto-cleanup 300)

(recentf-mode 1)

(setq-default vc-follow-symlinks)

(setq use-short-answers t)

(use-package browse-url
  :ensure nil
  :config
  (when (getenv "BROWSER")
    (setq browse-url-generic-program (executable-find (getenv "BROWSER"))
          browse-url-browser-function 'browse-url-generic)))

(cd "~/")

(setq global-auto-revert-non-file-buffers t)

(global-auto-revert-mode 1)

(setq delete-by-moving-to-trash t
      trash-directory (expand-file-name "emacs-trash" temporary-file-directory))

(setq large-file-warning-threshold nil)

(setq-default indent-tabs-mode nil
              tab-width 4)

(delete-selection-mode 1)

(use-package help
  :ensure nil
  :custom
  (help-window-select t))

(setenv "PAGER" "cat")
(setenv "MANPAGER" "cat")

(setq disabled-command-function nil)

(global-set-key (kbd "M-&") 'with-editor-async-shell-command)

(setq kill-do-not-save-duplicates t)

(use-package woman
  :ensure nil
  :custom
  (woman-fill-frame t))

(use-package ace-window
  :custom
  (aw-scope 'frame)
  (aw-ignore-current t)
  :bind ("M-o" . ace-window))

(use-package popper
  :defer 2
  :custom
  (popper-reference-buffers
   '("\\*Async Shell Command\\*"
     "\\*DWIM shell command\\* done"
     grep-mode
     debugger-mode))
  :config
  (popper-mode 1)
  (popper-echo-mode 1)
  :bind (("M-'" . popper-toggle-latest)
         ("C-'" . popper-cycle)
         ("C-M-'" . popper-toggle-type)))

(setq history-length 50
      history-delete-duplicates t)

(savehist-mode 1)

(setq enable-recursive-minibuffers t)

(use-package vertico
  :defer 1
  :config
  (vertico-mode 1))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless))
  (orderless-matching-styles '(orderless-flex)))

(use-package consult
  :after vertico
  :config
  (consult-customize consult-recent-file :preview-key nil)
  (consult-customize consult-org-heading :preview-key nil)
  :bind (("C-c r" . consult-recent-file)
         :map minibuffer-mode-map
         ("C-r" . consult-history))
  :init
  (setq-default completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     'consult-completion-in-region
                   'completion--in-region)
                 args))))

(setq read-extended-command-predicate 'command-completion-default-include-p)

(use-package corfu
  :defer 1
  :custom
  (corfu-preview-current nil)
  :config
  (global-corfu-mode 1))

(use-package corfu
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :bind (:map corfu-map
         ("M-m" . corfu-move-to-minibuffer)))

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
  :hook (eshell-pre-command . eshell-save-some-history)
  :custom
  (eshell-history-size 1000)
  (eshell-hist-ignoredups t)
  :bind (:map eshell-mode-map
         ("C-r" . consult-history)))

(use-package em-prompt
  :ensure nil
  :hook (eshell-mode . (lambda ()
                         (setq-local outline-regexp eshell-prompt-regexp)))
  :custom
  (eshell-prompt-regexp "^[^$\n]*\\\$ ")
  (eshell-prompt-function
   (lambda ()
     (concat
      "[" (abbreviate-file-name (eshell/pwd)) "]"
      (propertize "$" 'invisible t) " ")))
  :bind (:map eshell-mode-map
         ("C-c s" . consult-outline)))

(use-package xterm-color)

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

(use-package em-alias
  :ensure nil
  :custom
  (eshell-aliases-file "~/.emacs.d/eshell-aliases")
  :config
  (eshell-read-aliases-list))

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
  :custom
  (eshell-buffer-maximum-lines 1000)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-destroy-buffer-when-process-dies t)
  :config
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer))

(use-package vterm
  :bind (("C-c t" . vterm)
         :map vterm-mode-map
         ("C-q" . vterm-send-next-key))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-clear-scrollback-when-clearing t))

(use-package diredfl)

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-agGh --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (diredfl-global-mode)
  :bind (("C-x C-d" . dired-jump)
         :map dired-mode-map
         ("f" . dired-create-empty-file)))

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

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-saved-filter-groups
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
      ("Music" (name . "\*Mingus.*"))
      ("Games" (mode . gomoku-mode))
      ("Internal" (name . "^\*.*$"))
      ("Misc" (name . "^.*$")))))
  (ibuffer-show-empty-filter-groups nil)
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-switch-to-saved-filter-groups "Default"))))

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 20 20 :left :elide)
           " "
           (size-h 11 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :bind ("C-x C-b" . ibuffer))

(use-package eglot)

(add-hook 'c-mode-hook 'eglot-ensure)

(use-package cider)

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(setq frame-resize-pixelwise t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(column-number-mode 1)

(use-package rainbow-mode)

(setq-default cursor-type 'hbar
              cursor-in-non-selected-windows nil)

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-new-button nil)
  (tab-bar-close-button nil)
  (tab-bar-back-button nil)
  (tab-bar-border nil)
  (tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  (tab-bar-tab-name-truncated-max 15)
  (tab-bar-show 1)
  :bind (("C-<tab>" . tab-recent)
         ("C-x t b" . tab-switch)))

(defvar crz/font "Iosevka Slab 10")

(defun crz/set-font-faces ()
  (set-face-attribute 'default nil :font crz/font)
  (set-face-attribute 'fixed-pitch nil :font crz/font)
  (set-face-attribute 'variable-pitch nil :font crz/font))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (crz/set-font-faces))))
  (crz/set-font-faces))

(use-package modus-themes
  :ensure nil
  :custom
  (modus-themes-subtle-line-numbers t)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-mode-line '(borderless))
  :init
  (load-theme 'modus-operandi t))

(setq use-dialog-box nil)

(use-package time
  :ensure nil
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  :init
  (display-time-mode 1))

(use-package org
  :ensure nil
  :mode ("\\.org$" . org-mode)
  :custom
  (org-files-directory "~/media/docs/org")
  (org-return-follows-link t)
  :bind (:map org-mode-map
         ("C-c o" . consult-org-heading)))

(use-package org
  :ensure nil
  :custom
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(600))
  (org-startup-folded t)
  (org-hide-emphasis-markers t)
  (org-ellipsis " ▾")
  :hook (org-mode . visual-line-mode))

(use-package org-superstar
  :custom
  (org-superstar-headline-bullets-list '(9673 9675 10040))
  :hook (org-mode . org-superstar-mode))

(use-package org
  :ensure nil
  :custom
  (org-src-window-setup 'current-window)
  (org-edit-src-content-indentation 0)
  :config
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh")))

(use-package org
  :ensure nil
  :custom
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-agenda-files '("~/media/docs/notas/agenda.org"))
  :bind ("C-c a" . org-agenda))

(use-package magit)

(use-package dwim-shell-command
  :defer 2
  :config
  (require 'dwim-shell-commands)
  :custom
  (dwim-shell-command-default-command nil)
  :bind (("M-!" . dwim-shell-command)
         ("C-c k" . dwim-shell-commands-kill-process)
         :map dired-mode-map
         ("!" . dwim-shell-command)))

(use-package dwim-shell-command
  :config
  (defun dwim-shell-commands-flac-to-mp3 ()
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert flac to mp3"
     "ffmpeg -stats -n -i '<<f>>' -qscale:a 0 '<<fne>>.mp3'"
     :utils "ffmpeg")))

(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :custom
  (pdf-view-continuous nil))
 ; :init
 ; (pdf-tools-install :noquery))

(use-package pdf-view-restore
  :hook (pdf-view-mode . pdf-view-restore-mode))

(use-package esxml)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package erc-hl-nicks)

(use-package erc
  :ensure nil
  :custom
  (erc-accidental-paste-threshold-seconds nil)
  (erc-nick "crzjp")
  (erc-fill-column (- (window-width) 1))
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 20)
  (erc-image-inline-rescale 200)
  (erc-prompt (lambda () (concat "[" (buffer-name) "]")))
  :config
  (add-to-list 'erc-modules 'autojoin)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'hl-nicks))

(use-package transmission
  :custom
  (transmission-refresh-modes
   '(transmission-mode
     transmission-files-mode
     transmission-info-mode
     transmission-peers-mode)))

(use-package 0x0
  :custom
  (0x0-servers
   '((0x0
      :scheme "https"
      :host "0x0.st"
      :default-dir "~/"
      :curl-args-fun 0x0--make-0x0-curl-args
      :min-age 30
      :max-age 365
      :max-size ,(* 1024 1024 512)))))

(use-package mingus
  :custom
  (mingus-use-mouse-p nil)
  (mingus-mode-line-show-elapsed-time nil)
  (mingus-mode-line-show-volume nil))
