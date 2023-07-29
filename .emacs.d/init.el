;; -*- lexical-binding: t; -*-

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t))

(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t))

(setq user-full-name "João Paulo da Cruz"
      user-mail-address "crzzjp@gmail.com")

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

(use-package recentf
  :ensure nil
  :defer 2
  :custom
  (recentf-max-saved-items 100)
  (recentf-auto-cleanup (* 5 60))
  (recentf-exclude
   '("\\.[jp][pn]g\\'" "\\.webp\\'" "\\.pdf\\'" "\\.gpg\\'"
     "/gnu/.*" "~/.cache/.*" ".*/mail/.*"
     "bookmarks\\'"))
  :config
  (recentf-mode 1))

(setq-default vc-follow-symlinks nil)

(use-package help
  :ensure nil
  :custom
  (help-window-select t))

(use-package man
  :ensure nil
  :custom
  (Man-notify-method 'pushy))

(use-package woman
  :ensure nil
  :custom
  (woman-fill-frame t))

(setenv "PAGER" "cat")
(setenv "MANPAGER" "cat")

(setq use-short-answers t)

(setq large-file-warning-threshold nil)

(setq-default indent-tabs-mode nil
              tab-width 4)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(setq sentence-end-double-space nil)

(setq uniquify-buffer-name-style 'forward)

(setq bookmark-save-flag 1)

(use-package cape
  :defer 1
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package corfu
  :defer 1
  :bind (:map corfu-map
         ("M-m" . (lambda ()
                    (interactive)
                    (let ((completion-extra-properties corfu--extra)
                          completion-cycle-threshold completion-cycling)
                      (apply #'consult-completion-in-region completion-in-region--data)))))
  :custom
  (tab-always-indent 'complete)
  (corfu-preview-current nil)
  (corfu-auto t)
  :config
  (global-corfu-mode 1))

(use-package consult
  :after vertico
  :demand nil
  :bind (("C-c r" . consult-recent-file)
         :map minibuffer-mode-map
         ("C-r" . consult-history))
  :custom
  (completion-in-region-function
   (lambda (&rest args)
     (apply (if vertico-mode
                'consult-completion-in-region
              'completion--in-region)
            args)))
  :config
  (consult-customize consult-recent-file :preview-key nil)
  (consult-customize consult-org-heading :preview-key nil))

(use-package marginalia
  :after vertico
  :demand nil
  :config
  (marginalia-mode 1))

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

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless))
  (orderless-matching-styles '(orderless-initialism orderless-flex)))

(use-package dired
  :ensure nil
  :hook (dired-mode . (lambda () (setq truncate-lines t)))
  :bind (("C-x C-d" . dired-jump)
         :map dired-mode-map
         ("f" . dired-create-empty-file))
  :custom
  (dired-listing-switches "-agGh --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dwim-shell-command
  :defer 2
  :bind (([remap shell-command] . dwim-shell-command)
         ("C-c k" . dwim-shell-commands-kill-process)
         :map dired-mode-map
         ("!" . dwim-shell-command))
  :custom
  (dwim-shell-command-default-command nil)
  :config
  (defun dwim-shell-commands-flac-to-mp3 ()
    "Convert flac file to mp3 with ffmpeg using `dwim-shell-command-on-marked-files'"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert flac to mp3"
     "ffmpeg -stats -n -i '<<f>>' -ab 320k -map_metadata 0 -id3v2_version 3 '<<fne>>.mp3'"
     :utils "ffmpeg"))
  (defun dwim-shell-commands-set-wallpaper ()
    "Set wallpaper with hsetroot using `dwim-shell-command-on-marked-files'"
    (interactive)
    (let ((mode (completing-read "Choose parameter: " '("center" "cover" "extend" "fill" "full" "sane" "tile") nil t)))
      (dwim-shell-command-on-marked-files
       "Set wallpaper with PARAMETER parameter"
       (format "hsetroot -%s '<<f>>'" mode)
       :utils "hsetroot"
       :silent-success t)))
  (defun dwim-shell-commands-extract-audio-cover ()
    "Extract audio cover with ffmpeg using `dwim-shell-command-on-marked-files'"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Extract audio cover if it exists"
     "ffmpeg -nostats -loglevel 0 -y -i '<<f>>' Cover.jpg"
     :utils "ffmpeg"))
  (defun dwim-shell-commands-set-audio-tag ()
    "Set audio tag with ffmpeg using `dwim-shell-command-on-marked-files'"
    (interactive)
    (let ((tag (completing-read "Tag name: " nil)))
      (cond
       ((equal tag "image")
        (dwim-shell-command-on-marked-files
         "Set audio cover"
         (format "ffmpeg -loglevel 8 -hide_banner -i '<<f>>' -i %s -map 0:0 -map 1:0 -c copy '<<fne>>-%s-edited.<<e>>'"
                 (read-file-name "Image file: ") tag)
         :utils "ffmpeg"))
       (t
        (dwim-shell-command-on-marked-files
         "Set audio tag"
         (format "ffmpeg -loglevel 8 -hide_banner -i '<<f>>' -c copy -metadata %s=%s '<<fne>>-%s-edited.<<e>>'"
                 tag (completing-read "Tag value: " nil) tag)
         :utils "ffmpeg")))))
  (defun dwim-shell-commands-delete-audio-tags ()
    "Delete audio tags with ffmpeg using `dwim-shell-command-on-marked-files'"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Delete audio tags"
     "ffmpeg -loglevel 8 -hide_banner -i '<<f>>' -map 0:a -c:a copy -map_metadata -1 '<<fne>>-tags-deleted.<<e>>'"
     :utils "ffmpeg"))
  (require 'dwim-shell-commands))

(use-package ibuffer
  :ensure nil
  :bind (([remap list-buffers] . ibuffer)
         ("C-c b" . ibuffer))
  :hook (ibuffer-mode . ibuffer-auto-mode)
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
           " " filename))))

(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "Default")))
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
      ("VC" (or (name . "^magit.*")
                (name . "^\*vc.*")))
      ("Mail" (name . "^\*mu4e.*"))
      ("Book" (or (mode . pdf-view-mode)
                  (mode . nov-mode)))
      ("Dired" (mode . dired-mode))
      ("Chat" (mode . erc-mode))
      ("Help" (or (mode . help-mode)
                  (mode . Info-mode)
                  (mode . woman-mode)
                  (mode . Man-mode)
                  (mode . Custom-mode)
                  (mode . apropos-mode)))
      ("Media" (or (mode . image-mode)
                   (name . "^\*Mingus.*")
                   (name . "^\*transmission.*")
                   (mode . gomoku-mode)))
      ("Internal" (name . "^\*.*$"))
      ("Misc" (name . "^.*$")))))
  (ibuffer-show-empty-filter-groups nil))

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

(use-package cider
  :hook (clojure-mode . cider-mode)
  :config
  (add-to-list 'completion-category-defaults '(cider (styles basic))))

(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))

(use-package sly-mrepl
  :ensure nil
  :after sly
  :bind (:map sly-mrepl-mode-map
         ("C-r" . consult-history))
  :custom
  (sly-mrepl-history-file-name (expand-file-name "sly-mrepl-history" user-emacs-directory)))

(setq eldoc-echo-area-use-multiline-p nil)

(use-package eglot)

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package flymake-shellcheck
  :ensure nil
  :hook (sh-mode . flymake-shellcheck-load))

(use-package magit
  :bind ("C-c g" . magit-status))

(use-package pdf-tools
  :ensure nil
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package mu4e
  :ensure nil
  :bind ("C-c m" . mu4e)
  :custom
  (send-mail-function 'smtpmail-send-it)
  (sendmail-program "msmtp")
  (smtpmail-smtp-server "imap.gmail.com")
  (message-sendmail-f-is-evil t)
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc -a")
  (mu4e-maildir "~/public/mail")
  (mu4e-drafts-folder "/[Gmail]/Drafts")
  (mu4e-sent-folder "/[Gmail]/Sent Mail")
  (mu4e-refile-folder "/[Gmail]/All Mail")
  (mu4e-trash-folder "/[Gmail/Trash")
  (mu4e-read-option-use-builtin nil)
  (mu4e-completing-read-function 'completing-read))

(use-package erc-hl-nicks)

(use-package erc
  :ensure nil
  :custom
  (erc-accidental-paste-threshold-seconds nil)
  (erc-nick "crzjp")
  (erc-fill-column (- (window-width) 1))
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 20)
  (erc-prompt (lambda () (concat "[" (buffer-name) "]")))
  (erc-auto-query 'bury)
  (erc-rename-buffers t)
  (erc-autojoin-timing 'ident)
  (erc-autojoin-delay 1)
  (erc-autojoin-channels-alist
   '(("libera.chat" "#emacs" "#guix" "#nonguix" "#nixers" "#stumpwm")
     ("slackjeff.com.br" "#mundo-libre")))
  (erc-track-exclude-types
   '("JOIN" "MODE" "NICK" "PART" "QUIT" "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'autojoin)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'hl-nicks))

(use-package mingus
  :custom
  (mingus-use-mouse-p nil)
  (mingus-mode-line-show-elapsed-time nil)
  (mingus-mode-line-show-volume nil))

(use-package transmission
  :custom
  (transmission-refresh-modes
   '(transmission-mode
     transmission-files-mode
     transmission-info-mode
     transmission-peers-mode)))

(use-package org
  :ensure nil
  :mode ("\\.org$" . org-mode)
  :bind (:map org-mode-map
         ("C-c o" . consult-org-heading))
  :custom
  (org-directory "~/documents/org")
  (org-return-follows-link t)
  :config
  (add-to-list 'org-export-backends 'md))

(use-package org
  :ensure nil
  :bind ("C-c a" . org-agenda)
  :custom
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-agenda-files '("~/documents/agenda.org")))

(use-package org
  :ensure nil
  :custom
  (org-src-window-setup 'current-window)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  :config
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("eltn" . "src emacs-lisp :tangle no :noweb-ref")))

(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :custom
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(600))
  (org-startup-folded t))

(use-package org-modern
  :after org
  :demand nil
  :config
  (defun crz/org-pretty-mode ()
    (interactive nil org-mode)
    (if org-modern-mode
        (progn
          (setq org-hide-emphasis-markers nil
                org-ellipsis nil)
          (org-mode))
      (setq org-hide-emphasis-markers t
            org-ellipsis " ⤷")
      (org-mode)
      (org-modern-mode 1)
      (variable-pitch-mode 1))))

(use-package proced
  :ensure nil
  :bind ("C-c p" . proced)
  :hook (proced-mode . (lambda () (proced-toggle-auto-update 1)))
  :custom
  (proced-auto-update-interval 2))

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

(use-package em-alias
  :ensure nil
  :custom
  (eshell-aliases-file "~/.emacs.d/eshell-aliases")
  :config
  (eshell-read-aliases-list))

(use-package em-cmpl
  :ensure nil
  :hook (eshell-mode . (lambda () (setq-local corfu-auto nil)))
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
  :bind (:map eshell-mode-map
         ("C-r" . consult-history))
  :hook (eshell-pre-command . eshell-save-some-history)
  :custom
  (eshell-history-size 1000)
  (eshell-hist-ignoredups t))

(use-package em-prompt
  :ensure nil
  :bind (:map eshell-mode-map
         ("C-c s" . consult-outline))
  :hook (eshell-mode . (lambda () (setq-local outline-regexp eshell-prompt-regexp)))
  :custom
  (eshell-prompt-regexp "^[^$\n]*\\\$ ")
  (eshell-prompt-function (lambda ()
                            (concat
                             (crz/eshell-shortened-path (eshell/pwd) 30) " Σ"
                             (propertize "$" 'invisible t) " ")))
  :config
  (defun crz/eshell-shortened-path (path max-length)
    (let* ((components (split-string (abbreviate-file-name path) "/"))
           (len (+ (1- (length components))
                   (cl-reduce '+ components :key 'length)))
           (str ""))
      (while (and (> len max-length)
                  (cdr components))
        (setq str (concat str
                          (cond ((= 0 (length (car components))) "/")
                                ((= 1 (length (car components)))
                                 (concat (car components) "/"))
                                (t
                                 (if (string= "."
                                              (string (elt (car components) 0)))
                                     (concat (substring (car components) 0 2)
                                             "/")
                                   (string (elt (car components) 0) ?/)))))
              len (- len (1- (length (car components))))
              components (cdr components)))
      (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components)))))

(use-package shell
  :ensure nil
  :bind (("C-c s" . shell)
         :map shell-mode-map
         ("C-r" . consult-history))
  :custom
  (comint-prompt-read-only t))

(use-package vterm
  :ensure nil
  :bind (("C-c t" . vterm)
         :map vterm-mode-map
         ("C-q" . vterm-send-next-key))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-clear-scrollback-when-clearing t))

(use-package rainbow-mode)

(column-number-mode 1)

(setq-default cursor-type 'hbar
              cursor-in-non-selected-windows nil)

(setq use-dialog-box nil)

(use-package time
  :ensure nil
  :defer 2
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  :config
  (display-time-mode 1))

(defun crz/set-font-faces ()
  (set-face-attribute 'default nil :font "Iosevka Comfy 10")
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Comfy 10")
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile 10"))

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
  :custom
  (tab-bar-new-button nil)
  (tab-bar-close-button nil)
  (tab-bar-back-button nil)
  (tab-bar-border nil)
  (tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  (tab-bar-tab-name-truncated-max 15)
  (tab-bar-show 1))

(use-package modus-themes
  :ensure nil
  :custom
  (modus-themes-subtle-line-numbers t)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-mode-line '(borderless))
  (modus-themes-scale-headings t)
  :init
  (load-theme 'modus-vivendi t))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-scope 'frame)
  (aw-ignore-current t))

(use-package popper
  :defer 2
  :bind (("M-'" . popper-toggle-latest)
         ("C-'" . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :custom
  (popper-window-height 17)
  (popper-reference-buffers '("\\*Async Shell Command\\*"
                              "\\*DWIM shell command\\* done"
                              grep-mode
                              debugger-mode))
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package xterm-color)

(use-package comint
  :ensure nil
  :hook (shell-mode . (lambda ()
                        (font-lock-mode -1)
                        (make-local-variable 'font-lock-function)
                        (setq font-lock-function (lambda (_) nil))
                        (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)
                        (setenv "TERM" "xterm-256color")))
  :custom
  (comint-output-filter-functions
   (remove 'ansi-color-process-output comint-output-filter-functions)))

(use-package compile
  :ensure nil
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  :config
  (defun crz/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'crz/advice-compilation-filter))

(use-package esh-mode
  :ensure nil
  :hook (eshell-before-prompt . (lambda () (setq xterm-color-preserve-properties t)))
  :custom
  (xterm-color-use-bold-for-bright t)
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)
  (setenv "TERM" "xterm-256color"))

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package pinentry
  :defer 2
  :custom
  (epg-pinentry-mode 'loopback)
  :config
  (pinentry-start))
