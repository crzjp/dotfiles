;; -*- lexical-binding: t; -*-

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setopt package-user-dir (expand-file-name "elpa" user-emacs-directory))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setopt use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t)

(setopt user-full-name "João Paulo da Cruz"
        user-mail-address "crzzjp@gmail.com")

(keymap-global-set "M-&" 'with-editor-async-shell-command)

(setopt global-auto-revert-non-file-buffers t)

(global-auto-revert-mode 1)

(cd "~/")

(setopt delete-by-moving-to-trash t
        trash-directory (expand-file-name "emacs-trash" temporary-file-directory))

(delete-selection-mode 1)

(setopt kill-do-not-save-duplicates t)

(setopt disabled-command-function nil)

(use-package browse-url
  :ensure nil
  :config
  (when (getenv "BROWSER")
    (setopt browse-url-generic-program (executable-find (getenv "BROWSER"))
            browse-url-browser-function 'browse-url-generic)))

(setopt auto-save-list-file-prefix (expand-file-name "autosaves/" user-emacs-directory)
        auto-save-file-name-transforms `((".*" ,(expand-file-name "autosaves/" user-emacs-directory) t)))

(setopt backup-directory-alist `((".*" . ,(expand-file-name "backups" user-emacs-directory)))
        backup-by-copying t
        version-control t
        delete-old-versions t
        vc-make-backup-files t
        kept-old-versions 10
        kept-new-versions 10)

(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))

(use-package recentf
  :ensure nil
  :defer 2
  :custom
  (recentf-max-saved-items 100)
  (recentf-auto-cleanup (* 5 60))
  (recentf-exclude
   '("\\.[jp][pn]g\\'" "\\.webp\\'" "\\.pdf\\'" "\\.gpg\\'"
     "/usr/.*" "\\.cache/.*" ".*/mail/.*"))
  :config
  (recentf-mode 1))

(setopt vc-follow-symlinks nil)

(setopt help-window-select t)

(setopt Man-notify-method 'pushy
        woman-fill-frame t)

(setenv "PAGER" "cat")
(setenv "MANPAGER" "cat")

(setopt use-short-answers t)

(setopt large-file-warning-threshold nil)

(setopt indent-tabs-mode nil
        tab-width 4)

(keymap-global-unset "C-z")
(keymap-global-unset "C-x C-z")

(setopt sentence-end-double-space nil)

(setopt uniquify-buffer-name-style 'forward)

(setopt bookmark-save-flag 1)

(setopt auth-sources '("~/.authinfo.gpg"))

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
  (corfu-preview-current nil)
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

(setopt read-extended-command-predicate 'command-completion-default-include-p)

(setopt history-length 50
        history-delete-duplicates t)

(savehist-mode 1)

(setopt enable-recursive-minibuffers t)

(minibuffer-depth-indicate-mode 1)

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless))
  (orderless-matching-styles '(orderless-initialism orderless-flex)))

(use-package dired
  :ensure nil
  :bind (("C-x C-d" . dired-jump)
         :map dired-mode-map
         ("f" . dired-create-empty-file))
  :hook (dired-mode . (lambda () (setq truncate-lines t)))
  :custom
  (insert-directory-program "gnuls")
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
      ("Term" (or (mode . eat-mode)
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

(setopt eldoc-echo-area-use-multiline-p nil)

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package magit
  :bind ("C-c g" . magit-status))

(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package gnus
  :ensure nil
  :bind ("C-c m" . gnus)
  :custom
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (message-directory "~/public/mail")
  (mail-source-directory message-directory)
  (gnus-home-directory (expand-file-name "gnus" user-emacs-directory))
  (gnus-directory (expand-file-name "news" gnus-home-directory))
  (gnus-select-method '(nnimap "gmail"
                               (nnimap-address "imap.gmail.com")
                               (nnimap-server-port 993)
                               (nnimap-stream ssl)
                               (nnimap-authinfo-file "~/.authinfo.gpg"))))

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
   '(("libera.chat" "#emacs" "#freebsd" "#freebsd-emacs" "#nixers" "#stumpwm")
     ("slackjeff.com.br" "#mundo-libre")))
  (erc-track-exclude-types
   '("JOIN" "MODE" "NICK" "PART" "QUIT" "324" "329" "332" "333" "353" "477"))
  (erc-prompt-for-password nil)
  (erc-use-auth-source-for-nickserv-password t)
  :config
  (add-to-list 'erc-modules 'autojoin)
  (add-to-list 'erc-modules 'notifications))

(use-package erc-hl-nicks
  :after erc
  :demand nil
  :config
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
  (proced-auto-update-interval 2)
  (proced-enable-color-flag t))

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
  (comint-prompt-read-only t)
  (shell-kill-buffer-on-exit t))

(use-package eat
  :bind ("C-c t" . eat)
  :hook (eshell-load . eat-eshell-visual-command-mode)
  :custom
  (eat-enable-shell-prompt-annotation nil))

(use-package rainbow-mode)

(column-number-mode 1)

(setopt cursor-type 'hbar
        cursor-in-non-selected-windows nil)

(setopt use-dialog-box nil)

(use-package time
  :ensure nil
  :defer 2
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  :config
  (display-time-mode 1))

(defun crz/set-font-faces ()
  (set-face-attribute 'default nil :font "Iosevka Slab" :height 105)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Slab" :height 105)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 105))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (crz/set-font-faces))))
  (crz/set-font-faces))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setopt frame-resize-pixelwise t)

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
  (modus-themes-fringes nil)
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

(setopt ediff-keep-variants nil
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package pinentry
  :defer 2
  :custom
  (epg-pinentry-mode 'loopback)
  :config
  (pinentry-start))
