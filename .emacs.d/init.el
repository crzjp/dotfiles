;; -*- lexical-binding: t; -*-

(setq straight-check-for-modifications nil)

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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

(with-eval-after-load 'browse-url
  (setq browse-url-generic-program (executable-find (getenv "BROWSER"))
        browse-url-browser-function 'browse-url-generic))

(cd "~/")

(setq-default buffer-file-coding-system 'utf-8-unix
              default-file-name-coding-system 'utf-8-unix
              default-keyboard-coding-system 'utf-8-unix
              default-process-coding-system '(utf-8-unix . utf-8-unix)
              default-sendmail-coding-system 'utf-8-unix
              default-terminal-coding-system 'utf-8-unix)

(global-auto-revert-mode 1)

(setq global-auto-revert-non-file-buffers t)

(setq delete-by-moving-to-trash t
      trash-directory (expand-file-name "emacs-trash" temporary-file-directory))

(setq large-file-warning-threshold nil)

(setq-default indent-tabs-mode nil
              tab-width 4)

(delete-selection-mode 1)

(with-eval-after-load 'help
  (setq help-window-select t))

(defvar-local hide-cursor--original nil)

(define-minor-mode pager-mode
  "View buffer as a pager."
  :global nil
  :lighter " Pager"
  (if pager-mode
      (progn
        (scroll-lock-mode 1)
        (setq-local hide-cursor--original
                    cursor-type)
        (setq-local cursor-type nil))
    (scroll-lock-mode 0)
    (setq-local cursor-type (or hide-cursor--original t))))

(setenv "PAGER" "cat")

(setq disabled-command-function nil)

(global-set-key (kbd "M-&") 'with-editor-async-shell-command)

(setq kill-do-not-save-duplicates t)

(with-eval-after-load 'woman
  (setq woman-fill-frame t))

(straight-use-package 'ace-window)

(with-eval-after-load 'ace-window
  (setq aw-scope 'frame
        aw-ignore-current t))

(global-set-key (kbd "M-o") 'ace-window)

(straight-use-package 'popper)

(with-eval-after-load 'popper
  (setq popper-reference-buffers
        '("\\*Async Shell Command\\*"
          grep-mode
          debugger-mode)))

(global-set-key (kbd "M-'") 'popper-toggle-latest)
(global-set-key (kbd "C-'") 'popper-cycle)
(global-set-key (kbd "C-M-'") 'popper-toggle-type)

(popper-mode 1)
(popper-echo-mode 1)

(setq history-length 50
      history-delete-duplicates t)

(savehist-mode 1)

(setq enable-recursive-minibuffers t)

(straight-use-package 'vertico)

(vertico-mode 1)

(straight-use-package 'orderless)

(with-eval-after-load 'vertico
  (setq completion-styles '(orderless)
        orderless-matching-styles '(orderless-flex)))

(straight-use-package 'consult)

(with-eval-after-load 'consult
  (consult-customize consult-recent-file :preview-key nil)
  (consult-customize consult-org-heading :preview-key nil))

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   'consult-completion-in-region
                 'completion--in-region)
               args)))

(global-set-key (kbd "C-c r") 'consult-recent-file)

(with-eval-after-load 'org
  (define-key org-mode-map "\C-c\o" 'consult-org-heading))

(straight-use-package 'pcmpl-args)

(defun crz/eshell-history-config ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (setq eshell-history-size 1000
        eshell-hist-ignoredups t)
  (define-key eshell-mode-map (kbd "C-r") 'crz/eshell-history-search))

(defun crz/eshell-prompt ()
  (concat
   (propertize " " 'face '(:background "#2544bb"))
   (propertize (abbreviate-file-name (eshell/pwd))
               'face '(:background "#2544bb" :foreground "#ffffff"))
   (propertize " " 'face '(:background "#2544bb"))
   (propertize "$" 'invisible t) " "))

(defun crz/eshell-prompt-config ()
    (setq eshell-prompt-regexp "^[^$\n]*\\\$ "
          eshell-prompt-function 'crz/eshell-prompt))

(straight-use-package 'xterm-color)

(defun crz/eshell-colors-config ()
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))
  (setq xterm-color-use-bold-for-bright t)
  (setenv "TERM" "xterm-256color"))

(defun crz/eshell-history-search ()
  (interactive (unless (derived-mode-p 'eshell-mode)
                 (user-error "Must be called from Eshell buffer")))
  (insert
   (completing-read "Search History: "
                    (delete-dups (ring-elements eshell-history-ring)))))

(defun crz/eshell-alias-config ()
  (setq eshell-aliases-file "~/.emacs.d/eshell-aliases")
  (eshell-read-aliases-list))

(defun crz/eshell-config ()
  (crz/eshell-history-config)
  (crz/eshell-prompt-config)
  (crz/eshell-alias-config)
  (crz/eshell-colors-config)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-buffer-maximum-lines 1000
        eshell-scroll-to-bottom-on-input t))

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook 'crz/eshell-config))

(global-set-key (kbd "C-c e") 'eshell)

(straight-use-package 'vterm)

(with-eval-after-load 'vterm
  (setq vterm-kill-buffer-on-exit t))

(global-set-key (kbd "C-c t") 'vterm)

(straight-use-package 'diredfl)

(with-eval-after-load 'dired
  (setq dired-listing-switches "-lha --group-directories-first")
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "f") 'dired-create-empty-file)
  (diredfl-global-mode))

(global-set-key (kbd "C-x C-d") 'dired-jump)

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

(with-eval-after-load 'ibuffer
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

(with-eval-after-load 'ibuffer
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
           ("Magit" (or (mode . magit-process-mode)
                        (mode . magit-diff-mode)
                        (mode . magit-status-mode)))
           ("Book" (or (mode . pdf-view-mode)
                       (mode . nov-mode)))
           ("Dired" (mode . dired-mode))
           ("Chat" (mode . erc-mode))
           ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*")
                       (mode . help-mode)
                       (mode . woman-mode)
                       (mode . Man-mode)))
           ("Image" (mode . image-mode))
           ("Internal" (name . "^\*.*$"))
           ("Misc" (name . "^.*$")))))
  (setq ibuffer-show-empty-filter-groups nil))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "Default")))

(with-eval-after-load 'ibuffer
  (setq ibuffer-formats '((mark modified read-only locked " "
                                (name 20 20 :left :elide)
                                " "
                                (size-h 11 -1 :right)
                                " "
                                (mode 16 16 :left :elide)
                                " " filename-and-process)
                          (mark " "
                                (name 16 -1)
                                " " filename))))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)

(straight-use-package 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(straight-use-package 'cider)

(setq frame-resize-pixelwise t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(column-number-mode 1)

(straight-use-package 'rainbow-mode)

(setq-default cursor-type 'bar
              cursor-in-non-selected-windows nil)

(blink-cursor-mode 0)

(with-eval-after-load 'tab-bar
  (setq tab-bar-new-button nil
        tab-bar-close-button nil
        tab-bar-back-button nil
        tab-bar-border nil
        tab-bar-tab-name-function 'tab-bar-tab-name-truncated
        tab-bar-tab-name-truncated-max 15
        tab-bar-show 1))

(defvar crz/font "Iosevka Slab 10")

(defun crz/set-font-faces ()
  (set-face-attribute 'default nil :font crz/font)
  (set-face-attribute 'fixed-pitch nil :font crz/font)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Etoile 10"))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (crz/set-font-faces))))
  (crz/set-font-faces))

(setq modus-themes-subtle-line-numbers t
      modus-themes-org-blocks 'gray-background
      modus-themes-mode-line '(borderless))

(load-theme 'modus-operandi t)

(setq use-dialog-box nil)

(straight-use-package '(org :type built-in))

(with-eval-after-load 'org
  (setq org-files-directory "~/media/docs/org"
        org-return-follows-link t))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(with-eval-after-load 'org
  (setq org-startup-indented t
        org-startup-with-inline-images t
        org-image-actual-width '(600)
        org-startup-folded t
        org-hide-emphasis-markers t
        org-ellipsis " ???"))

(add-hook 'org-mode-hook 'visual-line-mode)

(straight-use-package 'org-superstar)

(with-eval-after-load 'org-superstar
  (setq org-superstar-headline-bullets-list '(9673 9675 10040)))

(add-hook 'org-mode-hook 'org-superstar-mode)

(with-eval-after-load 'org
  (setq org-modules '(org-tempo)
        org-src-window-setup 'current-window
        org-edit-src-content-indentation 0)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp")))

(with-eval-after-load 'org
  (setq org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-agenda-files '("~/media/docs/agenda.org")))

(global-set-key (kbd "C-c a") 'org-agenda)

(straight-use-package 'magit)

(straight-use-package 'pdf-tools)

(with-eval-after-load 'pdf-tools
  (setq pdf-view-continuous nil))

(pdf-tools-install :noquery)

(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))

(straight-use-package 'pdf-view-restore)

(add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)

(straight-use-package 'nov.el)
(straight-use-package 'esxml)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(straight-use-package 'erc-hl-nicks)

(with-eval-after-load 'erc
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

(defalias 'erc 'erc-tls)

(with-eval-after-load 'gnus
  (setq gnus-select-method '(nnnil "")
        gnus-secondary-select-methods '((nnimap "mail.riseup.net")
                                        (nnimap "mail.cock.li"))))

(straight-use-package 'transmission)

(with-eval-after-load 'transmission
  (setq transmission-refresh-modes '(transmission-mode
                                     transmission-files-mode
                                     transmission-info-mode
                                     transmission-peers-mode)))

(straight-use-package '0x0)

(straight-use-package 'emms) 

(with-eval-after-load 'emms
  (emms-all)
  (setq emms-source-file-default-directory "~/media/musics"
        emms-player-mpd-music-directory "~/media/musics"
        emms-browser-covers 'emms-browser-cache-thumbnail-async
        emms-source-file-directory-tree-function
        'emms-source-file-directory-tree-find
        emms-player-mpd-server-name "localhost"
        emms-player-mpd-server-port "6600"
        emms-mode-line-format " [%s]")
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (emms-player-mpd-sync-from-mpd)
  (emms-player-mpd-connect)
  (emms-playing-time-display-mode 0))
