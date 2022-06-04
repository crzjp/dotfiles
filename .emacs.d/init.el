;; -*- lexical-binding: t; -*-

(setq straight-check-for-modifications '(check-on-save))

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

(setq backup-directory-alist `((".*" . ,(concat user-emacs-directory "backups")))
      backup-by-copying t
      version-control t
      delete-old-versions t
      vc-make-backup-files t
      kept-old-versions 0
      kept-new-versions 10)

(setq auto-save-list-file-prefix (concat user-emacs-directory "autosaves")
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosaves") t)))

(setq recentf-max-menu-items 11
      recentf-max-saved-items 30
      recentf-auto-cleanup 'never)

(add-hook 'kill-emacs-hook (lambda () (recentf-cleanup)))

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
      trash-directory "/tmp/emacs-trash/")

(setq large-file-warning-threshold nil)

(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)

(straight-use-package 'ace-window)

(with-eval-after-load 'ace-window
  (setq aw-scope 'frame))

(global-set-key (kbd "M-o") 'ace-window)

(setq history-length 25)

(savehist-mode 1)

(setq enable-recursive-minibuffers t)

(straight-use-package 'vertico)

(vertico-mode 1)

(straight-use-package 'orderless)

(with-eval-after-load 'vertico
  (setq completion-styles '(orderless)
        orderless-matching-styles '(orderless-flex)))

(straight-use-package 'consult)

(setq completion-in-region-function 'consult-completion-in-region)

(with-eval-after-load 'consult
  (consult-customize consult-recent-file :preview-key nil)
  (consult-customize consult-org-heading :preview-key nil))

(global-set-key (kbd "C-c r") 'consult-recent-file)

(with-eval-after-load 'org
  (define-key org-mode-map "\C-c\o" 'consult-org-heading))

(defun crz/eshell-prompt ()
  (concat
   (propertize " " 'face '(:background "#2544bb"))
   (propertize (abbreviate-file-name (eshell/pwd)) 'face '(:background "#2544bb" :foreground "#ffffff"))
   (propertize " " 'face '(:background "#2544bb"))
   (propertize "$" 'invisible t) " "))

(defun crz/eshell-history-search ()
  (interactive (unless (derived-mode-p 'eshell-mode)
                 (user-error "Must be called from Eshell buffer")))
  (let ((command (with-temp-buffer
                   (insert-file-contents-literally (concat user-emacs-directory "eshell/history"))
                   (let ((history-list (split-string (buffer-string) "\n" t)))
                     (completing-read "History search: " history-list)))))
    (when command
      (insert command))))

(add-hook 'eshell-first-time-mode-hook
          (lambda ()
            (eshell/alias "f" "find-file $1")
            (eshell/alias "fo" "find-file-other-window $1")
            (eshell/alias "v" "view-file $1")
            (eshell/alias "d" "dired $1")
            (eshell/alias "grep" "grep --color $*")
            (eshell/alias "egrep" "egrep --color $*")
            (eshell/alias "zgrep" "zgrep --color $*")
            (eshell/alias "fgrep" "fgrep --color $*")
            (eshell/alias "-" "cd -")
            (eshell/alias "ll" "ls -lhA --color=always --group-directories-first $*")
            (eshell/alias "ls" "ls -AC --color=always --group-directories-first $*")
            (eshell/alias "rm" "rm -rfvI $*")
            (eshell/alias "cat" "cat -n $*")
            (eshell/alias "ping" "ping -c 3 gnu.org")
            (eshell/alias "cpu" "ps -A --sort -rsz -o pid,comm,pmem,pcpu | awk NR<=20")))

(defun crz/eshell-config ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size 1000
        eshell-buffer-maximum-lines 1000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-prompt-regexp "^[^$\n]*\\\$ "
        eshell-prompt-function 'crz/eshell-prompt)
  (setenv "PAGER" "cat")
  (define-key eshell-mode-map (kbd "C-r") 'crz/eshell-history-search))

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook 'crz/eshell-config))

(global-set-key (kbd "C-c e") 'eshell)

(defun crz/quick-term ()
  (interactive) (ansi-term (getenv "SHELL")))

(with-eval-after-load 'dired
  (setq dired-listing-switches "-lha --group-directories-first")
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(global-set-key (kbd "C-x C-d") 'dired-jump)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ibuffer-mode-hook '(lambda () (ibuffer-auto-mode 1)))

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
  (set-face-attribute 'fixed-pitch nil :font crz/font))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (crz/set-font-faces))))
  (crz/set-font-faces))

(straight-use-package 'modus-themes)

(setq modus-themes-subtle-line-numbers t
      modus-themes-org-blocks 'gray-background)

(modus-themes-load-themes)
(modus-themes-load-operandi)

(setq use-dialog-box nil)

(straight-use-package '(org :type built-in))

(with-eval-after-load 'org
  (setq org-modules '(org-tempo)
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-agenda-files '("~/media/docs/agenda.org")
        org-files-directory "~/media/docs/org"
        org-startup-indented t
        org-startup-with-inline-images t
        org-image-actual-width '(600)
        org-startup-folded t
        org-src-window-setup 'current-window
        org-hide-emphasis-markers t
        org-return-follows-link t
        org-ellipsis " ▾")
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp")))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(global-set-key (kbd "C-c a") 'org-agenda)

(add-hook 'org-mode-hook 'visual-line-mode)

(straight-use-package 'toc-org)

(add-hook 'org-mode-hook 'toc-org-mode)

(with-eval-after-load 'toc-org
  (setq toc-org-max-depth 10))

(straight-use-package 'org-superstar)

(add-hook 'org-mode-hook 'org-superstar-mode)

(with-eval-after-load 'org-superstar
  (setq org-superstar-headline-bullets-list '(9673 9675 10040)))

(straight-use-package 'pdf-tools)

(pdf-tools-install :no-query)

(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))

(with-eval-after-load 'pdf-tools
  (setq pdf-view-continuous nil))

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

(with-eval-after-load 'gnus
  (setq gnus-select-method '(nnnil "")
        gnus-secondary-select-methods '((nnimap "imap.yandex.com")
                                        ; (nnimap "mail.cock.li")
                                        (nnimap "imap.gmail.com"))))

(straight-use-package 'transmission)

(with-eval-after-load 'transmission
  (setq transmission-refresh-modes '(transmission-mode
                                     transmission-files-mode
                                     transmission-info-mode
                                     transmission-peers-mode)))

(straight-use-package '0x0)
