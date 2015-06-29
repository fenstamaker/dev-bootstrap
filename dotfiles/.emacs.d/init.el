(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(package-initialize)


(setq package-list
      '(use-package
	saveplace
	undo-tree
	smartparens
	helm
	web-mode
	cider
	company
	flycheck
	rainbow-delimiters
	))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(add-to-list 'load-path (concat user-emacs-directory "packages/"))

;; Hide the startup splash screen
(setq inhibit-startup-message t)

;; Removes scratch message
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

;; Turn off annoying system bell
(setq ring-bell-function 'ignore)

;(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;(load custom-file)

;; Write backups to the backup directory in emacs.d
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

(use-package saveplace
	     :config
	     (setq-default save-place t)
	     (setq save-place-file (expand-file-name "places" user-emacs-directory)))

;; Fix empty pasteboard error
(setq save-interprogram-paste-before-kill nil)


;; Full path in frame title
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Auto-refresh buffers when edits occur outside emacs
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Transparently open compress files
(auto-compression-mode t)

;; Replace 'yes-or-no' with 'y-or-n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use UTF-8 by default
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(show-paren-mode 1)

;; Remove text in selection when inserting text
(delete-selection-mode 1)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)
(setq linum-format " %3d ")

(setq transient-mark-mode t)

;; Lines should be 80 characters wide, not 72 ???
(setq fill-column 80)

;; Smooth scroll (one line at a time)
(setq mouse-wheel-scroll-amount '(1 ((shift) .1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 15)

;; Nicer scrolling with mouse wheel/trackpad.
;; From Graphene
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

;; Scroll one line when hitting bottom of window
(setq scroll-conservatively 10000)

;; Change cursor
(setq-default cursor-type 'box)
(blink-cursor-mode -1)

;; Do not insert tabs
(set-default 'indent-tabs-mode nil)

;; Navigate camelcase words
(global-subword-mode 1)

;; Turn off word wrap
(setq-default truncate-lines t)

;; Remove double space at end of sentence
(set-default 'sentence-end-double-space nil)

(use-package uniquify
	   :config
	   (setq uniquify-buffer-name-style 'forward))


;; Org mode

(use-package ob-core)
(use-package ox-md)
(use-package ox-latex)

(use-package undo-tree
	     :config
	     (global-undo-tree-mode t))

(use-package company
	     :config
	     (global-company-mode t)
	     (setq company-idle-delay 0.125)
	     (setq company-minimum-prefix-length 1))

(use-package flycheck
	     :config
	     (global-flycheck-mode t))

(use-package helm-config
	     :config
	     (helm-mode t)
	     ;; __Fuzzers__
	     
	     (setq helm-M-x-fuzzy-match        t
		   helm-buffers-fuzzy-matching t
		   helm-recentf-fuzzy-match    t
		   helm-semantic-fuzzy-match   t
		   helm-imenu-fuzzy-match      t)
	     
	     ;; __Keybindings__
	     
	     (setq helm-split-window-in-side-p t)
	     
	     (global-unset-key (kbd "C-x c")) ; Remove the default helm key
	     
	     (global-set-key (kbd "s-h") 'helm-command-prefix)
	     
	     (global-set-key (kbd "M-x") 'helm-M-x)
	     (global-set-key (kbd "s-;") 'helm-M-x)
	     
	     (global-set-key (kbd "M-y") 'helm-show-kill-ring)
	     (global-set-key (kbd "s-y") 'helm-show-kill-ring)
	     
	     (global-set-key (kbd "C-x b") 'helm-mini)
	     (global-set-key (kbd "s-b")   'helm-mini)
	     
	     (global-set-key (kbd "C-x C-f") 'helm-find-files)
	     (global-set-key (kbd "s-f")     'helm-find-files)
	     
	     (global-set-key (kbd "C-s") 'helm-occur)
	     (global-set-key (kbd "s-i") 'helm-semantic-or-imenu)
	     
	     (global-set-key (kbd "s-p") 'helm-projectile))

(use-package cider
	     :config
	     (cider-mode t)
	     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(use-package rainbow-delimiters
	     :config
	     (rainbow-delimiters-mode t))

(use-package web-mode
	     :config
	     (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
	     
	     (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(use-package smartparens
	     :config
	     (smartparens-global-mode t)
	     (show-smartparens-global-mode t)
	     (setq show-paren-style 'expression))

(global-auto-revert-mode t)
(global-linum-mode t)
(global-visual-line-mode t)

;; Aliases
(defalias 'ff        'helm-find-files)
(defalias 'files     'helm-find-files)
(defalias 'imenu     'helm-semantic-or-imenu)
(defalias 'paste     'helm-show-kill-ring)
(defalias 'resume    'helm-resume)
(defalias 'search    'helm-occur)
(defalias 'clipboard 'helm-show-kill-ring)
(defalias 'comment   'comment-or-uncomment-region)
(defalias 'start     'beginning-of-line)
(defalias 'begin     'back-to-indentation)
(defalias 'errors    'flycheck-list-errors)
(defalias 'open-repl 'cider-jack-in)
(defalias 'repl      'cider)
(defalias 'kill      'kill-buffer)



; === Shorty ===

(global-set-key (kbd "s-z")       'undo)
(global-set-key (kbd "s-S-z")     'undo-tree-redo)
(global-set-key (kbd "C-a")       'beginning-of-line-or-indentation)
(global-set-key (kbd "s-/")       'comment-or-uncomment-region)
(global-set-key (kbd "s-w")       'kill-buffer)

;; __Navigation__

(global-set-key (kbd "s-<up>")      'backward-paragraph)
(global-set-key (kbd "s-<down>")    'forward-paragraph)
(global-set-key (kbd "s-<left>")    'beginning-of-line-or-indentation)
(global-set-key (kbd "s-<right>")   'end-of-line)
(global-set-key (kbd "C-s-<right>") 'forward-sexp)
(global-set-key (kbd "C-s-<left>")  'backward-sexp)
(global-set-key (kbd "C-s-<up>")    'up-list)
(global-set-key (kbd "C-s-<down>")  'down-list)

;;;; Fonts
(add-to-list 'default-frame-alist '(font . "Source Code Pro-15"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (uniquify web-mode use-package undo-tree smartparens rainbow-delimiters helm flycheck company cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
