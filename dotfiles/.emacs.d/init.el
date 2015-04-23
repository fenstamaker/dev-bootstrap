(require 'package)

(setq package-list '(undo-tree smartparens helm web-mode cider company flycheck rainbow-delimiters-mode))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless package-archive-contents
    (package-refresh-contents))
    
(dolist (package package-list)
    (unless (package-install-p package)
        (package-install package)))

; === Funnies ===

(defun backup-file-name (fpath)
    "Return a new file path of a given file path.
     If the new path does not exist, create it."
    (let* ( (backupRootDir "~/.emacs.d/backups/")
            (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath))
            (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))) )
        (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
        backupFilePath))

(defun beginning-of-line-or-indentation ()
    "Move cursor to beginning to line or the indentation."
    (interactive)
    (if (bolp)
        (back-to-indentation)
        (beginning-of-line)))



; === Packages ===

(require 'undo-tree)
(require 'smartparens-config)
(require 'helm-config)
(require 'web-mode)
(require 'company)
(require 'flycheck)
(require 'cider)

;; __Package modes__
(global-company-mode t)
(global-undo-tree-mode t)
(global-flycheck-mode t)
(helm-mode t)
(cider-mode t)
(rainbow-delimiters-mode t)

;; __Internal modes__
(global-auto-revert-mode t)
(global-linum-mode 1)
(global-visual-line-mode 1)

(column-number-mode 1)
(transient-mark-mode 1) 
(delete-selection-mode 1)
(subword-mode 1)

;; __Hooks__
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)


; === Aliases ===

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

; === Editor ===

;; __GUI__
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)


;; __Settings___

; Change the backup files to ~/.emacs.d/backups/...
(setq make-backup-file-name-function 'backup-file-name)

(setq auto-save-default   nil)     ; Turn off auto-saves
(setq line-move-visual    nil)     ; Move cursor down by visual line
(setq indent-tabs-mode    nil)     ; Indents -> spaces

(setq ring-bell-function  'ignore) ; Turn off sounds
(setq inhibit-startup-message t)   ; Turn off startup message
(setq linum-format " %4d ")        ; Line number format

;;;; Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) 
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

;;;; Fonts
(add-to-list 'default-frame-alist '(font . "Source Code Pro-15"))

;;;; Company (Auto Complete)
(setq company-idle-delay 0.125
      company-minimum-prefix-length 1)


; === Modes ===

; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))


; === Paren ===

(show-paren-mode 1)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

; Highlights entire paren expression
(setq show-parent-style 'expression)


; === Text ===


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

; === Helm ====

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

(global-set-key (kbd "s-p") 'helm-projectile)

