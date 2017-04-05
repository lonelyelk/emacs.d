(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq mac-command-modifier 'super)

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package solarized-theme
  :ensure t
  :init (load-theme 'solarized-dark t))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(tool-bar-mode -1)

(set-default-font "Inconsolata LGC 14")

(use-package evil
  :ensure t
  :init (evil-mode +1)
  :config
  (defun evil-toggle-input-method ()
    "when toggle on input method, switch to evil-insert-state if possible.
    when toggle off input method, switch to evil-normal-state if current state is evil-insert-state"
    (interactive)
    (if (not current-input-method)
	(if (not (string= evil-state "insert"))
	    (evil-insert-state))
      (if (string= evil-state "insert")
	  (evil-normal-state)))
    (toggle-input-method))
  (global-set-key (kbd "C-\\") 'evil-toggle-input-method)
  (global-set-key (kbd "C-c -") (lambda () (interactive) (evil-scroll-line-to-center nil))))

(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode))

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1))

(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  (helm-mode +1)
  :config
  (define-key evil-ex-map "b " 'helm-mini))
;; (define-key evil-ex-map "e" 'helm-find-files)

(use-package projectile
  :ensure t
  :defer t
  :config
  (projectile-global-mode))

(use-package helm-projectile
  :bind (("C-S-P" . helm-projectile-switch-project)
         :map evil-normal-state-map
         ("C-p" . helm-projectile))
  :ensure t
  :config
  (evil-leader/set-key
    "ps" 'helm-projectile-ag
    "pa" 'helm-projectile-find-file-in-known-projects))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package js2-mode
  :ensure t
  :commands js2-jsx-mode
  :mode "\\.jsx?\\'"
  :config
  (setq-default js2-basic-offset 4)
  (setq-default tab-width 4)
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode)))

(use-package linum-relative
  :ensure t
  :init (linum-mode +1)
  :config
  (setq linum-relative-current-symbol "")
  (linum-relative-global-mode))

(use-package company
  :ensure t
  :init (global-company-mode))

(use-package elm-mode
  :ensure t
  :config
  (add-to-list 'company-backends 'company-elm)
  (setq-default elm-format-on-save t))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)
	 ("C-c C-g l" . magit-file-log)
	 ("C-c f" . magit-grep)))

(use-package nyan-mode
  :ensure t
  :config
  (setq nyan-wavy-trail t)
  (if nyan-mode () (nyan-mode t)))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/theme 'respectful)
  (setq sml/shorten-modes t)
  (sml/setup))

(use-package evil-nerd-commenter
  :ensure t
  :init
  (global-set-key (kbd "s-/") 'evilnc-comment-or-uncomment-lines)
  (global-evil-leader-mode)
  (evil-leader/set-key
    "\\" 'evilnc-comment-operator)
  :config
  (setq evilnc-comment-both-snippet-html t))

(use-package wcheck-mode
  :ensure t
  :config
  (global-set-key (kbd "C-c s") 'wcheck-mode)
  (global-set-key (kbd "C-c l") 'wcheck-change-language)
  (global-set-key (kbd "C-c c") 'wcheck-actions)
  (global-set-key (kbd "C-c n") 'wcheck-jump-forward)
  (global-set-key (kbd "C-c p") 'wcheck-jump-backward)
  (setq wcheck-language-data
	'(("Русский"
	   (program . "/usr/local/bin/enchant")
	   (args "-d" "ru" "-l")
	   (action-program . "/usr/local/bin/enchant")
	   (action-args "-d" "ru" "-a")
	   (action-parser . wcheck-parser-ispell-suggestions))
	  ("English"
	   (program . "/usr/local/bin/enchant")
	   (args "-d" "en" "-l")
	   (action-program . "/usr/local/bin/enchant")
	   (action-args "-d" "en" "-a")
	   (action-parser . wcheck-parser-ispell-suggestions))
	  ("Français"
	   (program . "/usr/local/bin/enchant")
	   (args "-d" "fr" "-l")
	   (action-program . "/usr/local/bin/enchant")
	   (action-args "-d" "fr" "-a")
	   (action-parser . wcheck-parser-ispell-suggestions)))))

(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)

(use-package neotree
  :ensure t
  :config
  (evil-leader/set-key
    "n"  'neotree-toggle)
  ;; (setq projectile-switch-project-action 'neotree-projectile-action)
  (add-hook 'neotree-mode-hook
	    (lambda ()
	      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	      (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
	      (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
	      (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
	      (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
	      (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
	      (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)

	      (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
	      (define-key evil-normal-state-local-map (kbd "S") 'neotree-enter-horizontal-split)

	      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

(use-package windmove
  :ensure t
  :config
  (defun ignore-error-wrapper (fn)
    "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
    (lexical-let ((fn fn))
      (lambda ()
	(interactive)
	(ignore-errors
	  (funcall fn)))))
  (global-set-key [s-M-left] (ignore-error-wrapper 'windmove-left))
  (global-set-key [s-M-right] (ignore-error-wrapper 'windmove-right))
  (global-set-key [s-M-up] (ignore-error-wrapper 'windmove-up))
  (global-set-key [s-M-down] (ignore-error-wrapper 'windmove-down)))

(load "~/.emacs.d/ru-syntax.el")
(use-package ru-syntax)

(load "~/.emacs.d/ru-typo.el")
(use-package ru-typo
  :config
  (global-set-key (kbd "C-c r") (lambda ()
				  (interactive)
				  (set-input-method "russian-computer")
				  (turn-on-ru-typo)
				  (turn-on-ru-syntax))))

(global-set-key (kbd "M-s-«") (lambda () (interactive) (indent-region (point-min) (point-max))))
