(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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
  :init (evil-mode +1))

(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode))

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1))

(use-package helm
  :ensure t
  :init (helm-mode +1)
  :config
  (define-key evil-ex-map "b " 'helm-mini))

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
  
;;(use-package smart-mode-line-powerline-theme
;;  :ensure t)

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/theme 'respectful)
  (sml/setup))

;; (define-key evil-ex-map "e" 'helm-find-files)

(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)
