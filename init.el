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

(use-package evil
  :ensure t
  :init (evil-mode +1))

(load-theme 'solarized-dark t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(tool-bar-mode -1)

(set-default-font "Inconsolata LGC 14")
