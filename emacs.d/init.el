;;; init.el --- My init file
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Enable package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package smart-mode-line-powerline-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'powerline)
  (sml/setup))

(use-package magit
  :ensure t
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x))
  :init
  (require 'helm-config)
  (helm-mode 1))

(use-package org
  :ensure org-plus-contrib
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (require 'org-contacts)
  (setq org-agenda-files '("~/org/main.org")))

(use-package nix-mode)

(use-package hi2
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'hi2-mode))

(use-package ghc
  :ensure t)

(use-package auto-complete
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package async
  :ensure t)

(use-package gnus
  :ensure t)

(use-package material-theme
  :defer t
  :ensure t)

(use-package leuven-theme
  :defer t
  :ensure t)

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2))
  :init
  (avy-setup-default))

(set-face-attribute 'default nil :height (if (eq system-type 'gnu/linux) 100 90))

(defun sudo-find-file ()
    "Find the file in current buffer with tramp-sudo."
    (interactive)
  (find-file (concat "/sudo:localhost:" buffer-file-name)))

(provide 'init)
;;; init.el ends here
