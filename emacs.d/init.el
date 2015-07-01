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
 '(haskell-process-suggest-remove-import-lines t)
 '(helm-external-programs-associations (quote (("sln" . "explorer.exe"))))
 '(org-agenda-files (quote ("~/org/cgm.org" "~/org/main.org")))
 '(package-selected-packages
   (quote
    (zop-to-char expand-region rdp auto-package-update
                 aggressive-indent aggressive-indent-mode smartparens
                 multiple-cursors whitespace-cleanup-mode visual-regexp neotree
                 persp-projectile perspective use-package
                 smart-mode-line-powerline-theme org-plus-contrib omnisharp
                 material-theme magit leuven-theme hi2 helm-projectile ghc avy)))
 '(send-mail-function (quote mailclient-send-it)))

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

(use-package auto-package-update
  :ensure t
  :config
  (auto-package-update-maybe))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(use-package dash :ensure t)
(use-package s :ensure t)
(use-package rdp :ensure t)

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
  (setq org-agenda-files '("~/org/main.org" "~/org/cgm.org"))
  (setq org-capture-templates '(("t" "Todo" entry (file+headline "cgm.org" "Todo")
                                 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                                ("j" "Project related journal" entry (file+headline "cgm.org" "Journal")
                                 "* %?\n%U\n" :clock-in t :clock-resume t)
                                ("i" "Interrupt" entry (file+datetree "cgm.org" "JOURNAL")
                                 "* %?\n%U\n" :clock-in t :clock-resume t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((sql . t))))

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

(use-package omnisharp
  :ensure t
  :config
  ;; (add-hook 'csharp-mode-hook #'omnisharp-mode)
  (setq omnisharp-server-executable-path
        (substitute-in-file-name
         (if (eq system-type 'gnu/linux)
             "$HOME/git_pull/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe"
           "D:\\Ontwikkeling\\omnisharp-server\\OmniSharp\\bin\\Debug\\OmniSharp.exe")))
  (define-key omnisharp-mode-map (kbd "M-.") #'omnisharp-auto-complete)
  (define-key omnisharp-mode-map (kbd "M-RET") #'omnisharp-run-code-action-refactoring)
  (define-key omnisharp-mode-map (kbd "<C-return>") #'omnisharp-fix-code-issue-at-point))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode)
  (smartparens-global-strict-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode)
  (add-hook 'csharp-mode-hook (lambda () (aggressive-indent-mode -1))))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (setq-default whitespace-style '(face spaces tabs newline
                                        space-mark tab-mark ; newline-mark
                                        trailing lines-tail empty
                                        indentation::space
                                        space-after-tab::space)
                whitespace-line-column 120
                indent-tabs-mode nil)
  (global-whitespace-mode)
  (add-hook 'csharp-mode-hook #'whitespace-cleanup-mode))

(use-package projectile
  :ensure t
  :bind (("M-p" . helm-projectile)
         ("M-P" . projectile-persp-switch-project))
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (setq projectile-indexing-method 'alien
        projectile-completion-system 'helm
        projectile-enable-caching t
        projectile-enable-idle-timer t)
  (use-package perspective
    :ensure t
    :config (progn (persp-mode)
                   (use-package persp-projectile
                     :ensure t)))
  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on)
    (setq projectile-switch-project-action (lambda () (magit-status (projectile-project-root)))))
  (setq magit-repo-dirs (mapcar (lambda (dir)
                                  (substring dir 0 -1))
                                (-filter (lambda (project)
                                           (file-directory-p (concat project "/.git/")))
                                         (projectile-relevant-known-projects)))
        magit-repo-dirs-depth 1))

(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle))) ; TODO: try getting the project root from projectile first

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-next-previous-this)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package visual-regexp
  :ensure t
  :bind (("M-s r" . vr/replace)
         ("M-s q" . vr/query-replace)
         ("M-s m" . vr/mc-mark)))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)))

;; Do not lose my clippings from outside of emacs
(setq save-interprogram-paste-before-kill t)

;; Use hippie expand instead of dabbrev-expand
(global-set-key (kbd "M-/") 'hippie-expand)

(use-package sql-reformat
  :load-path "lisp/sql-reformat"
  :commands sql-reformat)

(set-face-attribute 'default nil :height (if (eq system-type 'gnu/linux) 100 90))

(defun joranvar-insert-guid ()
  "Insert a guid at point."
  (interactive)
  (shell-command "C:\\Program Files (x86)\\Windows Kits\\8.1\\bin\\x64\\uuidgen.exe" t))

(defun sudo-find-file ()
  "Find the file in current buffer with tramp-sudo."
  (interactive)
  (find-file (concat "/sudo:localhost:" buffer-file-name)))

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
