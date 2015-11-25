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
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(helm-external-programs-associations
   (quote
    (("mkv" . "vlc --play-and-exit")
     ("avi" . "vlc --play-and-exit")
     ("sln" . "explorer.exe"))))
 '(magit-diff-arguments
   (quote
    ("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "-M" "-C")))
 '(magit-log-arguments
   (quote
    ("--graph" "--color" "--decorate" "--show-signature" "-n256")))
 '(magit-rebase-arguments (quote ("--autostash")))
 '(package-selected-packages
   (quote
    (powershell zop-to-char expand-region rdp auto-package-update aggressive-indent aggressive-indent-mode smartparens multiple-cursors whitespace-cleanup-mode visual-regexp neotree persp-projectile perspective use-package smart-mode-line-powerline-theme org-plus-contrib omnisharp material-theme magit leuven-theme hi2 helm-projectile ghc avy)))
 '(send-mail-function (quote mailclient-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Me
(setq user-full-name "Bart Post")
(setq user-mail-address "bart.post@gmail.com")

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
(require 'diminish)
(require 'bind-key)

(use-package paradox
  :ensure t
  :config
  (setq paradox-execute-asynchronously t)
  (paradox-enable)
  (paradox-upgrade-packages))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(use-package dash :ensure t)
(use-package s :ensure t)
(use-package f :ensure t)
(use-package rdp :ensure t)

(use-package smart-mode-line-powerline-theme
  :defer t
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package magit
  :ensure t
  :commands (magit-git-repo-p magit-status-internal)
  :bind (("M-G" . magit-status))
  :config
  (setq magit-commit-arguments (if (eq 'system 'gnu/linux)
                                   '("--gpg-sign=9BD68A49AB3D8E4D")
                                 '(""))))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :init
  (require 'helm-config)
  (helm-mode 1))

(defun joranvar/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects.

Based on bh/skip-non-stuck-projects from Bernd Hansen."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (save-excursion
        (forward-line 1)
        (if (or (> (point) subtree-end)
                (not (re-search-forward "^\\*" subtree-end t))
                (re-search-forward "\\*+ TODO " subtree-end t))
            next-headline
          nil) ; a stuck project, has subtasks but no todo task
        ))))

(defun joranvar/skip-scheduled-items ()
  "Skip items that have a scheduled date."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (save-excursion
        (forward-line 1)
        (if (or (> (point) next-headline)
                (re-search-forward "^[ ]*SCHEDULED:" next-headline t))
            next-headline
          nil) ; an item that has no scheduled date
        ))))

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(use-package org
  :ensure org-plus-contrib
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (add-to-list 'org-modules 'org-habit)
  (require 'org-contacts)
  (setq org-agenda-window-setup 'other-frame
        org-agenda-sticky t)
  (setq org-clock-persist t)
  (setq org-agenda-dim-blocked-tasks 'invisible
        org-enforce-todo-dependencies t)
  (setq org-agenda-todo-ignore-scheduled 'future) ;; Ignore TODO items for the future
  (setq org-use-speed-commands
      (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))
  (org-clock-persistence-insinuate)
  (setq org-agenda-files '("~/org/gtd.org"))
  (setq org-capture-templates '(("i" "INBOX" entry (file+headline "gtd.org" "INBOX")
                                 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                                ;; ("j" "Project related journal" entry (file+headline "cgm.org" "Journal")
                                ;;  "* %?\n%U\n" :clock-in t :clock-resume t)
                                ;; ("i" "Interrupt" entry (file+datetree "cgm.org" "JOURNAL")
                                ;;  "* %?\n%U\n" :clock-in t :clock-resume t)
                                ))
  (setq org-agenda-custom-commands
        '(
          (" " "Agenda"
           ((agenda ""
                    ((org-agenda-skip-function nil)))
            (tags-todo "+LEVEL=2/-DONE"
                       ((org-agenda-overriding-header "Stuck")
                        (org-agenda-skip-function 'joranvar/skip-non-stuck-projects)))
            (tags-todo "/TODO"
                  ((org-agenda-overriding-header "Todo")
                   (org-agenda-skip-function 'joranvar/skip-scheduled-items)
                   (org-tags-match-list-sublevels 'indented)))))))
  (setq org-mobile-directory "~/org/mobile/")
  (setq org-feed-alist '(("xkcd" "http://xkcd.com/rss.xml" "~/org/gtd.org" "INBOX"))
        org-feed-default-template "\n* TODO %h\n  %U\n  %description\n  %a\n")
  (org-feed-update-all)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 5)))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3 :properties ("Pomodori") :step day))
  (setq org-refile-target-verify-function 'bh/verify-refile-target)
  (org-babel-do-load-languages 'org-babel-load-languages '((sql . t))))

(use-package htmlize
  :ensure t)

(use-package sauron
  :ensure t
  :config
  (when (eq system-type 'windows-nt)
    (delete 'sauron-dbus sauron-modules))
  (sauron-start))

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

(use-package powershell
  :ensure t)

(use-package ghc
  :ensure t)

(use-package fsharp-mode
  :ensure t)

(use-package linum-relative
  :ensure t
  :defer t
  :config
  (global-linum-mode)
  (linum-relative-on))

(use-package auto-complete
  :ensure t)

(defun joranvar-find-file-upwards (filename)
  "Find the nearest occurrence of FILENAME in the current buffer file directory or upwards."
  (f-expand filename (f--traverse-upwards (f-exists? (f-expand filename it)) (f-dirname buffer-file-name))))

(use-package flycheck
  :ensure t
  :defines fsharp-check flycheck-define-checker
  :config
  (progn
  (flycheck-define-checker fsharp-check
    "My F# checker that utilizes FSharp.mk and fsharp-check."
    :command
    ("sh"
     (eval (joranvar-find-file-upwards "fsharp-check"))
     (eval (joranvar-find-file-upwards "Makefile"))
     source
     (eval (f-filename buffer-file-name)))
    :error-patterns
    ((error line-start (file-name) "(" line "," column "): error "
            (message (and (one-or-more not-newline) "\n" (zero-or-more (and (one-or-more not-newline) "\n")) (or "\n" buffer-end))))
     (warning line-start (file-name) "(" line "," column "): warning "
              (message (and (one-or-more not-newline) "\n" (zero-or-more (and (one-or-more not-newline) "\n")) (or "\n" buffer-end)))))
    :modes fsharp-mode)
  (global-flycheck-mode)))

(use-package highlight-symbol
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook #'highlight-symbol-mode)))

(use-package async
  :ensure t)

(use-package gnus
  :ensure t
  :defines smtpmail-smtp-service smtpmail-default-smtp-server gnus-ignored-newsgroups
  :config
  (use-package gnus-desktop-notify
    :if (eq system-type 'gnu/linux)
    :ensure t
    :config
    (use-package alert
      :ensure t
      :config
      (setq alert-default-style 'libnotify)))
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "gmail"
                        (nnimap-address "imap.gmail.com")
                        (nnimap-server-port "imaps")
                        (nnimap-stream ssl)))
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "cgm"
                        (nnimap-address "localhost")
                        (nnimap-server-port "1143")
                        (nnimap-stream network)))
  (setq nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_)) ) ;; Fix adaptive scoring in [GMAIL] folders
  (setq smtpmail-smtp-service 587
        mail-user-agent 'message-user-agent
        message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com"
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
        mm-discouraged-alternatives '("text/html" "text/richtext") ;; Prefer text/plain
        )
  (setq gnus-select-method '(nntp "news.usenetserver.com"))
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-format "On %a, %b %d %Y, %f writes:\n")
  (setq message-cite-style message-cite-style-outlook)
  (when window-system
    (setq gnus-sum-thread-tree-indent "  ")
    (setq gnus-sum-thread-tree-root "● ")
    (setq gnus-sum-thread-tree-false-root "◯ ")
    (setq gnus-sum-thread-tree-single-indent "◎ ")
    (setq gnus-sum-thread-tree-vertical        "│")
    (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
    (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
  (setq-default gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
                gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
                gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
                gnus-thread-sort-functions '(gnus-thread-sort-by-number (not gnus-thread-sort-by-total-score))
                gnus-subthread-sort-functions '(gnus-sort-thread-by-number))
  (setq gnus-decay-scores t
        gnus-use-adaptive-scoring t)
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode) ;; Show me topics
  (add-hook 'gnus-startup-hook (lambda ()
                                 (if (eq system-type 'gnu/linux) (gnus-desktop-notify-mode))
                                 (gnus-demon-add-handler 'gnus-demon-scan-news 5 t))))

(require 'org-mime)
(add-hook 'message-mode-hook
          (lambda ()
            (orgstruct-mode)
            (local-set-key "\C-c\M-o" (lambda ()
                                        (interactive)
                                        (save-excursion
                                          (message-goto-body)
                                          (when (looking-at "<#secure.*>") (forward-line 1))
                                          (set-mark-command nil)
                                          (insert "#+OPTIONS: toc:nil ^:nil\n")
                                          (goto-char (point-max))
                                          (org-mime-htmlize))))))
(add-hook 'org-mode-hook
          (lambda () (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))

(require 'org-contacts)
(setq org-contacts-files '("~/org/people.org"))

(use-package epg
  :ensure t
  :config
  (setq mml2015-use 'epg

        mml2015-verbose t
        epg-user-id "1C0DD510"
        mml2015-encrypt-to-self t
        mml2015-always-trust nil
        mml2015-cache-passphrase t
        mml2015-passphrase-cache-expiry '36000
        mml2015-sign-with-sender t

        gnus-message-replyencrypt t
        gnus-message-replysign t
        gnus-message-replysignencrypted t
        gnus-treat-x-pgp-sig t

        ;; mm-sign-option 'guided
        ;; mm-encrypt-option 'guided
        mm-verify-option 'always
        mm-decrypt-option 'always

        gnus-buttonized-mime-types '("multipart/alternative" "multipart/encrypted" "multipart/signed"))
  (add-hook 'gnus-message-setup-hook (lambda () (mml-secure-message-sign))))

(use-package eww
  :ensure t
  :config
  (setq browse-url-browser-function 'eww-browse-url))

(defvar joranvar/current-theme nil)
(defun joranvar/toggle-theme (theme)
  "Enable THEME if it is not loaded, disabling all themes that are."
  (if (eq joranvar/current-theme theme)
      (progn
        (setq joranvar/current-theme nil)
        (disable-theme theme))
    (progn
      (disable-theme joranvar/current-theme)
      (setq joranvar/current-theme theme)
      (enable-theme theme))))

(use-package material-theme
  :ensure t
  :config (disable-theme 'material)
  :init (bind-key "C-c t d" (lambda () (interactive) (joranvar/toggle-theme 'material))))

(use-package leuven-theme
  :ensure t
  :config (disable-theme 'leuven)
  :init (bind-key "C-c t l" (lambda () (interactive) (joranvar/toggle-theme 'leuven))))

(use-package rase
  :ensure t
  :config
  (setq calendar-latitude 50.9342277
        calendar-longitude -5.7725223)
  (add-hook 'rase-functions (lambda (sun-event &optional first-run)
                              (if first-run
                                  (cond ((memq sun-event '(sunrise midday))
                                         (joranvar/toggle-theme 'leuven))
                                        (t
                                         (joranvar/toggle-theme 'material))))
                              (cond ((eq sun-event 'sunrise)
                                     (joranvar/toggle-theme 'leuven))
                                    ((eq sun-event 'sunset)
                                     (joranvar/toggle-theme 'material)))))
  :init
  (rase-start t))

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2))
  :init
  (avy-setup-default))

(use-package omnisharp
  :ensure t
  :config
  (add-hook 'csharp-mode-hook #'omnisharp-mode)
  (setq omnisharp-server-executable-path
        (substitute-in-file-name
         (if (eq system-type 'gnu/linux)
             "$HOME/git/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe"
           "D:\\Ontwikkeling\\omnisharp-server\\OmniSharp\\bin\\Debug\\OmniSharp.exe")))
  (use-package company
    :ensure t
    :config
    (add-hook 'after-init-hook 'global-company-mode))
  (define-key omnisharp-mode-map (kbd "M-.") #'omnisharp-auto-complete)
  (define-key omnisharp-mode-map (kbd "M-RET") #'omnisharp-run-code-action-refactoring)
  (define-key omnisharp-mode-map (kbd "<C-return>") #'omnisharp-fix-code-issue-at-point))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode)
  (smartparens-global-strict-mode)
  (sp-use-paredit-bindings))

(use-package aggressive-indent
  :ensure t
  :config
  ;;(global-aggressive-indent-mode)
  (add-hook 'csharp-mode-hook (lambda () (aggressive-indent-mode -1))))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (setq-default whitespace-style '(face spaces tabs newline
                                        space-mark tab-mark ; newline-mark
                                        trailing lines-tail empty
                                        indentation::space
                                        space-after-tab::space)
                whitespace-line-column 160
                indent-tabs-mode nil
                require-final-newline nil)
  (global-whitespace-mode)
  (add-hook 'csharp-mode-hook #'whitespace-cleanup-mode)
  (add-hook 'fsharp-mode-hook #'whitespace-cleanup-mode))

(use-package projectile
  :ensure t
  :bind (("M-p" . helm-projectile)
         ("M-P" . projectile-persp-switch-project))
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (setq projectile-indexing-method 'alien
        projectile-completion-system 'helm
        ; projectile-enable-caching t ;; This messes up tramp-sudo, see https://github.com/bbatsov/projectile/issues/835
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
    (setq projectile-switch-project-action (lambda () (if (magit-git-repo-p (projectile-project-root))
                                                          (magit-status-internal (projectile-project-root))
                                                        (dired (projectile-project-root))))))
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
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)
         ("C-s"   . isearch-forward-regexp)
         ("M-C-s" . isearch-forward)
         ("C-r"   . isearch-backward-regexp)
         ("M-C-r" . isearch-backward)))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window))
  :config
  (setq aw-scope 'frame))

(setq focus-follows-mouse t
      mouse-autoselect-window t)

(use-package flyspell
  :ensure t
  :config
  (setq ispell-program-name "aspell"
        ispell-list-command "--list"))

;; Do not lose my clippings from outside of emacs
(setq save-interprogram-paste-before-kill t)

;; Use hippie expand instead of dabbrev-expand
(global-set-key (kbd "M-/") 'hippie-expand)

(use-package sql-reformat
  :load-path "lisp/sql-reformat"
  :commands sql-reformat)

(use-package tf-git
  :load-path "lisp/tf-git"
  :commands (tf-mark-reviewed
             tf-get-reviewer
             tf-set-jira-issue-id))

(use-package gpg-agent-reload
  :load-path "lisp/gpg-agent-reload"
  :commands (gpg-agent-reload))

(use-package moz-reload
  :load-path "lisp/moz-reload"
  :commands (moz-enable-auto-update
             moz-disable-auto-update)
  :init (use-package moz
          :ensure t))

(use-package ecb
  :ensure t
  :config
  (setq ecb-layout-name "left1")
  (setq-default semantic-symref-tool "global"))

(use-package ggtags
  :ensure t
  :config
  (ggtags-mode 1))

;; Printing
;; TODO: Move to package
(when (eq system-type 'windows-nt)
  (setenv "GS_LIB" "C:/Program Files/gs/gs9.15;C:/Program Files/gs/gs9.15/lib")
  (setq ps-lpr-command "C:/Program Files/gs/gs9.15/bin/gswin64c.exe")
  (setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2"))
  (setq ps-printer-name t))

(when (eq system-type 'windows-nt)
  (setenv "PATH" (concat "C:\\GnuWin\\bin;" (getenv "PATH")))
  (setq exec-path (append '("C:/GnuWin/bin") exec-path)))

(set-face-attribute 'default nil :height (if (eq system-type 'gnu/linux) 100 80))

(defun joranvar-insert-guid ()
  "Insert a guid at point."
  (interactive)
  (s-replace "\r\n" "" (shell-command "C:\\Program Files (x86)\\Windows Kits\\8.1\\bin\\x64\\uuidgen.exe" t)))

(defun sudo-find-file ()
  "Find the file in current buffer with tramp-sudo."
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))

;; This fixes sudo-tramp on NixOS
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;; This fixes the utf-8-signature removing behavior in e.g. nxml
(defun joranvar-save-buffer-and-keep-utf-8-with-signature-dos ()
  (interactive)
  (if (eq buffer-file-coding-system 'utf-8-with-signature-dos)
      (let ((coding-system-for-write 'utf-8-with-signature-dos))
        (save-buffer))
    (save-buffer)))

(define-key (current-global-map) [remap save-buffer] #'joranvar-save-buffer-and-keep-utf-8-with-signature-dos)

(defun joranvar-distinct-matches-in-buffer (regex)
  "Get a list of unique matching occurrences of REGEX in the current buffer."
  (interactive "sRegex: \n")
  (-distinct (s-match-strings-all regex (buffer-substring-no-properties (point-min)(point-max)))))

(server-start)
(tool-bar-mode -1)
(menu-bar-mode -1)

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
