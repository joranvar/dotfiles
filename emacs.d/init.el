;;; init.el --- The place where everything starts (well, Emacs, but what's the difference?)

;;; Commentary:
;; This is the first file that is loaded when Emacs starts.
;; As Emacs' org-babel is the vehicle that carries the rest of the dotfiles as well, this is taken care of, here.

;;; Code:
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa"))

(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa"        . "http://melpa.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")))

(package-initialize)

(require 'org)

(let ((joranvar:init-path (file-name-directory (or load-file-name buffer-file-name))))
  (org-babel-tangle-file (expand-file-name "init.org" joranvar:init-path))
  (load-file (expand-file-name "init.org.el" joranvar:init-path)))

(provide 'init)
;;; init.el ends here
