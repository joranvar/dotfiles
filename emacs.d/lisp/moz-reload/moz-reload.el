;;; moz-reload.el --- Reload current buffer in the mozrepl

;;; Commentary:
;; Snatched from http://people.internetconnection.net/2009/02/interactive-html-development-in-emacs/

;;; Code:

(require 'moz)
(require 'json)

(defun moz-update (&rest ignored)
  "Update the remote mozrepl instance.

Any arguments are IGNORED."
  (interactive)
  (comint-send-string (inferior-moz-process)
                      (concat "content.document.body.innerHTML="
                              (json-encode (buffer-string)) ";")))

(defun moz-enable-auto-update ()
  "Automatically update the remote mozrepl on change of this buffer."
  (interactive)
  (add-hook 'after-change-functions 'moz-update t t))

(defun moz-disable-auto-update ()
  "Disable automatic mozrepl updating."
  (interactive)
  (remove-hook 'after-change-functions 'moz-update t))

(provide 'moz-reload)
;;; moz-reload.el ends here
