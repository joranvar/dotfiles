;;; gpg-agent-reload --- Reload gpg-agent settings if e.g. the scdaemon/yubikey was hanging again

;;; Commentary:

;;; Code:

(defun gpg-agent-reload (gpg-agent-info ssh-auth-sock)
  "Reload gpg-agent settings.

GPG-AGENT-INFO is the string to set in environment variable GPG_AGENT_INFO.
SSH-AUTH-SOCK is the string to set in environment variable SSH_AUTH_SOCK."
  (setenv "GPG_AGENT_INFO" gpg-agent-info)
  (setenv "SSH_AUTH_SOCK" ssh-auth-sock))

(provide 'gpg-agent-info)
;;; gpg-agent-reload.el ends here
