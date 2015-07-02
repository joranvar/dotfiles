(defvar tf-program-location "C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\Common7\\IDE\\TF.exe"
  "The location of the tf executable.")

(defun tf-get-changeset-in-current-line ()
  "Get the (first) changeset id in the current line."
  (let ((s (substring-no-properties (thing-at-point 'line))))
    (when (string-match "TFS_C\\([0-9]*\\)" s)
      (match-string 1 s))))

(defun tf-get-reviewer (&optional changeset-id)
  "Get the reviewer note of CHANGESET-ID.

If changeset-id is nil, get the reviewer note of the changeset id on the current line."
  (interactive)
  (let ((changeset-id (or changeset-id
                          (tf-get-changeset-in-current-line))))
    (when changeset-id
      (let ((output (shell-command-to-string (s-join " "
                                                     (list (s-concat "\"" tf-program-location "\"")
                                                           "changeset"
                                                           "/noprompt"
                                                           changeset-id)))))
        (when (string-match "Code Reviewer:\n    \\(.*\\)" output)
          (message (match-string 1 output)))))))

(defun tf-mark-reviewed ()
  "Set the reviewer note of the changeset-at-point to `variable:user-full-name'."
  (interactive)
  (let ((changeset-id (tf-get-changeset-in-current-line)))
    (when changeset-id
      (let ((reviewer (tf-get-reviewer changeset-id)))
        (if reviewer
            (message (s-concat "Already reviewed by " reviewer "!"))
          (progn
            (start-process "tf" "*tf*" tf-program-location
                           "changeset"
                           (s-concat "/notes:Code Reviewer=" user-full-name)
                           "/noprompt"
                           changeset-id)
            (switch-to-buffer "*tf*")))))))
