(defvar tf-program-location "C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\Common7\\IDE\\TF.exe"
  "The location of the tf executable.")

(defun tf-get-changeset-in-current-line ()
  "Get the (first) changeset id in the current line."
  (let ((s (substring-no-properties (thing-at-point 'line))))
    (when (string-match "TFS_C\\([0-9]*\\)" s)
      (match-string 1 s))))

(defun tf-set-note (changeset-id note-name note-value)
  "Set the changeset note of CHANGESET-ID with name NOTE-NAME to NOTE-VALUE."
  (with-output-to-temp-buffer "*tf*"
    (princ "Before:\n-------\n\n")
    (start-process "tf" "*tf*" tf-program-location
                   "changeset"
                   "/noprompt"
                   changeset-id)
    (princ "\n\nAfter:\n------\n\n")
    (start-process "tf" "*tf*" tf-program-location
                   "changeset"
                   (s-concat "/notes:" note-name "=" note-value)
                   "/noprompt"
                   changeset-id)))

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
          (tf-set-note changeset-id "Code Reviewer" user-full-name))))))

(defun tf-set-jira-issue-id (id)
  "Set the jira issue id note of the changeset-at-point to ID."
  (interactive "sJira Issue ID: \n")
  (let ((changeset-id (tf-get-changeset-in-current-line)))
    (when changeset-id
      (tf-set-note changeset-id "Jira Issue ID" id))))
