(require 'org-taskjuggler)
(require 'taskjuggler-mode)

(setq org-export-taskjuggler-target-version 3.5)
(setq org-export-taskjuggler-project-tag "project")
(setq org-export-taskjuggler-resource-tag "resource")
(setq org-export-taskjuggler-default-project-duration 365)
(setq org-export-taskjuggler-default-global-properties "
account cost "费用"
account revenue "收入"
balance cost revenue")

(setq org-export-taskjuggler-default-reports '("include \"reports.tji\""))

(defun org-taskjuggler-generate-reports ()
  (interactive)

  (message "Generate Reports...")
  (let* ((process-name "tj3")
         (filename (expand-file-name
                    (concat
                     (file-name-sans-extension
                      (file-name-nondirectory buffer-file-name))
                     ".tjp")))
         (command (concat process-name " " filename)))
    (start-process-shell-command process-name nil command))
  (message "Generate Reports...Done"))

(add-hook 'org-export-taskjuggler-final-hook 'org-taskjuggler-generate-reports)

(provide 'init-taskjuggler)
