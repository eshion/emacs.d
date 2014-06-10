; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto sync for emacs

(defcustom auto-sync-script-name ".sync"
  "customized scripts which to be executed after save-buffer done")

(defun auto-sync-search-script-hierarchy (dir)
  (progn
    (while (not (or (equal dir "/")
                    (file-exists-p
                     (concat dir auto-sync-script-name))))
      (setq dir (file-name-as-directory
                 (file-name-directory
                  (directory-file-name dir)))))
    (if (equal dir "/") nil dir)))

(defun auto-sync-start-process (dir)

  (let ((script (concat dir auto-sync-script-name))
        (process-obj (get-process "auto-sync-process"))
        (fold (progn (string-match dir dir) (replace-match "" nil nil (file-name-directory buffer-file-name) 0))))
    (unless (and process-obj
                 (eq (process-status process-obj) 'run))
      (start-process "auto-sync-process"
                     (get-buffer-create "*Messages*")
                     script "upload" fold (file-name-nondirectory buffer-file-name)))))

(defun auto-sync-run ()
  (interactive)
  (let ((dir (auto-sync-search-script-hierarchy
             (file-name-directory buffer-file-name))))
    (message dir)
    (if dir
        (if (auto-sync-start-process dir)
            (message "Synchronized %s" buffer-file-name)
          (message "Synchronize %s failed" buffer-file-name)
      (message "Wrote %s done" buffer-file-name)))))

(add-hook 'after-save-hook 'auto-sync-run)

(provide 'init-auto-sync)
