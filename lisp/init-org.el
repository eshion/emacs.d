;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-src-fontify-natively t
      org-tags-column 80)

;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)
;; 不转义 _ 字符
(setq org-export-with-sub-superscripts '{})

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)"))))

;; org-babel-load-languages
;;
;; 1. dita
;; #+BEGIN_SRC ditaa :file ${1:export-file-name} :cmdline -r -s 0.8 
;; ${0}
;; #+END_SRC
;; 2. dot
;; #+BEGIN_SRC dot :file ${1:export-file-name}.png :cmdline -Kdot -Tpng
;; title ${0}
;; #+END_SRC
;; 3. uml
;; #+BEGIN_SRC plantuml :file ${1:export-file-name} :cmdline -charset UTF-8
;; title ${0}
;; #+END_SRC
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   ))

(add-to-list 'org-structure-template-alist
             '("dot" "#+BEGIN_SRC dot :file file_?.png :cmdline -Kdot -Tpng\n?\n#+END_SRC"))

(setq org-confirm-babel-evaluate nil)

;; preview image
(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))


;; 中英文对齐设置,先设置默认字体height:120
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (set-fontset-font "fontset-default"
                                    'chinese-gbk "WenQuanYi Micro Hei Mono 15"))))
  (set-fontset-font "fontset-default" 'chinese-gbk "WenQuanYi Micro Hei Mono 15"))


(defun wl-org-column-view-uses-fixed-width-face ()
  ;; copy from org-faces.el
  (when (fboundp 'set-face-attribute)
    ;; Make sure that a fixed-width face is used when we have a column table.
    (set-face-attribute 'org-column nil
                        :height (face-attribute 'default :height)
                        :family (face-attribute 'default :family))))

(when (and (fboundp 'daemonp) (daemonp))
  (add-hook 'org-mode-hook 'wl-org-column-view-uses-fixed-width-face))

(provide 'init-org)
