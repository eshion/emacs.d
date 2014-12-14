;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-use-fast-todo-selection t
      org-export-kill-product-buffer-when-displayed t
      org-export-with-sub-superscripts '{}
      org-src-fontify-natively t
      org-tags-column 80)

;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Agenda Setup
(setq org-agenda-files (quote ("~/personal/doc/")))
                               
;; Refile targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)


;; TODO keywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("STARTED" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))
;; TODO state triggers
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("STARTED" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Capture Templates
(setq org-directory "~/personal/doc/")
(setq org-default-notes-file "~/personal/doc/refile.org")

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/personal/doc/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/personal/doc/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/personal/doc/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/personal/doc/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/personal/doc/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/personal/doc/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/personal/doc/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/personal/doc/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

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
