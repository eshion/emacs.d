; list the packages you want
(setq package-list '(use-package))

;; ELPA archive repositories and two packages to install by default.
(setq package-archives
      '(;("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ;("melpa"       . "http://melpa.org/packages/")
        ("popkit"       . "http://elpa.popkit.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")))
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)
(use-package org
  :ensure t
  :init
  (progn
    (setq emacs-base-dir (file-name-directory (or load-file-name (buffer-file-name))))
    (org-babel-load-file (concat emacs-base-dir "eshion.org"))))
