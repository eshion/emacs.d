; list the packages you want
(setq package-list '(use-package))

;; ELPA archive repositories and two packages to install by default.
(setq package-archives
      '(("gnu"         . "http://elpa.emacs-china.org/gnu/")
	("org"         . "http://elpa.emacs-china.org/org/")
	("melpa"       . "http://elpa.emacs-china.org/melpa/")
	;("melpa"       . "http://melpa.org/packages/")
        ;("popkit"       . "http://elpa.popkit.org/packages/")
	("marmalade"   . "http://elpa.emacs-china.org/marmalade/")))
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
    (org-babel-load-file (concat emacs-base-dir "config.org"))))
(put 'dired-find-alternate-file 'disabled nil)
