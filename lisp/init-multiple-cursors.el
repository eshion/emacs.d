(require-package 'multiple-cursors)
(require-package 'region-bindings-mode)

(require 'region-bindings-mode)
(region-bindings-mode-enable)
;Binding mouse events
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)
(define-key region-bindings-mode-map ">" 'mc/unmark-previous-like-this)
(define-key region-bindings-mode-map "<" 'mc/unmark-next-like-this)
(define-key region-bindings-mode-map "P" 'mc/skip-to-previous-like-this)
(define-key region-bindings-mode-map "N" 'mc/skip-to-next-like-this)
(provide 'init-multiple-cursors)
