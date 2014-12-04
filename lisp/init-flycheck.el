(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
(add-hook 'php-mode-hook
          (lambda () (flycheck-mode t)))
(provide 'init-flycheck)
