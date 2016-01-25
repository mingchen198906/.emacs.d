(add-hook 'smartparens-enable-hook #'evil-smartparens-mode)
(smartparens-global-mode t)
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-strict-mode)


(provide 'list-smartparens)
