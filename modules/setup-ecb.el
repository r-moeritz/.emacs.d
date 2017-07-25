(add-to-list 'load-path "~/.emacs.d/packages/ecb")
(require 'ecb)

(setq ecb-tip-of-the-day nil)
(custom-set-variables (list 'ecb-options-version ecb-options-version))
(global-set-key (kbd "C-c b") 'ecb-minor-mode)

(provide 'setup-ecb)
