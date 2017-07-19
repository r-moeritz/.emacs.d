;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/modules")

(require 'setup-general)
(require 'setup-packages)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (irfc elpy auto-complete zencoding-mode smex paredit web-mode modeline-posn markdown-mode log4j-mode htmlize expand-region ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
