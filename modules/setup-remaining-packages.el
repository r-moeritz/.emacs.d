;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Functions to configure 3rd party packages

(defvar package-config-funcs
  (list
   'setup-smex
   'setup-markdown-mode
   'setup-web-mode
   'setup-elpy
   'setup-expand-region
   'setup-ace-jump-mode
   'setup-magit
   'setup-dtrt-indent
   'setup-ws-butler
   'setup-projectile
   'setup-smartparens
   'setup-alect-themes
   'setup-rjsx-mode
   'setup-basic-mode
   'setup-rust-mode
   'setup-yaml-mode)
  "List of functions to configure package.el packages.")

(defun setup-rjsx-mode ()
  (use-package rjsx-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))))

(defun setup-alect-themes ()
  (use-package alect-themes
    :config
    (load-theme 'alect-dark t)))

(defun setup-smartparens ()
  (use-package smartparens
    :config
    (require 'smartparens-config)
    (show-smartparens-global-mode 1)
    (smartparens-global-mode 1)
    (smartparens-global-strict-mode 1)

    ;; when you press RET, the curly braces automatically
    ;; add another newline
    (sp-with-modes '(c-mode c++-mode)
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
      (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                                ("* ||\n[i]" "RET"))))))

(defun setup-projectile ()
  (use-package projectile
    :config (projectile-mode +1)
    :init
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(defun setup-elpy ()
  (use-package elpy
               :config
               (elpy-enable)
               :init
               (setq elpy-default-minor-modes
                     '(eldoc-mode highlight-indentation-mode yas-minor-mode auto-complete-mode))))

(defun setup-markdown-mode ()
  (use-package markdown-mode
               :init
               (add-to-list 'auto-mode-alist
                            '("\\.\\(md\\|markdown\\)$" . markdown-mode) t)))

(defun setup-smex ()
  (use-package smex
               :bind (("M-x" . smex)
                      ("M-X" . smex-major-mode-commands))
               :config (smex-initialize)))

(defun setup-web-mode ()
  (use-package web-mode
               :init
               (add-to-list 'auto-mode-alist
                            '("\\.html?\\'" . web-mode))))

(defun setup-expand-region ()
  (use-package expand-region
               :bind ("C-=" . er/expand-region)))

(defun setup-ace-jump-mode ()
  (use-package ace-jump-mode
               :bind ("C-." . ace-jump-mode)))

(defun setup-magit ()
  (use-package magit
               :bind ("C-x g" . magit-status)))

(defun setup-dtrt-indent ()
  (use-package dtrt-indent
               :config
               (dtrt-indent-mode 1)
               :init
               (setq dtrt-indent-verbosity 0)))

(defun setup-ws-butler ()
  (use-package ws-butler
               :init
               (add-hook 'prog-mode-hook 'ws-butler-mode)
               (add-hook 'text-mode 'ws-butler-mode)
               (add-hook 'fundamental-mode 'ws-butler-mode)))

(defun setup-basic-mode ()
  (use-package basic-mode
    :config
    (setq basic-auto-number 10
          basic-line-number-cols 5)))

(defun setup-rust-mode ()
  (use-package rust-mode))

(defun setup-yaml-mode ()
  (use-package yaml-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.\\(yml\\|yaml\\)$" . yaml-mode))))

(mapc 'funcall package-config-funcs)

(provide 'setup-remaining-packages)
