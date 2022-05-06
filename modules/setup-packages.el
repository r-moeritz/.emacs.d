;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Functions to configure 3rd party packages

(defvar package-config-funcs
  (list
   'setup-smex
   'setup-markdown-mode
   'setup-elpy
   'setup-expand-region
   'setup-ace-jump-mode
   'setup-magit
   'setup-dtrt-indent
   'setup-ws-butler
   'setup-smartparens
   'setup-theme
   'setup-basic-mode
   'setup-yaml-mode
   'setup-ace-window
   'setup-slime)
  "List of functions to configure package.el packages.")

(defun setup-ace-window ()
  (use-package ace-window
    :bind ("M-o" . ace-window)))

(defun setup-theme ()
  (use-package immaterial-theme
    :ensure t
    :config
    (load-theme 'immaterial-dark t)))

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

(defun setup-elpy ()
  (use-package elpy
               :config
               (elpy-enable)
               (when (require 'flycheck nil t)
                 (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

               :init
               (setq elpy-default-minor-modes
                     '(eldoc-mode highlight-indentation-mode yas-minor-mode auto-complete-mode)))
  (use-package flycheck
    :init
    (add-hook 'elpy-mode-hook #'flycheck-mode))
  (use-package blacken
    :init
    (add-hook 'elpy-mode-hook #'blacken-mode)))

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
               (add-hook 'prog-mode-hook #'ws-butler-mode)
               (add-hook 'text-mode #'ws-butler-mode)
               (add-hook 'fundamental-mode #'ws-butler-mode)))

(defun setup-basic-mode ()
  (use-package basic-mode
    :config
    (setq basic-auto-number 10
          basic-line-number-cols 5)))

(defun setup-yaml-mode ()
  (use-package yaml-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.\\(yml\\|yaml\\)$" . yaml-mode))))

(defun setup-slime ()
  (use-package slime
    :config
    (setq inferior-lisp-program  "sbcl")))

(mapc 'funcall package-config-funcs)

(provide 'setup-packages)
