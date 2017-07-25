;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Functions to configure 3rd party packages

(defvar package-config-funcs
  (list
   'setup-modeline-posn
   'setup-smex
   'setup-markdown-mode
   'setup-web-mode
   'setup-elpy
   'setup-irfc
   'setup-expand-region
   'setup-ace-jump-mode
   'setup-magit
   'setup-dtrt-indent
   'setup-ws-butler
   'setup-company
   'setup-srefactor
   'setup-projectile
   )
  "List of functions to configure package.el packages.")

(defun setup-projectile ()
  (use-package projectile
    :config (projectile-global-mode)))

(defun setup-srefactor ()
  (use-package srefactor
    :init
    (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
    (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
    :bind (("M-RET o" . srefactor-lisp-one-line)
           ("M-RET m" . srefactor-lisp-format-sexp)
           ("M-RET d" . srefactor-lisp-format-defun)
           ("M-RET b" . srefactor-lisp-format-buffer))))

(defun setup-company ()
  (use-package company
    :bind ("S-SPC" . company-complete)
    :init
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-backends (delete 'company-clang company-backends))))

(defun setup-elpy ()
  (use-package elpy
               :config
               (elpy-enable)
               :init
               (setq elpy-default-minor-modes
                     '(eldoc-mode highlight-indentation-mode yas-minor-mode auto-complete-mode))))

(defun setup-irfc ()
  (use-package irfc
               :init
               (setq irfc-directory temporary-file-directory)
               (setq irfc-assoc-mode t)))

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

(defun setup-modeline-posn ()
  (use-package modeline-posn
               :config
               (column-number-mode 1)
               (size-indication-mode 1)))

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

(mapc 'funcall package-config-funcs)

(provide 'setup-remaining-packages)
