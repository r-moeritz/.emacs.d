(setq package-selected-packages
  '(ace-jump-mode
    expand-region
    log4j-mode
    markdown-mode
    modeline-posn
    web-mode
    paredit
    smex
    zencoding-mode
    auto-complete
    elpy
    irfc
    magit
    ws-butler
    dtrt-indent
    use-package
    ))

(defvar package-config-funcs
  (list
   'verify-required-packages
   'setup-paredit
   'setup-modeline-posn
   'setup-smex
   'setup-markdown-mode
   'setup-web-mode
   'setup-auto-complete
   'setup-elpy
   'setup-irfc
   'setup-expand-region
   'setup-ace-jump-mode
   'setup-magit
   'setup-dtrt-indent
   'setup-ws-butler
   )
  "List of functions to configure package.el packages.")

(defun verify-required-packages ()
  "Verify that all required package.el packages are installed
and install them if necessary."
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install-selected-packages))

(defun setup-auto-complete ()
  (use-package auto-complete
               :config (ac-config-default)))

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
                            '("\\.\\(md\\|markdown\\|post\\)$" . markdown-mode) t)))

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

(defun rmoritz/paredit-hook ()
  (local-set-key (kbd "RET") 'electrify-return-if-match)
  (paredit-mode t)
  (show-paren-mode t))

(defun setup-paredit ()
  (use-package paredit
               :init
               (add-hook 'emacs-lisp-mode-hook 'rmoritz/paredit-hook)
               (add-hook 'scheme-mode-hook 'rmoritz/paredit-hook)
               (add-hook 'lisp-mode-hook 'rmoritz/paredit-hook)
               (add-hook 'clojure-mode-hook 'rmoritz/paredit-hook)
               (add-hook 'hy-mode-hook 'rmoritz/paredit-hook)))

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

(provide 'setup-packages)
