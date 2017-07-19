(defvar required-package-names
  (list
   'ace-jump-mode
   'expand-region
   'htmlize
   'log4j-mode
   'markdown-mode
   'modeline-posn
   'web-mode
   'paredit
   'smex
   'zencoding-mode
   'auto-complete
   'elpy
   'irfc
   )
  "List of package.el packages that should be installed if not present")

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
   'global-set-package-keys
   )
  "List of functions to configure package.el packages.")

(defun verify-required-packages ()
  "Verify that all required package.el packages are installed
and install them if necessary."
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (pkg required-package-names)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(defun setup-auto-complete ()
  (require 'auto-complete-config)
  (ac-config-default))

(defun setup-elpy ()
  (elpy-enable)
  (setq elpy-default-minor-modes
        '(eldoc-mode highlight-indentation-mode yas-minor-mode auto-complete-mode)))

(defun setup-irfc ()
  (setq irfc-directory temporary-file-directory)
  (setq irfc-assoc-mode t))

(defun setup-markdown-mode ()
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist
               '("\\.\\(md\\|markdown\\|post\\)$" . markdown-mode) t))

(defun setup-smex ()
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(defun setup-web-mode ()
  (require 'web-mode)
  (add-to-list 'auto-mode-alist
               '("\\.html?\\'" . web-mode)))

(defun global-set-package-keys ()
  "Set global keyboard shortcuts that rely on package.el packages."
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C-@") 'ace-jump-mode))

(defun setup-paredit ()
  "Enable paredit for all our Lisps."
  (let ((gen-enable-paredit
         (lambda ()
           (lambda ()
             (paredit-mode t)
             (show-paren-mode t)
             (local-set-key (kbd "RET") 'electrify-return-if-match)))))
    (add-hook 'emacs-lisp-mode-hook (funcall gen-enable-paredit))
    (add-hook 'scheme-mode-hook (funcall gen-enable-paredit))
    (add-hook 'lisp-mode-hook (funcall gen-enable-paredit))
    (add-hook 'clojure-mode-hook (funcall gen-enable-paredit))
    (add-hook 'hy-mode-hook (funcall gen-enable-paredit))))

(defun setup-modeline-posn ()
  (column-number-mode 1)
  (size-indication-mode 1))

(mapc 'funcall package-config-funcs)

(provide 'setup-packages)
