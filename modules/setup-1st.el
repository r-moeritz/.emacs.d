;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Hacks to prevent Emacs from messing with our init.el

(setq custom-file "/dev/null")
(defun package--save-selected-packages (&rest opt) nil)
(setq package--init-file-ensured t)

;; Configure package.el and install use-package

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Load use-package, telling it to install packages when necessary.

(require 'use-package)
(setq use-package-always-ensure t)

(provide 'setup-1st)
