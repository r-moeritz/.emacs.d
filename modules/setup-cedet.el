;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)

(semantic-mode 1)

(defun rmoritz/cedet-hook ()
  (when-let ((projects-file "~/.emacs.d/cedet-projects.el")
             (projects-file-exists-t (file-exists-p projects-file)))
    (load-file projects-file))
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'rmoritz/cedet-hook)
(add-hook 'c-mode-hook 'rmoritz/cedet-hook)
(add-hook 'c++-mode-hook 'rmoritz/cedet-hook)

(require 'ede)
(global-ede-mode)

(provide 'setup-cedet)
