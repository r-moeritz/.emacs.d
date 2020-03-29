;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/modules")

(require 'setup-1st)

(require 'setup-general)
(require 'setup-cedet)
(require 'setup-helm)
(require 'setup-remaining-packages)
(put 'downcase-region 'disabled nil)
