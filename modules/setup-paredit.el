;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; "Electric return" feature for Paredit

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun rmoritz/paredit-hook ()
  (local-set-key (kbd "RET") 'electrify-return-if-match)
  (paredit-mode t)
  (show-paren-mode t))

;; Use Paredit in all Lisp dialects

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook 'rmoritz/paredit-hook)
  (add-hook 'scheme-mode-hook 'rmoritz/paredit-hook)
  (add-hook 'lisp-mode-hook 'rmoritz/paredit-hook)
  (add-hook 'clojure-mode-hook 'rmoritz/paredit-hook)
  (add-hook 'hy-mode-hook 'rmoritz/paredit-hook))

(provide 'setup-paredit)
