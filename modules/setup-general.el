;;;; Configuration variables ;;;;

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

;;;; Helper functions ;;;;

(defun hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

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

(defun get-buffers-matching-mode (mode)
  "Return a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching regexp in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;;;; Configuration functions ;;;;

(defvar vanilla-config-funcs
  (list
   'setup-server
   'setup-org-mode
   'setup-tramp
   'setup-nxml
   'global-set-vanilla-keys
   'global-set-vanilla-preferences
   )
  "List of functions to configure a vanilla emacs.")

(defun setup-server ()
  "Start server, suppressing error 'directory ~/.emacs.d/server is unsafe' on Windows."
  (require 'server)
  (when (equal window-system 'w32)
    (defun server-ensure-safe-dir (dir)
      "noop" t))
  (server-start))

(defun setup-tramp ()
  "Configure tramp for SSH."
  (setq tramp-default-method "plink")
  (setq explicit-shell-file-name "/bin/bash"))

(defun setup-nxml ()
  (add-to-list 'auto-mode-alist
               '("\\.\\(xaml\\|config\\)$" . nxml-mode) t))

(defun setup-org-mode ()
  (add-to-list 'auto-mode-alist
               '("\\.org$" . org-mode) t)
  (setq org-log-done t
        org-src-fontify-natively t
        org-insert-mode-line-in-empty-file t
        org-pretty-entities t)
  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key (kbd "\C-cl") 'org-store-link)
              (local-set-key (kbd "\C-ca") 'org-agenda))))

(defun global-set-vanilla-keys ()
  "Set global keyboard shortcuts that work in vanilla Emacs."
  (global-set-key (kbd "C-;") 'comment-or-uncomment-region)
  (global-set-key (kbd "C-M-z") 'multi-occur-in-this-mode))

(defun global-set-vanilla-preferences ()
  "A potpourri of preferences that work in vanilla emacs. Set globally."
  (setq inhibit-startup-message t)                          ;; no splash screen
  (setq-default indent-tabs-mode nil)                       ;; no tabs please
  (prefer-coding-system 'utf-8)                             ;; prefer utf-8
  (add-hook 'text-mode-hook 'turn-on-auto-fill)             ;; auto-fill in text-mode
  (put 'erase-buffer 'disabled nil)                         ;; enable erase-buffer
  (winner-mode 1)                                           ;; winner mode FTW
  (put 'upcase-region 'disabled nil)                        ;; enable upcase-region
  (setq-default fill-column 79)                             ;; fill at col 79
  (savehist-mode 1)                                         ;; minibuffer history
  (cua-mode t)                                              ;; de-facto standard copy 'n paste keys
  (setq cua-auto-tabify-rectangles nil)                     ;; don't tabify after recangles
  (transient-mark-mode 1)                                   ;; no region when it is not highlighted
  (setq cua-keep-region-after-copy t)                       ;; de-facto standard behaviour
  (setq make-backup-files nil)                              ;; stop creating backup~ files
  )

(mapc 'funcall vanilla-config-funcs)

(provide 'setup-general)
