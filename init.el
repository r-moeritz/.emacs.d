;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; ----------------------------------------------------------------------
;;                              VARIABLES
;; ----------------------------------------------------------------------

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

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

;; ----------------------------------------------------------------------
;;                            HELPER FUNCTIONS
;; ----------------------------------------------------------------------

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

(defun package-contents-exist-p ()
  "Determine if cached package.el archive contents exist."
  (let ((exist-p t))
    (dolist (archive package-archives)
      (let* ((dir (concat "archives/" (car archive)))
             (contents-file (concat dir "/archive-contents"))
             (filename (expand-file-name contents-file package-user-dir)))
        (unless (file-exists-p filename)
          (setq exist-p nil))))
    exist-p))

;; ----------------------------------------------------------------------
;;                            VANILLA CONFIG FUNCTIONS
;; ----------------------------------------------------------------------

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
  (savehist-mode 1)                                         ;; i want mibuffer history
  (cua-mode t)                                              ;; i want standard copy 'n paste keys
  (setq cua-auto-tabify-rectangles nil)                     ;; don't tabify after recangles
  (transient-mark-mode 1)                                   ;; no region when it is not highlighted
  (setq cua-keep-region-after-copy t)                       ;; std windows behaviour
  (setq make-backup-files nil)                              ;; stop creating backup~ files
  )

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

;; ----------------------------------------------------------------------
;;                            PACKAGE CONFIG FUNCTIONS
;; ----------------------------------------------------------------------

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

(defun setup-markdown-mode ()
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist
	       '("\\.\\(md\\|markdown\\|post\\)$" . markdown-mode) t))

(defun install-package-archives ()
  "Add package.el archives."
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

(defun setup-smex ()
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(defun setup-web-mode ()
  (require 'web-mode)
  (add-to-list 'auto-mode-alist
               '("\\.html?\\'" . web-mode)))

(defun verify-required-packages ()
  "Verify that all required package.el packages are installed
and install them if necessary."
  (package-initialize)
  (setq package-enable-at-startup nil)
  (install-package-archives)
  (unless (package-contents-exist-p)
    (package-refresh-contents))
  (dolist (pkg required-package-names)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(defun setup-auto-complete ()
  (require 'auto-complete-config)
  (ac-config-default))

(defun setup-elpy ()
  (elpy-enable)
  ;; Disable flymake because I can't get it working with python3
  (setq elpy-default-minor-modes 
        '(eldoc-mode highlight-indentation-mode yas-minor-mode auto-complete-mode)))

(defun setup-irfc ()
  (setq irfc-directory temporary-file-directory)
  (setq irfc-assoc-mode t))

;; ----------------------------------------------------------------------
;;                             INIT FUNCTIONS
;; ----------------------------------------------------------------------

(defun init-vanilla ()
  "Startup code that works on vanilla emacs."
  (mapc 'funcall vanilla-config-funcs))

(defun init-package ()
  "Startup code that relies on package."
  (mapc 'funcall package-config-funcs))

;; ----------------------------------------------------------------------
;;                             MAIN FUNCTION
;; ----------------------------------------------------------------------

(defun main ()
  (init-vanilla)
  (init-package))

(main)

