;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; ----------------------------------------------------------------------
;;                              VARIABLES
;; ----------------------------------------------------------------------

(defvar required-package-names
  (list 'color-theme-solarized
        'ace-jump-mode
        'csharp-mode
        'expand-region
        'gnuplot-mode
        'go-mode
        'haskell-mode
        'htmlize
        'log4j-mode
        'markdown-mode
        'modeline-posn
        'multi-web-mode
        'nrepl
        'paredit
        'powershell-mode
        'smex
        'zencoding-mode
        'fsharp-mode
        'python
        'graphviz-dot-mode
        ) 
  "List of package.el packages that should be installed if not present")

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

;; ----------------------------------------------------------------------
;;                            HELPER FUNCTIONS
;; ----------------------------------------------------------------------

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

(defun global-set-font (font)
  "Globally sets the font to FONT"
  (let ((fontify-frame
         (lambda (frame)
           (set-frame-parameter frame 'font font))))
    (funcall fontify-frame nil)
    (push fontify-frame after-make-frame-functions)))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun global-set-umlaut-keys ()
  (let ((gen-insert-key
         (lambda (key)
           (lambda ()
             (interactive)
             (ucs-insert key)))))
    (global-set-key (kbd "\C-co") (funcall gen-insert-key #xf6))
    (global-set-key (kbd "\C-cu") (funcall gen-insert-key #xfc))
    (global-set-key (kbd "\C-ca") (funcall gen-insert-key #xe4))
    (global-set-key (kbd "\C-cs") (funcall gen-insert-key #xdf))))

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
;;                            SETUP FUNCTIONS
;; ----------------------------------------------------------------------

(defun setup-paredit ()
  "Enable paredit for Elisp and Common Lisp."
  (let ((gen-enable-paredit
         (lambda () 
           (lambda ()
             (paredit-mode t)
             (show-paren-mode t)
             (local-set-key (kbd "RET") 'electrify-return-if-match)))))
    (add-hook 'emacs-lisp-mode-hook (funcall gen-enable-paredit))
    (add-hook 'lisp-mode-hook (funcall gen-enable-paredit))))

(defun setup-org-mode ()
  "setup org-mode"
  (add-to-list 'auto-mode-alist
	       '("\\.org$" . org-mode) t)
  (setq org-log-done t 
        org-src-fontify-natively t
        org-insert-mode-line-in-empty-file t)
  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key (kbd "\C-cl") 'org-store-link)
              (local-set-key (kbd "\C-ca") 'org-agenda))))

(defun setup-server ()
   "start server, suppressing error 'directory ~/.emacs.d/server is unsafe' on windows" 
  (require 'server)
  (when (equal window-system 'w32)
    (defun server-ensure-safe-dir (dir)
      "noop" t))
  (server-start))

(defun set-preferences ()
  "misc preferences"
  (setq inhibit-startup-message t)                  ;; disable annoying splash screen
  (setq-default indent-tabs-mode nil)               ;; no tabs please
  (prefer-coding-system 'utf-8)                     ;; prefer utf-8
  (add-hook 'text-mode-hook 'turn-on-auto-fill)     ;; enable auto-fill in text-mode
  (global-set-font "Consolas-11:antialias=natural") ;; global font
  (put 'erase-buffer 'disabled nil)                 ;; enable erase-buffer
  (winner-mode 1)                                   ;; winner mode FTW
  (put 'upcase-region 'disabled nil)                ;; enable upcase-region
  )                                  

(defun setup-tramp ()
  (setq tramp-default-method "plink")
  (setq explicit-shell-file-name "/bin/bash"))

(defun setup-modeline-posn ()
  "setup modeline-posn"
  (column-number-mode 1)
  (size-indication-mode 1))

(defun setup-markdown-mode ()
  "setup markdown-mode"
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist
	       '("\\.\\(md\\|markdown\\|post\\)$" . markdown-mode) t))

(defun install-package-archives ()
  "add package.el archives"
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

(defun setup-powershell-mode ()
  "setup powershell-mode"
  (require 'powershell-mode)
  (add-to-list 'auto-mode-alist
               '("\\.ps[md]?1$" . powershell-mode) t ))

(defun setup-smex ()
  "setup smex"
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(defun setup-scons ()
  (add-to-list 'auto-mode-alist
               '("SConstruct" . python-mode) t)
  (add-to-list 'auto-mode-alist
               '("SConscript" . python-mode) t))

(defun setup-multi-web ()
  (require 'multi-web-mode)
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags '((js-mode "<script[^>]*>" "</script>")
                    (css-mode "<style[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("htm" "html"))
  (multi-web-global-mode 1))

(defun setup-gnuplot-mode ()
  (require 'gnuplot-mode)
  (setq gnuplot-program "C:\\Program Files (x86)\\gnuplot\\bin\\gnuplot.exe")
  (add-to-list 'auto-mode-alist
             '("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode) t))

(defun setup-fsharp-mode ()
  (setq inferior-fsharp-program "\"C:\\Program Files (x86)\\Microsoft SDKs\\F#\\3.0\\Framework\\v4.0\\fsi.exe\"")
  (setq fsharp-compiler "\"C:\\Program Files (x86)\\Microsoft SDKs\\F#\\3.0\\Framework\\v4.0\\fsc.exe\""))

(defun verify-required-packages ()
  "Verify that all required package.el packages are installed
and install them if necessary"
  (package-initialize)
  (install-package-archives)
  (unless (package-contents-exist-p)
    (package-refresh-contents))
  (dolist (pkg required-package-names)
    (unless (package-installed-p pkg)
      (package-install pkg))))

;; ----------------------------------------------------------------------
;;                             INIT FUNCTIONS
;; ----------------------------------------------------------------------

(defun init-vanilla ()
  "startup code that works on vanilla emacs"
  (set-preferences)
  (setup-server)
  (setup-org-mode)
  (setup-scons)
  (setup-tramp)
  
  ;; global keyboard shortcuts
  (global-set-umlaut-keys)
  (global-set-key (kbd "C-;") 'comment-or-uncomment-region)
  (global-set-key (kbd "C-M-z") 'multi-occur-in-this-mode))

(defun init-package ()
  "startup code that relies on package"
  (verify-required-packages)

  (load-theme 'solarized-dark t)
  
  (setup-paredit)
  (setup-modeline-posn)
  (setup-smex)
  (setup-powershell-mode)
  (setup-markdown-mode)
  (setup-multi-web)
  (setup-gnuplot-mode)
  (setup-fsharp-mode)
  
  ;; global keyboard shortcuts
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C-@") 'ace-jump-mode))

(defun init-local ()
  "startup code that relies on local customizations"
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "c:/ccl/wx86cl.exe"))

(init-vanilla)
(init-package)
(init-local)

