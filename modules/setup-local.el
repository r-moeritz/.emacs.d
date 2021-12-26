(defvar local-config-funcs
  (list
   'setup-kickasm-mode))

(defun setup-kickasm-mode ()
  (let ((default-directory (concat user-emacs-directory
          (convert-standard-filename "modules/"))))
    (normal-top-level-add-subdirs-to-load-path))
  (require 'kickasm-mode)
  (customize-set-variable 'kickasm-command "java -cp \"$HOME/Programs/KickAssembler/*\" kickass.KickAssembler")
  (customize-set-variable 'kickasm-assemble-command (concat kickasm-command " -vicesymbols -debugdump"))
  (add-to-list 'auto-mode-alist '("\\.s\\'" . kickasm-mode)))

(mapc 'funcall local-config-funcs)

(provide 'setup-local)
