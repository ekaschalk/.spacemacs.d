;; Dumping ground for stuff only relevant to me

(require 'macros)

(provide 'personal)


;; Emacs-anywhere defaults to org-mode rather than markdown-mode
(add-hook 'ea-popup-hook (lambda (&rest args) (org-mode)))

;; Hy-mode development
;; (load-file "~/dev/hy-mode/hy-mode.el")
;; (load-file "~/dev/hy-mode/hy-personal.el")
;; (require 'hy-mode)
;; (require 'hy-personal)

;; Emacs-core development
;; (setq find-function-C-source-directory "~/dev/emacs-dev/src")
