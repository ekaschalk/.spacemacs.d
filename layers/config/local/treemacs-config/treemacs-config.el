(require 'macros)
(require 'treemacs)
(require 'treemacs-evil)

(provide 'treemacs-config)


(setq treemacs-show-hidden-files
      nil)
(setq treemacs-silent-refresh
      t)
(setq treemacs-is-never-other-window
      t)
(setq treemacs-filewatch-mode
      nil)

(defun treemacs-ignore-pyfiles-predicates (f path)
  "Python files to ignore in treemacs."
  (pcase f
    ("__init__.py" f)
    ("__pycache__" f)
    (_ nil)))

(push 'treemacs-ignore-pyfiles-predicates
      treemacs-ignored-file-predicates)

(defun evil-previous-line-5 () (interactive) (evil-previous-line 5))
(defun evil-next-line-5 ()     (interactive) (evil-next-line 5))

(evil-global-set-keys
 '(normal)
 (kbd "M-f") 'treemacs-select-window
 (kbd "M-p") 'treemacs-projectile-toggle)

(define-keys treemacs-mode-map
  (kbd "C-k") 'evil-previous-line-5
  (kbd "C-j") 'evil-next-line-5)

(define-key evil-treemacs-state-map
  "l" 'treemacs-visit-node-ace)

(evil-define-key '(normal operator motion emacs) treemacs-mode-map
  "u" 'treemacs-uproot
  "h" 'treemacs-goto-parent-node
  "s" 'treemacs-toggle-show-dotfiles
  "r" 'treemacs-change-root)
