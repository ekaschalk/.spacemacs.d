(require 'python)

(provide 'python-config)


;;; Bindings

(spacemacs/set-leader-keys-for-major-mode
  'python-mode          "," 'lisp-state-toggle-lisp-state)
(spacemacs/set-leader-keys-for-major-mode
  'inferior-python-mode "," 'lisp-state-toggle-lisp-state)
