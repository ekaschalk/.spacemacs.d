(require 'clojure-mode)

(provide 'clojure-config)


;;; Bindings

(spacemacs/set-leader-keys-for-major-mode
  'clojure-mode    "," 'lisp-state-toggle-lisp-state)
(spacemacs/set-leader-keys-for-major-mode
  'cider-repl-mode "," 'lisp-state-toggle-lisp-state)
