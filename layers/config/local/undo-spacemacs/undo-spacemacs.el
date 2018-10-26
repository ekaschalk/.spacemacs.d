;; A pkg for wrapping undoing spacemacs leader bindings with warnings for
;; when keybindings change. The alist is configured in the config/packages.el.

(require 'dash)
(require 'dash-functional)

(provide 'undo-spacemacs)

;;; Undo-spacemacs
;;;; Config

(defvar undo-spacemacs-bindings-alist nil
  "An alist of key/old-def to execute `undo-spacemacs/set-leader-keys' over.

Optionally, a new-def can be included in any ele to indicate a new binding.")

(defvar undo-spacemacs-prefixes-list nil
  "A list of keys to remove as leader prefixes, along with their bindings.")

;;;; Functions

(defun undo-spacemacs-set-leader-keys (key old-def &optional new-def)
  "Undo leader binding KEY, warning if OLD-DEF has changed or isn't bound."
  (let* ((full-prefix (concat dotspacemacs-leader-key " " key))
         (full-emacs-prefix (concat dotspacemacs-emacs-leader-key " " key))
         (cur-def (lookup-key (current-global-map) (kbd full-emacs-prefix)))
         (def-changed? (not (eq cur-def old-def))))
    (cond ((not cur-def) (display-warning
                          :warning
                          (format
                           (concat "Undoing binding for `%s' originally calling "
                                   "`%s' is not necessary, binding is not set.")
                           full-prefix old-def)))
          (def-changed? (display-warning
                         :warning
                         (format
                          (concat "Undoing binding for `%s' "
                                  "has changed from previous `%s' to `%s'. "
                                  "Not undoing the binding.")
                          full-prefix old-def cur-def)))
          (t (spacemacs/set-leader-keys key new-def)))))

(defun undo-spacemacs-prefixes (key)
  "Remove leader prefix for KEY. Keys in KEY should be space-delimited."
  (-let [prefix (concat "SPC " key)]
    (if (assoc prefix which-key--prefix-title-alist)
        (spacemacs/set-leader-keys key nil)
      (display-warning
       :warning
       (format "Attempting to unbind non-existant prefix `%s'." key)))))

(defun undo-spacemacs-bindings ()
  "Run `undo-spacemacs-set-leader-keys' over `undo-spacemacs-bindings-alist'."
  (-each undo-spacemacs-prefixes-list #'undo-spacemacs-prefixes)
  (-each undo-spacemacs-bindings-alist
    (-applify #'undo-spacemacs-set-leader-keys)))
