;;; redo-spacemacs.el --- Redoing spacemacs leader bindings safely -*- lexical-binding: t; -*-

(require 'dash)
(require 'dash-functional)

(provide 'redo-spacemacs)

;;; Redo-spacemacs
;;;; Config

(defvar redo-spacemacs-undo-bindings-alist nil
  "An alist of key/old-def to execute `redo-spacemacs/set-leader-keys' over.

Optionally, a new-def can be included in any ele to indicate a new binding.")

(defvar redo-spacemacs-new-bindings-alist nil
  "Simple alist of key-def to apply `global-set-key' over.")

(defvar redo-spacemacs-prefixes-list nil
  "A list of keys to remove as leader prefixes, along with their bindings.")

;;;; Warnings

(defun redo-spacemacs--warn-no-def (full-prefix old-def)
  (-let [msg (format
              (concat "Undoing binding for `%s' originally calling "
                      "`%s' is not necessary, binding is not set.")
              full-prefix old-def)]
    (display-warning :warning msg)))

(defun redo-spacemacs--warn-change (full-prefix old-def cur-def)
  (-let [msg (format
              (concat "Undoing binding for `%s' "
                      "has changed from previous `%s' to `%s'. "
                      "Not undoing the binding.")
              full-prefix old-def cur-def)]
    (display-warning :warning msg)))

;;;; Functions

(defun redo-spacemacs-set-leader-keys (key old-def &optional new-def)
  "Undo leader binding KEY, warning if OLD-DEF has changed or isn't bound."
  (let* ((full-prefix (concat dotspacemacs-leader-key " " key))
         (full-emacs-prefix (concat dotspacemacs-emacs-leader-key " " key))
         (cur-def (lookup-key (current-global-map) (kbd full-emacs-prefix)))
         (def-changed? (not (eq cur-def old-def))))
    (cond ((not cur-def) (redo-spacemacs--warn-no-def full-prefix old-def))
          (def-changed?  (redo-spacemacs--warn-change full-prefix old-def cur-def))
          (t (spacemacs/set-leader-keys key new-def)))))

(defun redo-spacemacs-prefixes (key)
  "Remove leader prefix for KEY. Keys in KEY should be space-delimited."
  (-let [prefix (concat "SPC " key)]
    (if (assoc prefix which-key--prefix-title-alist)
        (spacemacs/set-leader-keys key nil)
      (display-warning
       :warning
       (format "Attempting to unbind non-existant prefix `%s'." key)))))

(defun redo-spacemacs-new-bindings (key def &rest maps)
  "Wrap `bind-key' for MAPS."
  (bind-key key def)
  (--each maps (bind-key key def (symbol-value it))))

;;;###autoload
(defun redo-spacemacs-bindings ()
  "Remove unused prefixes and bindings and apply new bindings."
  (-each redo-spacemacs-prefixes-list
    #'redo-spacemacs-prefixes)
  (-each redo-spacemacs-undo-bindings-alist
    (-applify #'redo-spacemacs-set-leader-keys))
  (-each redo-spacemacs-new-bindings-alist
    (-applify #'redo-spacemacs-new-bindings)))
