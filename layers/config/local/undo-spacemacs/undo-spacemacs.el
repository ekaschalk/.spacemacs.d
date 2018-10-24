;; A pkg for wrapping undoing spacemacs bindings with warnings for
;; when things change

(require 'dash)
(require 'dash-functional)

(provide 'undo-spacemacs)

;;; Undo-spacemacs

(defvar undo-spacemacs-bindings-alist nil
  "An alist of key/old-def to execute `undo-spacemacs/set-leader-keys' over.")

(defun undo-spacemacs-set-leader-keys (key old-def)
  "Undo leader binding KEY, warning if OLD-DEF has changed or isn't bound."
  (let* ((full-prefix (concat dotspacemacs-leader-key " " key))
         (full-emacs-prefix (concat dotspacemacs-emacs-leader-key " " key))
         (cur-def (lookup-key (current-global-map) (kbd full-emacs-prefix)))

         (def-changed? (not (eq cur-def old-def)))

         ;; (def-changed? (and (not (null cur-def))
         ;;                    (symbolp cur-def)
         ;;                    (not (string= (symbol-name cur-def)
         ;;                                  (symbol-name old-def)))))
         )
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
          (t (spacemacs/set-leader-keys key nil)))))

(defun undo-spacemacs-bindings ()
  "Run `undo-spacemacs-set-leader-keys' over `undo-spacemacs-bindings-alist'."
  (-each undo-spacemacs-bindings-alist
    (-applify #'undo-spacemacs-set-leader-keys)))
