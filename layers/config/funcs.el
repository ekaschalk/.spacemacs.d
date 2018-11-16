;;; Funcs -*- lexical-binding: t; -*-
;;; Unowned Packages
;;;; Eshell

(defun eshell-pop-eshell ()
  "Eshell popup straight to insert mode."
  (interactive)
  (spacemacs/shell-pop-eshell nil)
  (if (string= major-mode "eshell-mode")
      (evil-insert 1)
    (evil-escape)))

;;;; Evil

(defun evil-execute-q-macro ()
  "Execute macro stores in q-register, ie. run `@q'."
  (interactive)
  (evil-execute-macro 1 "@q"))

(defun evil-scroll-to-center-advice (&rest args)
  "Scroll line to center, for advising functions."
  (evil-scroll-line-to-center (line-number-at-pos)))

(defun evil-end-of-line-interactive ()
  "Wrap `evil-end-of-line' in interactive, fix point being 1+ in vis state."
  (interactive)
  (evil-end-of-line))

;;;; Org

(defun org-sort-entries-priorities ()
  "Run `org-sort-entries' for priorities."
  (interactive)
  (org-sort-entries nil ?p))

;;; Owned Packages
;;;; Outshine

(when (configuration-layer/package-used-p 'outshine)
  (defun outshine-advise-narrow-start-pos ()
    "Narrowing works within the headline rather than requiring to be on it."
    (unless (outline-on-heading-p t)
      (outline-previous-visible-heading 1))))
