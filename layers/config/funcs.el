(defun config/shell-pop-eshell ()
  "Eshell popup straight to insert mode."
  (interactive)
  (spacemacs/shell-pop-eshell nil)
  (if (string= major-mode "eshell-mode")
      (evil-insert 1)
    (evil-escape)))

(defun config/execute-q-macro ()
  "Execute @q"
  (interactive)
  (evil-execute-macro 1 "@q"))

(defun config/scroll-to-center-advice (&rest args)
  "Scroll line to center, for advising functions."
  (evil-scroll-line-to-center (line-number-at-pos)))

(when (configuration-layer/package-usedp 'olivetti)
  (defun olivetti ()
    "Integrate `olivetti-mode' and `spacemacs/toggle-maximize-buffer'."
    (interactive)
    (if olivetti-mode
        (spacemacs/toggle-maximize-buffer)
      (spacemacs/toggle-maximize-buffer)
      (olivetti-mode 1)))

  (defun olivetti-end (&rest args)
    "Advise `spacemacs/toggle-maximize-buffer' to disable olivetti."
    (olivetti-mode 0)))
