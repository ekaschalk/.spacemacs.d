(require 'evil)
(require 'macros)

(provide 'eshell-config)


(defun eshell-pop-eshell ()
  "Eshell popup straight to insert mode."
  (interactive)
  (spacemacs/shell-pop-eshell nil)
  (if (string= major-mode "eshell-mode")
      (evil-insert 1)
    (evil-escape)))

(evil-global-set-keys
 '(normal insert)
 (kbd "C-e") 'eshell-pop-eshell)
