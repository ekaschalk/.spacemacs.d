;; Quick macro execution
(evil-global-set-key 'normal (kbd "Q") 'config/execute-q-macro)

;; Eshell popup
(evil-global-set-key 'normal (kbd "C-e") 'config/shell-pop-eshell)
(evil-global-set-key 'insert (kbd "C-e") 'config/shell-pop-eshell)

;; Windows commands
(global-set-key (kbd "M-d") 'spacemacs/delete-window)
(evil-define-key 'normal outline-minor-mode-map (kbd "C-M-i")  ; M-tab
  'spacemacs/alternate-buffer)
