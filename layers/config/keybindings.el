;; Package agnostic keybindings

;; Windows commands
(global-set-key
 (kbd "M-d") 'spacemacs/delete-window)
(evil-define-key 'normal outline-minor-mode-map
  (kbd "C-M-i") 'spacemacs/alternate-buffer)  ; M-tab

;; Outline navigation
(evil-define-key '(normal visual motion) outline-minor-mode-map
  "gh" 'outline-up-heading
  "gj" 'outline-forward-same-level
  "gk" 'outline-backward-same-level
  "gl" 'outline-next-visible-heading
  "gu" 'outline-previous-visible-heading)

(spacemacs/set-leader-keys
  "nj" 'outline-move-subtree-down
  "nk" 'outline-move-subtree-up
  "nh" 'outline-promote
  "nl" 'outline-demote)
