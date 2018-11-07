(require 'macros)
(require 'ranger)

(provide 'ranger-config)

;;; ranger-config

(setq ranger-deer-show-details nil)

(evil-global-set-key 'normal "_" 'ranger)

(bind-keys :map ranger-mode-map
           ("n"   . dired-create-directory)
           ("E"   . wdired-change-to-wdired-mode)
           ("C-j" . ranger-travel)
           ("C-e" . ranger-pop-eshell))
