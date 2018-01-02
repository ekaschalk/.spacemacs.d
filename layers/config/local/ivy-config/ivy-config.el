(require 'ivy)
(require 'macros)

(provide 'ivy-config)


(setq ivy-format-function
      'ivy-format-function-arrow)
(setq ivy-height
      20)
(setq completion-in-region-function
      'ivy-completion-in-region)

;; Resume last ivy session
(spacemacs/set-leader-keys
  "ai" 'ivy-resume)

(define-keys
  ivy-minibuffer-map

  ;; Perform default action on avy-selected minibuffer line
  (kbd "C-l") 'ivy-avy

  ;; Evil-like scrolling of ivy minibuffer
  (kbd "C-u") 'ivy-scroll-down-command
  (kbd "C-d") 'ivy-scroll-up-command

  ;; Rebind C-n/C-y/C-p to narrow/yank from buffer/paste into buffer
  (kbd "C-n") 'ivy-restrict-to-matches
  (kbd "C-y") 'ivy-yank-word

  ;; Read-only buffer of candidates with shortcuts to dispatches
  (kbd "C-o") 'ivy-occur

  ;; Non-exiting default action
  (kbd "C-<return>") 'ivy-call

  ;; Dispatch actions
  (kbd "C-SPC") 'ivy-dispatching-done
  (kbd "C-S-SPC") 'ivy-dispatching-call)
