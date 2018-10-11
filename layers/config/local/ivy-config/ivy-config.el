(require 'ivy)
(require 'macros)

(provide 'ivy-config)


;;; Variables

(setq ivy-format-function 'ivy-format-function-arrow)
(setq ivy-height 20)
(setq completion-in-region-function 'ivy-completion-in-region)

;;; Bindings

(spacemacs/set-leader-keys "ai" 'ivy-resume)

(define-keys
  ivy-minibuffer-map

  ;; Select prompt with avy line motion
  (kbd "C-l") 'ivy-avy

  ;; Evil-like scrolling of ivy minibuffer
  (kbd "C-u") 'ivy-scroll-down-command
  (kbd "C-d") 'ivy-scroll-up-command

  ;; Narrowing prompt and yanking into prompt
  (kbd "C-n") 'ivy-restrict-to-matches
  (kbd "C-y") 'ivy-yank-word

  ;; Call default action without exiting prompt
  (kbd "C-<return>") 'ivy-call

  ;; Dispatch actions
  (kbd "C-SPC") 'ivy-dispatching-done
  (kbd "C-S-SPC") 'ivy-dispatching-call)
