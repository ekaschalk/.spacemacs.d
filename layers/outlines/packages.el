;;; Outlines Layer

(setq outlines-packages
      '(
        ;; Core Outlines Packages
        outshine

        ;; Locacl Packages
        ;; (outline-ivy :location local)
        ))

;;; Outshine

(defun outlines/init-outshine ()
  (use-package outshine
    :init
    (progn
      (spacemacs/set-leader-keys
        "nn" 'outshine-narrow-to-subtree
        "nw" 'widen)

      (let ((kmap outline-minor-mode-map))
        (define-key kmap (kbd "M-RET") 'outshine-insert-heading)
        (define-key kmap (kbd "<backtab>") 'outshine-cycle-buffer)))

    :config
    (progn
      ;; Narrowing works within the headline rather than requiring to be on it
      (advice-add 'outshine-narrow-to-subtree :before
                  (lambda (&rest args) (unless (outline-on-heading-p t)
                                    (outline-previous-visible-heading 1))))

      (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
      (add-hook 'prog-mode-hook 'outline-minor-mode))))

;;; Outline-ivy

;; (defun outlines/init-outline-ivy ()
;;   (use-package outline-ivy
;;     :config
;;     (global-set-key (kbd "C-j") 'oi-jump)))
