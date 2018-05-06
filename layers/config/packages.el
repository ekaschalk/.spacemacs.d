;;; Config Layer

(setq config-packages
      '(
        ;; Owned packages
        outshine

        ;; Elsewhere-owned packages with trivial config
        ispell
        projectile
        yasnippet

        ;; Elsehwere-owned packages
        (avy-config      :location local)
        (eshell-config   :location local)
        (evil-config     :location local)
        (gnus-config     :location local)
        (ivy-config      :location local)
        (org-config      :location local)))

;;; Minor Config

(defun config/post-init-ispell ()
  (setq ispell-program-name
        "aspell"))

(defun config/post-init-projectile ()
  (setq projectile-indexing-method
        'native))

(defun config/pre-init-yasnippet ()
  (global-set-key (kbd "C-SPC") 'hippie-expand))

;;; Local Config

(defun config/init-avy-config ()
  (use-package avy-config
    :after macros))

(defun config/init-eshell-config ()
  (use-package eshell-config
    :after evil macros))

(defun config/init-evil-config ()
  (use-package evil-config
    :after evil macros))

(defun config/init-gnus-config ()
  (use-package gnus-config
    :after gnus))

(defun config/init-ivy-config ()
  (use-package ivy-config
    :after ivy macros))

(defun config/init-org-config ()
  (use-package org-config
    :after org macros))

;;; Outshine

(defun config/init-outshine ()
  (defun advise-outshine-narrow-start-pos ()
    (unless (outline-on-heading-p t)
      (outline-previous-visible-heading 1)))

  (use-package outshine
    :after macros
    :init
    (progn
      (spacemacs/set-leader-keys
        "nn" 'outshine-narrow-to-subtree
        "nw" 'widen)
      (define-keys outline-minor-mode-map
        (kbd "M-RET") 'outshine-insert-heading
        (kbd "<backtab>") 'outshine-cycle-buffer))

    :config
    (progn
      ;; Narrowing works within the headline rather than requiring to be on it
      (advice-add 'outshine-narrow-to-subtree :before
                  'advise-outshine-narrow-start-pos)

      (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
      (add-hook 'prog-mode-hook 'outline-minor-mode))))
