;;; Config Layer

(setq config-packages
      '(;; Owned packages
        outshine

        ;; Elsewhere-owned packages with trivial config
        aggressive-indent
        yasnippet

        ;; Elsehwere-owned packages
        (avy-config    :location local)
        (eshell-config :location local)
        (evil-config   :location local)
        (gnus-config   :location local)
        (ivy-config    :location local)
        (org-config    :location local)

        ;; Elsewhere-owned packages for languages
        (clojure-config :location local)
        (python-config  :location local)

        ;; Special Package
        (undo-spacemacs :location local)))

;;; Owned Packages
;;;; Outshine

(defun config/init-outshine ()

  (defun advise-outshine-narrow-start-pos ()
    "Narrowing works within the headline rather than requiring to be on it."
    (unless (outline-on-heading-p t)
      (outline-previous-visible-heading 1)))

  (use-package outshine
    :after macros

    :init (progn
            ;; Narrowing global keybindings
            (spacemacs/set-leader-keys
              "nn" 'outshine-narrow-to-subtree
              "nw" 'widen)

            ;; Make <backtab> globally cycle like in org-mode buffers
            (define-keys outline-minor-mode-map
              (kbd "<backtab>") 'outshine-cycle-buffer))

    :config (progn
              ;; So *all* prog-modes have `outline-minor-mode' enabled
              (add-hook 'prog-mode-hook 'outline-minor-mode)

              ;; So *all* prog-modes have `outshine-mode' enabled
              (add-hook 'outline-minor-mode-hook 'outshine-hook-function)

              ;; Fix outshine narrowing
              (advice-add 'outshine-narrow-to-subtree :before
                          'advise-outshine-narrow-start-pos))))

;;; Unowned Packages
;;;; Yasnippet

(defun config/pre-init-yasnippet ()
  (global-set-key (kbd "C-SPC") 'hippie-expand))

;;;; Aggressive Indent

(defun config/post-init-aggressive-indent ()
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook    #'aggressive-indent-mode)
  (add-hook 'hy-mode-hook         #'aggressive-indent-mode))

;;; Local Packages

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

;;; Local Language Packages

(defun config/init-clojure-config ()
  (use-package clojure-config
    :after clojure-mode))

(defun config/init-python-config ()
  (use-package python-config
    :after python))

;;; Special Packages

(defun config/init-undo-spacemacs ()
  (use-package undo-spacemacs
    :after macros
    :config (setq undo-spacemacs-bindings-alist
                  '(;; A - applications
                    ("ad" deer)
                    ("ak" spacemacs/paradox-list-packages)

                    ;; C - capture/colors
                    ("Cc" org-capture)
                    ))))
