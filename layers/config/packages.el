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

        ;; Special Packages
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
    :config
    ;; The `undo-spacemacs-bindings' is performed in `init.el' in
    ;; user-config section: `dotspacemacs/user-config/post-layer-load-config'
    (progn
      (setq undo-spacemacs-prefixes-list
            '("C"   ; capture/colors
              "a s"  ; shells
              "b N"  ; new buffers
              "f v"  ; file/dir-local-variables
              "f C"  ; files/convert
              ))
      (setq undo-spacemacs-bindings-alist
            '(;; Top-level
              ("!" shell-command)
              ("'" spacemacs/default-pop-shell)
              ("0" neotree-show)
              ("?" counsel-descbinds)
              ("`" winum-select-window-by-number)

              ;; A - applications
              ("ad" deer)
              ("ak" spacemacs/paradox-list-packages)

              ;; B - buffers
              ("b." spacemacs/buffer-transient-state/body)
              ("bB" spacemacs-layouts/non-restricted-buffer-list-ivy)
              ("bD" spacemacs/ace-kill-this-buffer)
              ("bh" spacemacs/home
               spacemacs/switch-to-help-buffer)
              ("bH" spacemacs/switch-to-help-buffer)
              ("be" spacemacs/safe-erase-buffer)
              ("bb" ivy-switch-buffer
               ibuffer)
              ("bI" ibuffer)
              ("bn" next-buffer)
              ("bp" previous-buffer)
              ("bP" spacemacs/copy-clipboard-to-whole-buffer)
              ("bR" spacemacs/safe-revert-buffer)
              ("bw" read-only-mode)
              ("bW" spacemacs/goto-buffer-workspace)
              ("bY" spacemacs/copy-whole-buffer-to-clipboard)
              ("b C-d"   spacemacs/kill-other-buffers)
              ("b C-S-d" spacemacs/kill-matching-buffers-rudely)

              ;; C - capture/colors
              ;; ("Cc" org-capture)

              ;; c - compile/comments
              ("cl" spacemacs/comment-or-uncomment-lines)
              ("cL" spacemacs/comment-or-uncomment-lines-inverse)
              ("cP" spacemacs/comment-or-uncomment-paragraphs-inverse)
              ("cT" spacemacs/quick-comment-or-uncomment-to-the-line-inverse)
              ("cY" spacemacs/copy-and-comment-lines-inverse)

              ;; e - errors
              ;; ... Don't use checkers much ...

              ;; F - frames
              ("Fb" spacemacs/switch-to-buffer-other-frame)
              ("FB" spacemacs/display-buffer-other-frame)
              ("FD" delete-other-frames)
              ("Ff" spacemacs/find-file-other-frame)
              ("Fn" make-frame)
              ("FO" spacemacs/dired-other-frame)

              ;; f - files
              ("fA" spacemacs/find-file-and-replace-buffer)
              ("fb" counsel-bookmark)
              ("fE" spacemacs/sudo-edit)
              ("fg" rgrep)
              ("fh" hexl-find-file)
              ("fi" spacemacs/insert-file)
              ("fJ" spacemacs/open-junk-file)
              ("fj" dired-jump)
              ("fl" find-file-literally)
              ("fL" counsel-locate)

              ;; g - git/version-control

              ;; h - help

              ;; i - insertion

              ;; j - jump/join/split

              ;; k - lisp

              ;; N - navigation

              ;; n - narrow/numbers

              ;; p - projects

              ;; q - quit

              ;; r - registers/rings/resume

              ;; s - search/symbol

              ;; T - UI toggles/themes

              ;; t - toggles

              ;; w - windows

              ;; x - text

              ;; z - zoom
              )))))
