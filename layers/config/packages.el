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
            '(;; Primary prefixes
              "C"    ; capture/colors
              "i"    ; insertion
              "j"    ; jump/join/split
              "k"    ; lisp-state (access via `lisp-state-toggle-lisp-state')
              "N"    ; navigation
              "r"    ; registers/rings/resume
              "t"    ; toggles
              "z"    ; zoom

              ;; Sub prefixes
              "a s"  ; shells
              "b N"  ; new buffers
              "f v"  ; file/dir-local-variables
              "f C"  ; files/convert
              "p $"  ; projects/shell
              "s k"  ; search/ack
              "s r"  ; search/ripgrep
              "s t"  ; search/pt
              "w p"  ; windows/popup
              "x d"  ; text/delete
              "x g"  ; text/google-translate
              "x j"  ; text/justification
              "x t"  ; text/transposition
              "x w"  ; text/words
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

              ;; c - compile/comments
              ("cl" spacemacs/comment-or-uncomment-lines)
              ("cL" spacemacs/comment-or-uncomment-lines-inverse)
              ("cP" spacemacs/comment-or-uncomment-paragraphs-inverse)
              ("cT" spacemacs/quick-comment-or-uncomment-to-the-line-inverse)
              ("cY" spacemacs/copy-and-comment-lines-inverse)

              ;; e - errors
              ;; ... Haven't went through yet ...

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
              ;; ... Haven't went through yet ...

              ;; h - help
              ;; ... Haven't went through yet ...

              ;; i - insertion
              ;; Removed entire leader

              ;; j - jump/join/split
              ;; Removed entire leader

              ;; k - lisp
              ;; Removed entire leader (I always use transient state for these)

              ;; N - navigation
              ;; Removed entire leader

              ;; n - narrow/numbers
              ("n+" spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt)
              ("np" narrow-to-page)
              ("nr" narrow-to-region)

              ;; p - projects
              ("p%" projectile-replace-regexp)
              ("pe" projectile-edit-dir-locals)
              ("pF" projectile-find-file-dwim)
              ("pR" projectile-replace)
              ("pT" projectile-test-project)
              ("pv" projectile-vc)

              ;; q - quit
              ("qs" spacemacs/save-buffers-kill-emacs)
              ("qt" spacemacs/restart-emacs-adv-timers)

              ;; r - registers/rings/resume
              ;; Removed entire leader

              ;; s - search/symbol
              ("sf" spacemacs/search-auto)
              ("sF" spacemacs/search-auto-region-or-symbol)
              ("sh" spacemacs/symbol-highlight)
              ("sH" spacemacs/goto-last-searched-ahs-symbol)
              ("sj" spacemacs/counsel-jump-in-buffer)
              ("sp" spacemacs/search-project-auto)
              ("sP" spacemacs/search-project-auto-region-or-symbol)
              ("ss" swiper)
              ("sS" spacemacs/swiper-region-or-symbol)

              ;; T - UI toggles/themes
              ;; Leaving unchanged

              ;; t - toggles
              ;; Removed entire leader

              ;; w - windows
              ("w+" spacemacs/window-layout-toggle)
              ("w1" spacemacs/window-split-single-column)
              ("w2" spacemacs/window-split-double-columns)
              ("w3" spacemacs/window-split-triple-columns)
              ("w_" spacemacs/maximize-horizontally)
              ("wC" spacemacs/toggle-centered-buffer-mode-frame)
              ("wF" make-frame)
              ("wh" evil-window-left)
              ("wj" evil-window-down)
              ("wk" evil-window-up)
              ("wl" evil-window-right)
              ("ws" split-window-below)
              ("wS" split-window-below-and-focus)
              ("wv" split-window-right)
              ("wV" split-window-right-and-focus)
              ("ww" other-window
               ace-window)
              ("wx" kill-buffer-and-window)
              ("wW" ace-window)
              ("w|" spacemacs/maximize-vertically)
              ("w <down>"  evil-window-down)
              ("w <up>"    evil-window-up)
              ("w <left>"  evil-window-left)
              ("w <right>" evil-window-right)
              ("w <S-down>"  evil-window-move-very-bottom)
              ("w <S-up>"    evil-window-move-very-top)
              ("w <S-left>"  evil-window-move-far-left)
              ("w <S-right>" evil-window-move-far-right)

              ;; x - text
              ("x TAB" indent-rigidly)
              ("xJ" spacemacs/move-text-transient-state/move-text-down)
              ("xK" spacemacs/move-text-transient-state/move-text-up)
              ("xo" link-hint-open-link)
              ("xO" link-hint-open-multiple-links)

              ;; z - zoom
              ;; Removed entire leader
              )))))
