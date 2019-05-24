;;; Config Layer -*- lexical-binding: t; -*-

(setq config-packages
      '(;; Unowned Packages
        aggressive-indent
        avy
        eshell
        evil
        ivy
        magit
        ob org org-bullets
        ranger

        ;; Owned Packages
        auto-dim-other-buffers
        dash-functional
        faceup
        outshine  ; also configures `outline-mode'
        s

        ;; Local Packages
        (redo-spacemacs :location local)))

;;; Unowned Packages
;;;; Aggressive indent

(defun config/pre-init-aggressive-indent ()
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook    #'aggressive-indent-mode)
  (add-hook 'hy-mode-hook         #'aggressive-indent-mode))

;;;; Avy

(defun config/pre-init-avy ()
  (setq avy-timeout-seconds 0.35)

  (evil-global-set-key 'normal "s" 'avy-goto-char-timer)
  (bind-keys ("C-l" . evil-avy-goto-line)
             ("C-h" . avy-pop-mark)))

;;;; Eshell

(defun config/pre-init-eshell ()
  (spacemacs|use-package-add-hook eshell
    :post-init
    (evil-define-key '(normal insert) 'global (kbd "C-e") 'eshell-pop-eshell)))

;;;; Evil

(defun config/post-init-evil ()
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence "true")

  (evil-global-set-key 'normal "Q" 'evil-execute-q-macro)
  (evil-define-key '(normal visual motion) 'global
    "H" 'evil-first-non-blank
    "L" 'evil-end-of-line-interactive
    "0" 'evil-jump-item)

  (advice-add 'evil-ex-search-next     :after 'evil-scroll-to-center-advice)
  (advice-add 'evil-ex-search-previous :after 'evil-scroll-to-center-advice))

;;;; Ivy

(defun config/pre-init-ivy ()
  (setq ivy-format-function 'ivy-format-function-arrow)
  (setq completion-in-region-function 'ivy-completion-in-region))

(defun config/post-init-ivy ()
  (setq ivy-height 20)

  (spacemacs/set-leader-keys "ai" 'ivy-resume)

  (bind-keys :map ivy-minibuffer-map
             ("C-l"        . ivy-avy)
             ("C-u"        . ivy-scroll-down-command)
             ("C-d"        . ivy-scroll-up-command)
             ("C-n"        . ivy-restrict-to-matches)
             ("C-y"        . ivy-yank-word)
             ("C-<return>" . ivy-call)
             ("C-SPC"      . ivy-dispatching-done)
             ("C-S-SPC"    . ivy-dispatching-call)))

;;;; Magit

(defun config/post-init-magit ()
  (bind-keys :map magit-mode-map
             ("M-1" . winum-select-window-1)
             ("M-2" . winum-select-window-2)
             ("M-3" . winum-select-window-3)
             ("M-4" . winum-select-window-4)))

;;;; Org

(defun config/pre-init-org-bullets ()
  (setq org-bullets-bullet-list '("" "" "" "")))

(defun config/pre-init-ob ()
  (setq org-confirm-babel-evaluate   nil)
  (setq org-src-fontify-natively     t)
  (setq org-src-tab-acts-natively    t)
  (setq org-src-preserve-indentation t)
  (setq org-src-window-setup         'current-window)

  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(dot . t))))

(defun config/pre-init-org ()
  (setq org-ellipsis "")
  (setq org-priority-faces
        '((65 :inherit org-priority :foreground "red")
          (66 :inherit org-priority :foreground "brown")
          (67 :inherit org-priority :foreground "blue")))
  (setq org-structure-template-alist
        '(("n" "#+NAME: ?")
          ("L" "#+LaTeX: ")
          ("h" "#+HTML: ")
          ("q" "#+BEGIN_QUOTE\n\n#+END_QUOTE")
          ("s" "#+BEGIN_SRC ?\n\n#+END_SRC")
          ("se" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC")
          ("sp" "#+BEGIN_SRC python\n\n#+END_SRC")))

  (add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; Experimenting with the following indentation vars:
  (setq org-startup-indented nil)
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers nil)
  (setq org-indent-indentation-per-level 1))

(defun config/post-init-org ()
  (evil-define-key '(normal visual motion) org-mode-map
    "gh" 'outline-up-heading
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gl" 'outline-next-visible-heading
    "gu" 'outline-previous-visible-heading)

  (spacemacs/set-leader-keys "aof" 'org-open-at-point-global)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "r" 'org-refile
    "h" 'org-metaleft  ; Because of MacOS's damned, indestructable M-h binding...
    "s p" 'org-sort-entries-priorities))

;;;; Ranger

(defun config/pre-init-ranger ()
  (setq ranger-deer-show-details nil)

  (evil-global-set-key 'normal "_" 'ranger)

  ;; To get around `ranger/post-init-dired' overwriting keybindings
  (spacemacs|use-package-add-hook ranger
    :post-config
    (bind-keys :map ranger-mode-map
               ("n"   . dired-create-directory)
               ("E"   . wdired-change-to-wdired-mode)
               ("C-j" . ranger-travel)
               ("C-e" . ranger-pop-eshell)
               ("M-1" . winum-select-window-1)
               ("M-2" . winum-select-window-2)
               ("M-3" . winum-select-window-3)
               ("M-4" . winum-select-window-4)
               ("M-5" . winum-select-window-5))))

;;; Owned Packages
;;;; Auto Dim Other Buffers

(defun config/init-auto-dim-other-buffers ()
  (use-package auto-dim-other-buffers
    :config
    (auto-dim-other-buffers-mode)))

;;;; Dash functional

(defun config/init-dash-functional ()
  ;; The spacemacs core file `core-documentation' requires dash.
  ;; So we only have to use-pkg dash-functional to have all of dash around.
  (use-package dash-functional))

;;;; Faceup

(defun config/init-faceup ()
  (use-package faceup
    :defer t))

;;;; Outshine

(defun config/init-outshine ()
  (use-package outshine
    :hook ((prog-mode          . outline-minor-mode)
           (outline-minor-mode . outshine-mode))

    :bind (("<backtab>"     . outshine-cycle-buffer)
           ([(meta return)]       . outshine-insert-heading)
           ([(meta shift return)] . outshine-insert-subheading)
           :map outline-minor-mode-map)

    :init
    (progn
      (evil-define-key '(normal visual motion) outline-minor-mode-map
        "gh" 'outline-up-heading
        "gj" 'outline-forward-same-level
        "gk" 'outline-backward-same-level
        "gl" 'outline-next-visible-heading
        "gu" 'outline-previous-visible-heading)

      (spacemacs/set-leader-keys
        "nn" 'outshine-narrow-to-subtree
        "nw" 'widen
        "nj" 'outline-move-subtree-down
        "nk" 'outline-move-subtree-up
        "nh" 'outline-promote
        "nl" 'outline-demote)

      (advice-add 'outshine-narrow-to-subtree :before 'outshine-fix-narrow-pos)

      (advice-add 'outshine-insert-heading    :before 'outshine-fix-insert-pos)
      (advice-add 'outshine-insert-heading    :after 'evil-insert-advice)
      (advice-add 'outshine-insert-subheading :after 'evil-insert-advice)

      ;; Fix the new bindings in outline-minor-mode overwriting org-mode-map
      ;; I also add advice here because it mirrors outshine modifications
      (spacemacs|use-package-add-hook org
        :post-config
        (progn
          (bind-keys :map org-mode-map
                     ([(meta return)]       . org-meta-return)
                     ([(meta shift return)] . org-insert-subheading))
          (advice-add 'org-insert-heading    :before 'org-fix-heading-pos)
          (advice-add 'org-insert-heading    :after 'evil-insert-advice)
          (advice-add 'org-insert-subheading :after 'evil-insert-advice))))))

;;;; Strings

(defun config/init-s ()
  (use-package s))

;;; Local Packages
;;;; Redo-spacemacs

;; `redo-spacemacs-bindings' is executed in user-config in `init.el'
;; with the `dotspacemacs/user-config/post-layer-load-config' function

;; If any removed bindings make you scratch your head, check out
;; the ending `redo-spacemacs-new-bindings-alist' to see what I rebound it
;; to (for example, `spacemacs/delete-window' from 'SPC w d' to 'M-d')
;; They are unbound to force muscle-memory development.

(defun config/init-redo-spacemacs ()
  (use-package redo-spacemacs
    :if (and (boundp 'redo-bindings?) redo-bindings?)
    :init
    (progn
      (setq redo-spacemacs-prefixes-list
            '(;; Primary prefixes
              "C"    ; capture/colors
              "i"    ; insertion
              "j"    ; jump/join/split
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

      (setq redo-spacemacs-undo-bindings-alist
            '(;; Top-level
              ("!" shell-command)
              ("'" spacemacs/default-pop-shell)
              ("0" neotree-show)
              ("?" counsel-descbinds)
              ("`" winum-select-window-by-number)
              ("1" winum-select-window-1)
              ("2" winum-select-window-2)
              ("3" winum-select-window-3)
              ("4" winum-select-window-4)
              ("5" winum-select-window-5)
              ("6" winum-select-window-6)
              ("7" winum-select-window-7)
              ("8" winum-select-window-8)
              ("9" winum-select-window-9)

              ;; A - applications
              ("ad" deer)
              ("ar" ranger)

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
              ("wC" spacemacs/toggle-distraction-free)
              ("wc" spacemacs/toggle-centered-buffer)
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
              ("w <down>"    evil-window-down)
              ("w <up>"      evil-window-up)
              ("w <left>"    evil-window-left)
              ("w <right>"   evil-window-right)
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

              ;; Important bindings that I use chords for now.
              ;; They are removed to force muscle-memory.
              ("v" er/expand-region)
              ("wm" spacemacs/toggle-maximize-buffer)
              ("wd" spacemacs/delete-window)
              ("w/" split-window-right)
              ("w-" split-window-below)
              ("ff" counsel-find-file)
              ("fr" counsel-recentf)
              ))

      (setq redo-spacemacs-new-bindings-alist
            '(;; Windows, Layouts Management
              ("M-w"   spacemacs/toggle-maximize-buffer)
              ("M-d"   spacemacs/delete-window)
              ("M-c"   spacemacs/toggle-centered-buffer-mode)
              ("M-/"   split-window-right)
              ("C-M-/" split-window-right-and-focus)
              ("M--"   split-window-below)
              ("C-M--" split-window-below-and-focus)
              ("M-1" winum-select-window-1)
              ("M-2" winum-select-window-2)
              ("M-3" winum-select-window-3)
              ("M-4" winum-select-window-4)
              ("M-5" winum-select-window-5)

              ;; Editing, Searching, Movement
              ("C-,"   lisp-state-toggle-lisp-state)
              ("C-SPC" er/expand-region)
              ("C-S-s" spacemacs/swiper-region-or-symbol)

              ;; Files, Buffers
              ("M-f" counsel-find-file)
              ("M-r" counsel-recentf)

              ;; Rebindings to look at
              ;; spacemacs/kill-this-buffer
              ;; M-u, M-i
              )))))
