;;; packages.el --- Display Layer
;;
;; Copyright (c) 2017 Eric Kaschalk
;;
;; Author: Eric Kaschalk <ekaschalk@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq display-packages
      '(
        ;; neotree
        spaceline-all-the-icons
        (pretty-eshell :location local)
        (pretty-magit :location local)
        (pretty-outlines :location local)
        ))

;;; Pretty-eshell

(defun display/init-pretty-eshell ()
  (use-package pretty-eshell
    :config
    (progn
      (esh-section esh-dir
                   "\xf07c"  ; 
                   (abbreviate-file-name (eshell/pwd))
                   '(:foreground "gold" :bold ultra-bold :underline t))
      (esh-section esh-git
                   "\xe907"  ; 
                   (magit-get-current-branch)
                   '(:foreground "pink"))
      (esh-section esh-python
                   "\xe928"  ; 
                   pyvenv-virtual-env-name)
      (esh-section esh-clock
                   "\xf017"  ; 
                   (format-time-string "%H:%M" (current-time))
                   '(:foreground "forest green"))
      (esh-section esh-num
                   "\xf0c9"  ; 
                   (number-to-string esh-prompt-num)
                   '(:foreground "brown"))
      (setq eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num)))))

;;; Pretty-magit

(defun display/init-pretty-magit ()
  (use-package pretty-magit
    :config
    (progn
      (pretty-magit "Feature" ? (:foreground "slate gray" :height 1.2))
      (pretty-magit "Add"     ? (:foreground "#375E97" :height 1.2))
      (pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.2))
      (pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.2))
      (pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.2))
      (pretty-magit "master"  ? (:box t :height 1.2) t)
      (pretty-magit "origin"  ? (:box t :height 1.2) t))))

(defun display/post-init-pretty-magit ()
  (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
  (add-hook 'git-commit-setup-hook 'magit-commit-prompt)

  (advice-add 'magit-status :after 'add-magit-faces)
  (advice-add 'magit-refresh-buffer :after 'add-magit-faces)
  (advice-add 'magit-commit :after 'use-magit-commit-prompt))

;;; Pretty-outlines

(defun display/init-pretty-outlines ()
  (use-package pretty-outlines))

(defun display/post-init-pretty-outlines ()
  ;; Outline-ellipsis
  (add-hook 'outline-mode-hook 'pretty-outline-set-display-table)
  (add-hook 'outline-minor-mode-hook 'pretty-outline-set-display-table)

  ;; Outline-bullets
  (add-hook 'emacs-lisp-mode-hook 'pretty-outline-add-bullets)
  ;; (add-hook 'hy-mode-hook 'pretty-outline-add-bullets)
  ;; (add-hook 'python-mode-hook 'pretty-outline-add-bullets)
  )

;;; Modeline

(defun display/init-spaceline-all-the-icons ()
  (use-package spaceline-all-the-icons
    :after spaceline
    :config
    (progn
      (setq spaceline-highlight-face-func 'spaceline-highlight-face-default
            spaceline-all-the-icons-icon-set-modified 'chain
            spaceline-all-the-icons-icon-set-window-numbering 'circle
            spaceline-all-the-icons-separator-type 'none
            spaceline-all-the-icons-primary-separator "")

      (spaceline-toggle-all-the-icons-buffer-size-off)
      (spaceline-toggle-all-the-icons-buffer-position-off)
      (spaceline-toggle-all-the-icons-vc-icon-off)
      (spaceline-toggle-all-the-icons-vc-status-off)
      (spaceline-toggle-all-the-icons-git-status-off)
      (spaceline-toggle-all-the-icons-flycheck-status-off)
      (spaceline-toggle-all-the-icons-time-off)
      (spaceline-toggle-all-the-icons-battery-status-off)
      (spaceline-toggle-hud-off))))

(defun display/post-init-spaceline-all-the-icons ()
  (spaceline-all-the-icons-theme)
  (when (configuration-layer/package-usedp 'neotree)
    (spaceline-all-the-icons--setup-neotree)))

;;; Neotree

;; (defun display/post-init-neotree ()
;;   (when (configuration-layer/package-usedp 'neotree)
;;     (spaceline-all-the-icons--setup-neotree)))


;;; Notes

;; OUTSIDE PACKAGES
;; all-the-icons (check individual fonts in set-icon-fonts)
;; (check fira code installed for fira-font-lock-alist)
;; theming layer (to replace custom-set-packages)
;; spaceline-all-the-icons
;; org/org-bullets
;; (prettify-utils-generate maybe)
;; pretty-mode
;; dash/s

;; PERSONAL PACKAGES
;; font-lock-stuff
;; outline-updates
;; shell-updates
;; pretty-magit
;; prettify-symbols

;; NOTES
;; use post-init hooks to add language-specific font-locks
