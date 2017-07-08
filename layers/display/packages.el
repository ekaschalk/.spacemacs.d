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
        (pretty-magit :location local)
        ))

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
