;;; packages.el --- org-python layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eric Kaschalk
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Org-py:
(defconst org-python-packages
  '(
    (org-py-includes :location local)
    ;; (org-py-tangle :location local)  ; Replaced with outshine
    ))

(defun org-python/init-org-py-includes ()
  (use-package org-py-includes
    ;; :defer t
    :init
    (progn
      (add-hook 'before-save-hook #'update-python-includes)
      (add-hook 'before-save-hook #'python-include-docstrings)
      ;; (add-to-list 'org-ctrl-c-ctrl-c-hook 'org-babel-async-execute:python)

      ;; (add-hook 'after-save-hook 'tangle-on-save-org-mode-file)
      ;; (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
      ;;   (kbd "RET") 'ek/tangle-in-src-edit)
      ;; (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
      ;;   (kbd "t") 'ek/test-in-src-edit)
    )))

;; Might be able to use this for dash-functional
;; (some-package :location (recipe
;;                          :fetcher github
;;                          :repo "some/repo"))

;; Guard against say org/python not being loaded with:
;; (when (configuration-layer/package-usedp 'my-package)
;;   (defun spacemacs/my-package-enable () ...)
;;   (defun spacemacs/my-package-disable () ...))

;; layers.el -> packages.el -> funcs.el -> config.el -> keybindings.el
;; keybindings must be safe regardless of installed packages.

;;; packages.el ends here
