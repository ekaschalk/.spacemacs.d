;;; -*- lexical-binding: t -*-

;; Display mofidications for `solarized-light' and `zenburn' applied here.

;; Try out changes with `spacemacs/update-theme' to see theme updates
;; or alternatively run `spacemacs/cycle-spacemacs-theme' with 'SPC T n'.

;; Theming updates are structured and modularized where possible.

;; Changes of note:
;; 1. All outline/org-level heading styling
;; 2. Comments/strings are italicized
;; 3. Transparent active and monochrome inactive modelines
;; 4. Various small gradient changes to core font-lock-faces

;;; Configuration
;;;; Core

(setq solarized-use-variable-pitch nil)
(setq face-remapping-alist '(;; Headers - outlines match org
                             (outline-1 org-level-1)
                             (outline-2 org-level-2)
                             (outline-3 org-level-3)

                             ;; Modeline - invis. active, monochrome inactive
                             (powerline-active1        mode-line)
                             (powerline-active2        mode-line)
                             (spaceline-highlight-face mode-line)

                             (powerline-active0        mode-line)
                             (mode-line-active         mode-line)
                             (mode-line-inactive       mode-line)
                             (powerline-inactive0      mode-line)
                             (powerline-inactive1      mode-line)
                             (powerline-inactive2      mode-line)
                             ))

;;;; Styling
;;;;; Headers

(setq display/headers/common '(:underline t :inherit nil))
(setq display/headers/zenburn
      `((org-level-1
         ,@display/headers/common
         :height 1.35
         :foreground "#DFAF8F")
        (org-level-2
         ,@display/headers/common
         :height 1.25
         :foreground "#BFEBBF")
        (org-level-3
         ,@display/headers/common
         :height 1.15
         :foreground "#7CB8BB")))
(setq display/headers/solarized-light
      `((org-level-1
         ,@display/headers/common
         :height 1.35
         :foreground "#a71d31")
        (org-level-2
         ,@display/headers/common
         :height 1.25
         :foreground "#8D6B94")
        (org-level-3
         ,@display/headers/common
         :height 1.15)))

;;;;; Org-blocks

(setq display/org-blocks/common '(:italic nil :underline nil :box t))
(setq display/org-blocks
      `((org-block-begin-line
         ,@display/org-blocks/common)
        (org-block-end-line
         ,@display/org-blocks/common)))

;;;;; Company

(setq display/company/common '(:weight bold :underline nil))
(setq display/company
      `((company-tooltip-common
         ,@display/company/common
         :inherit company-tooltip)
        (company-tooltip-common-selection
         ,@display/company/common
         :inherit company-tooltip-selection)))

;;;;; Mode-line

(setq display/mode-line/common '(:box nil :underline nil))
(setq display/mode-line
      `((mode-line
         ,@display/mode-line/common
         :background nil)
        (mode-line-inactive
         ,@display/mode-line/common)))

;;;;; Font-locks

(setq display/font-locks
      `((font-lock-comment-face
         :italic t
         :weight normal)
        (font-lock-doc-face
         :italic t
         :weight normal)))

;;; Theming
;;;; Common

(setq display/common-theming
      `(,@display/company
        ,@display/mode-line
        ,@display/org-blocks

        (avy-background-face :italic nil)
        (fringe :background nil)))

;;;; Themes

(setq display/solarized-light-theming
      `(;; Overwrites
        (mode-line-inactive :background "#eee8d5"
                            ,@(alist-get 'mode-line-inactive
                                         display/mode-line))

        (font-lock-comment-face :foreground "#586e75"
                                ,@(alist-get 'font-lock-comment-face
                                             display/font-locks))
        (font-lock-doc-face :foreground "#2aa198"
                            ,@(alist-get 'font-lock-doc-face
                                         display/font-locks))

        ;; Extra
        (sp-show-pair-match-face :background  "CadetBlue3")
        (auto-dim-other-buffers-face :background "#fcf4df")

        ;; ... Experiments ...
        ))

(setq display/zenburn-theming
      `(;; Overwrites
        (font-lock-comment-face :foreground "gray50"
                                ,@(alist-get 'font-lock-comment-face
                                             display/font-locks))
        (font-lock-doc-face :foreground "gray65"
                            ,@(alist-get 'font-lock-doc-face
                                         display/font-locks))

        ;; Extra
        (font-lock-comment-delimiter-face :foreground "gray35")
        (font-lock-function-name-face     :foreground "CadetBlue2")
        (font-lock-type-face              :foreground "LightCoral")
        (auto-dim-other-buffers-face      :background "gray22")

        ;; ... Experiments ...
        ))

;;;; Set Modifications

;; This variable is the only `theming' layer requirement to enable our theming

(setq theming-modifications
      `((zenburn         ,@display/common-theming
                         ,@display/headers/zenburn
                         ,@display/zenburn-theming)
        (solarized-light ,@display/common-theming
                         ,@display/headers/solarized-light
                         ,@display/solarized-light-theming)))
