;;; -*- lexical-binding: t -*-

;; Display mofidications for `solarized-light' and `zenburn' applied here.

;; Try out changes with `spacemacs/update-theme' to see theme updates
;; or alternatively run `spacemacs/cycle-spacemacs-theme' with 'SPC T n'.
;; I do not style outlines level 4 or greater because I never go that deep.

;;; Configuration
;;;; Core

(setq solarized-use-variable-pitch nil)
(setq face-remapping-alist '(;; Headers
                             (outline-1 org-level-1)
                             (outline-2 org-level-2)
                             (outline-3 org-level-3)

                             ;; Modeline
                             (powerline-active1 mode-line)
                             (powerline-active2 mode-line)
                             (powerline-inactive1 mode-line-inactive)
                             (powerline-inactive2 mode-line-inactive)
                             (spaceline-highlight-face mode-line)))

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
         :foreground "#C3A29E")
        (org-level-2
         ,@display/headers/common
         :height 1.25
         :foreground "#8D6B94")
        (org-level-3
         ,@display/headers/common
         :height 1.15
         :foreground "#8C5F66")))

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

(setq display/mode-line `((mode-line :background nil :box nil :underline nil)
                          (mode-line-inactive :box nil :underline nil)))

;;; Solarized-light
;;;; Grouped

(setq display/solarized-light-theming
      `(solarized-light

        ,@display/headers/solarized-light
        ,@display/org-blocks
        ,@display/company
        ,@display/mode-line

        (font-lock-comment-face :foreground "#586e75" :italic t :weight normal)
        (avy-background-face :foreground "#586e75" :italic nil)
        (font-lock-doc-face :foreground "#2aa198" :italic t :weight normal)

        ;; Makes matching parens obvious
        (sp-show-pair-match-face :inherit sp-show-pair-match-face
                                 :background "#586e75")))

;;; Zenburn

(setq display/zenburn
      `(zenburn

        ,@display/headers/zenburn
        ,@display/org-blocks
        ,@display/company
        ,@display/mode-line

        (avy-background-face :foreground "#586e75" :italic nil)

        ;; Makes matching parens obvious
        (sp-show-pair-match-face :underline t)

        (fringe :background nil)

        (font-lock-type-face :foreground "LightCoral")
        (font-lock-function-name-face :foreground "CadetBlue2")

        (font-lock-comment-delimiter-face :foreground "gray35")
        (font-lock-comment-face :italic t :weight normal :foreground "gray50")
        (font-lock-doc-face :italic t :weight normal :foreground "gray65")
        ))

;;; Set Theme Changes

(setq theming-modifications
      (list display/zenburn
            display/solarized-light-theming))
