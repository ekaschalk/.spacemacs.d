;;; -*- lexical-binding: t -*-

;; Display mofidications for `solarized-light' and `zenburn' applied here.

;; Try out changes with `spacemacs/update-theme' to see theme updates
;; or alternatively run `spacemacs/cycle-spacemacs-theme' with 'SPC T n'.

;; Notes:
;; 1. I do not style outlines level 4 or greater because I never go that deep.
;; 2. I use dash/dash-functional basically everywhere *but* this file
;;    as we cannot access it yet without brittle advice hacks

;;; Configuration

(setq display/outline-styling/common '(:italic nil :underline t :inherit nil))
(setq display/outline-heights/common '(1.35 1.25 1.15))

(setq display/outline-palette/zenburn         '("#DFAF8F" "#BFEBBF" "#7CB8BB"))
(setq display/outline-palette/solarized-light '("#C3A29E" "#8D6B94" "#8C5F66"))

(setq display/org-blocks/common '(:italic nil :underline nil :box t))
(setq display/org-blocks        `((org-block-begin-line
                                   ,@display/org-blocks/common)
                                  (org-block-end-line
                                   ,@display/org-blocks/common)))

(setq display/company/common '(:weight bold :underline nil))
(setq display/company        `((company-tooltip-common
                                ,@display/company/common
                                :inherit company-tooltip)
                               (company-tooltip-common-selection
                                ,@display/company/common
                                :inherit company-tooltip-selection)))

;;; Utils

(defun display/style-outline (level height foreground)
  "Return 2 alists of outline/org faces for `level' and their styling."
  (let* ((level (number-to-string level))
         (outline-face (concat "outline-" level))
         (org-face (concat "org-level-" level)))
    (mapcar (lambda (face)
              `(,(intern face)
                :height ,height
                :foreground ,foreground
                ,@display/outline-styling/common))
            (list org-face outline-face))))

(defun display/style-outlines (foregrounds)
  "Execute `display/style-outline' for each of FOREGROUNDS."
  (let ((levels '(1 2 3)))
    (apply 'append
           (mapcar* 'display/style-outline
                    levels
                    display/outline-heights/common
                    foregrounds))))

;;; Solarized-light
;;;; Mode-line

(setq display/solarized-light-theming/mode-line
      `(;; active modeline has no colors
        (mode-line :inherit mode-line :background "#eee8d5"
                   :box nil :underline nil :overline "#eee8d5")
        (mode-line-inactive :inherit mode-line :background "#eee8d5"
                            :box nil :underline nil :overline nil)
        (spaceline-highlight-face :inherit mode-line :background "#eee8d5")
        (powerline-active1 :inherit mode-line :background "#eee8d5")
        (powerline-active2 :inherit mode-line :background "#eee8d5")

        ;; Inactive modeline has tint
        (powerline-inactive2 :inherit powerline-inactive1 :background "#eee8d5")))

;;;; Grouped

(setq display/solarized-light-theming
      `(solarized-light
        ,@(display/style-outlines display/outline-palette/solarized-light)

        ,@display/solarized-light-theming/mode-line
        ,@display/org-blocks
        ,@display/company

        (font-lock-comment-face :foreground "#586e75" :italic t :weight normal)
        (avy-background-face :foreground "#586e75" :italic nil)
        (font-lock-doc-face :foreground "#2aa198" :italic t :weight normal)

        ;; Makes matching parens obvious
        (sp-show-pair-match-face :inherit sp-show-pair-match-face
                                 :background "#586e75")))

;;; Zenburn
;;;; Outlines

(setq display/zenburn/outlines
      `(
        ,@(display/style-outlines display/outline-palette/zenburn)

        ;; zenburn specific changes
        (oi-face-1 :inherit outline-1)
        (oi-face-2 :inherit outline-2 :underline nil)
        (oi-face-3 :inherit outline-3 :underline nil)))

;;;; Mode-line


(setq display/zenburn/mode-line
      `(;; active modeline has no colors
        (mode-line :inherit mode-line :background "#3F3F3F"
                   :box nil :underline nil :overline nil)
        (mode-line-inactive :inherit mode-line :background "#3F3F3F"
                            :box nil :underline nil :overline nil)
        (spaceline-highlight-face :inherit mode-line :background "#3F3F3F")
        (powerline-active1 :inherit mode-line :background "#3F3F3F")
        (powerline-active2 :inherit mode-line :background "#3F3F3F")
        (powerline-inactive2 :inherit powerline-inactive1 :background nil)))

;;;; Font-Lock faces

(setq display/zenburn/font-lock-faces
      `((font-lock-type-face :foreground "LightCoral")
        (font-lock-function-name-face :foreground "CadetBlue2")

        (font-lock-comment-delimiter-face :foreground "gray35")
        (font-lock-comment-face :italic t :weight normal :foreground "gray50")
        (font-lock-doc-face :italic t :weight normal :foreground "gray65")))

;;;; Grouped

(setq display/zenburn
      `(zenburn
        ,@display/zenburn/outlines
        ,@display/zenburn/mode-line
        ,@display/zenburn/font-lock-faces

        ,@display/org-blocks
        ,@display/company

        (avy-background-face :foreground "#586e75" :italic nil)

        ;; Makes matching parens obvious
        (sp-show-pair-match-face :underline t)

        (fringe :background nil)))

;;; Set Theme Changes

(setq theming-modifications
      (list display/zenburn
            display/solarized-light-theming))
