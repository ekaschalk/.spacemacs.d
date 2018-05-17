;; Display mofidications for `solarized-light' and `zenburn' applied here

;;; Solarized-light
;;;; Outlines

(setq display/solarized-light-theming/org
      `((outline-1 :height 1.25
                   :foreground "#C3A29E"
                   :weight ,(if linux? 'normal 'ultra-bold)
                   :italic nil
                   :underline t)

        (outline-2 :height 1.15
                   :foreground "#8D6B94"
                   :weight ,(if linux? 'normal 'ultra-bold)
                   :italic nil
                   :underline t)

        (outline-3 :height 1.15
                   :foreground "#8C5F66"
                   :weight ,(if linux? 'normal 'ultra-bold)
                   :italic nil
                   :underline t)

        (org-level-1 :height 1.25
                     :inherit nil
                     :foreground "#C3A29E"
                     :weight ,(if linux? 'normal 'ultra-bold)
                     :italic nil
                     :underline t)

        (org-level-2 :height 1.15
                     :inherit nil
                     :foreground "#8D6B94"
                     :weight ,(if linux? 'normal 'ultra-bold)
                     :italic nil
                     :underline t)

        (org-level-3 :height 1.15
                     :inherit nil
                     :foreground "#8C5F66"
                     :weight ,(if linux? 'normal 'ultra-bold)
                     :italic nil)

        (org-block-begin-line :height 1.05 :foreground "#576e75"
                              :box t :weight bold)
        (org-block-end-line :height 1.05 :foreground "#576e75"
                            :box t :weight bold)))

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
        ,@display/solarized-light-theming/org
        ,@display/solarized-light-theming/mode-line

        (company-tooltip-common
         :inherit company-tooltip :weight bold :underline nil)
        (company-tooltip-common-selection
         :inherit company-tooltip-selection :weight bold :underline nil)

        (font-lock-comment-face :foreground "#586e75" :italic t :weight normal)
        (avy-background-face :foreground "#586e75" :italic nil)
        (font-lock-doc-face :foreground "#2aa198" :italic t :weight normal)

        ;; Makes matching parens obvious
        (sp-show-pair-match-face :inherit sp-show-pair-match-face
                                 :background "#586e75")))

;;; Zenburn
;;;; Outlines

(setq display/zenburn/outlines
      `((outline-1 :height 1.35
                   :foreground "#DFAF8F"
                   :weight ,(if linux? 'normal 'ultra-bold)
                   :italic nil
                   :underline t)

        (outline-2 :height 1.25
                   :foreground "#BFEBBF"
                   :weight ,(if linux? 'normal 'ultra-bold)
                   :italic nil
                   :underline t
                   :inherit nil)

        (outline-3 :height 1.15
                   :foreground "#7CB8BB"
                   :weight ,(if linux? 'normal 'ultra-bold)
                   :italic nil
                   :underline t
                   :inherit nil)

        (org-level-1 :height 1.35
                     :inherit nil
                     :foreground "#DFAF8F"
                     :weight ,(if linux? 'normal 'ultra-bold)
                     :italic nil
                     :underline t
                     :inherit nil)

        (org-level-2 :height 1.25
                     :inherit nil
                     :foreground "#BFEBBF"
                     :weight ,(if linux? 'normal 'ultra-bold)
                     :italic nil
                     :underline t
                     :inherit nil)

        (org-level-3 :height 1.15
                     :inherit nil
                     :foreground "#7CB8BB"
                     :weight ,(if linux? 'normal 'ultra-bold)
                     :italic nil
                     :inherit nil)

        (org-block-begin-line :height 1.05 :foreground "#576e75"
                              :box t :weight bold)
        (org-block-end-line :height 1.05 :foreground "#576e75"
                            :box t :weight bold)

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

        (company-tooltip-common
         :inherit company-tooltip :weight bold :underline nil)
        (company-tooltip-common-selection
         :inherit company-tooltip-selection :weight bold :underline nil)

        (avy-background-face :foreground "#586e75" :italic nil)

        ;; Makes matching parens obvious
        (sp-show-pair-match-face :underline t)

        (fringe :background nil)))

;;; Set Theme Changes

(setq theming-modifications (list display/zenburn
                                  display/solarized-light-theming))
