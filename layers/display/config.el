;;; Solarized-dark

(setq
 display/solarized-dark-theming
 `(solarized-dark
   (company-tooltip-common
    :inherit company-tooltip :weight bold :underline nil)
   (company-tooltip-common-selection
    :inherit company-tooltip-selection :weight bold :underline nil)

   (font-lock-comment-face :foreground "#586e75" :italic t :weight normal)
   (avy-background-face :foreground "#586e75" :italic nil)
   (font-lock-doc-face :foreground "#2aa198" :italic t :weight normal)

   ;; Makes matching parens obvious
   (sp-show-pair-match-face :inherit sp-show-pair-match-face
                            :background "#586e75")

   ;; active modeline has no colors
   (mode-line :inherit mode-line :background "#002b36"
              :box nil :underline nil :overline nil)
   (mode-line-inactive :inherit mode-line :background "#002b36"
                       :box nil :underline nil :overline nil)
   (spaceline-highlight-face :inherit mode-line :background "#002b36")
   (powerline-active1 :inherit mode-line :background "#002b36")
   (powerline-active2 :inherit mode-line :background "#002b36")

   ;; Inactive modeline has tint
   (powerline-inactive2 :inherit powerline-inactive1 :background "#073642")

   ;; Org and outline header updates
   (outline-1 :height 1.25
              :foreground "#C3A29E"
              :weight ,(if is-linuxp 'normal 'ultra-bold)
              :italic nil
              :underline t)

   (outline-2 :height 1.15
              :foreground "#8D6B94"
              :weight ,(if is-linuxp 'normal 'ultra-bold)
              :italic nil
              :underline t)

   (outline-3 :height 1.15
              :foreground "#8C5F66"
              :weight ,(if is-linuxp 'normal 'ultra-bold)
              :italic nil
              :underline t)

   (org-level-1 :height 1.25
                :inherit nil
                :foreground "#C3A29E"
                :weight ,(if is-linuxp 'normal 'ultra-bold)
                :italic nil
                :underline t)

   (org-level-2 :height 1.15
                :inherit nil
                :foreground "#8D6B94"
                :weight ,(if is-linuxp 'normal 'ultra-bold)
                :italic nil
                :underline t)

   (org-level-3 :height 1.15
                :inherit nil
                :foreground "#8C5F66"
                :weight ,(if is-linuxp 'normal 'ultra-bold)
                :italic nil)

   (org-block-begin-line :height 1.05 :foreground "#576e75"
                         :box t :weight bold)))

;;; Solarized-light

;; Solarized-light not as finalized as solarized-dark updates

(setq my-black "#1b1b1e")

(setq
 display/solarized-light-theming
 `(solarized-light
   ;; Makes matching parens obvious
   (sp-show-pair-match-face :inherit sp-show-pair-match-face
                            :background "light gray")

   ;; active modeline has no colors
   (mode-line :inherit mode-line :background "#fdf6e3")
   (mode-line-inactive :inherit mode-line :background "#fdf6e3")
   (spaceline-highlight-face :inherit mode-line :background "#fdf6e3")
   (powerline-active1 :inherit mode-line :background "#fdf6e3")
   (powerline-active2 :inherit mode-line :background "#fdf6e3")

   ;; Inactive modeline has tint
   (powerline-inactive2 :inherit powerline-inactive1)

   ;; Org and outline header updates
   (org-level-1 :height 1.25 :foreground ,my-black
                :background "#C9DAEA"
                :weight bold)
   (org-level-2 :height 1.15 :foreground ,my-black
                :background "#7CDF64"
                :weight bold)
   (org-level-3 :height 1.05 :foreground ,my-black
                :background "#F8DE7E"
                :weight bold)

   (outline-1 :inherit org-level-1)
   (outline-2 :inherit org-level-2)
   (outline-3 :inherit org-level-3)
   (outline-4 :inherit org-level-4)

   (org-todo :foreground ,my-black :weight extra-bold
             :background "light gray")
   (org-priority :foreground ,my-black :weight bold
                 :background "light gray")))

;;; Set Theme Changes

(setq theming-modifications (list display/solarized-dark-theming
                                  display/solarized-light-theming))
