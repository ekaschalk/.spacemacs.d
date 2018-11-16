;;; Funcs -*- lexical-binding: t; -*-
;;; Owned Packages
;;;; All-the-icons-ivy

(when (configuration-layer/package-used-p 'all-the-icons-ivy)
  (defun all-the-icons-ivy-file-transformer-stdized (s)
    "Fix `all-the-icons-ivy-file-transformer' vertical alignment issues."
    ;; I like slightly < 1 to make icons not so dominating. Alignment issues are
    ;; only present when height is more than 1. Also note some (but not all)
    ;; projectile funcs double-dip on this transform, like `counsel-projectile'.
    ;; I don't use the funcs it double dips on really right now, will fix this
    ;; eventually
    (format "%s\t%s"
            (propertize "\t" 'display
                        (all-the-icons-icon-for-file s :height 0.9 :v-adjust 0))
            s)))
