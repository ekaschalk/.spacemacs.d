(require 'macros)

(provide 'evil-config)


(setq evil-escape-key-sequence
      "jk")
(setq evil-escape-unordered-key-sequence
      "true")

(advice-add 'evil-ex-search-next
            :after 'config/scroll-to-center-advice)
(advice-add 'evil-ex-search-previous
            :after 'config/scroll-to-center-advice)

(evil-global-set-keys
 '(normal visual motion)
 "H" 'evil-first-non-blank
 "L" (lambda () (interactive) (evil-end-of-line))
 "0" 'evil-jump-item)
