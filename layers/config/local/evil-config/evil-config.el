(require 'macros)

(provide 'evil-config)


(setq evil-escape-key-sequence
      "jk")
(setq evil-escape-unordered-key-sequence
      "true")

(defun evil-scroll-to-center-advice (&rest args)
  "Scroll line to center, for advising functions."
  (evil-scroll-line-to-center (line-number-at-pos)))

(advice-add 'evil-ex-search-next
            :after 'evil-scroll-to-center-advice)
(advice-add 'evil-ex-search-previous
            :after 'evil-scroll-to-center-advice)

(defun evil-execute-q-macro ()
  "Execute @q"
  (interactive)
  (evil-execute-macro 1 "@q"))

(evil-global-set-keys
 '(normal visual motion)
 "H" 'evil-first-non-blank
 "L" (lambda () (interactive) (evil-end-of-line))
 "0" 'evil-jump-item)

(evil-global-set-key
 'normal
 "Q" 'evil-execute-q-macro)
