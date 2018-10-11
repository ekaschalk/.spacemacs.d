(require 'macros)

(provide 'evil-config)


;;; Utils

(defun evil-execute-q-macro ()
  "Execute macro stores in q-register, ie. run `@q'."
  (interactive)
  (evil-execute-macro 1 "@q"))

(defun evil-scroll-to-center-advice (&rest args)
  "Scroll line to center, for advising functions."
  (evil-scroll-line-to-center (line-number-at-pos)))

(defun evil-end-of-line-interactive ()
  "Wrap `evil-end-of-line' in interactive, fix point being 1+ in visual motion."
  (interactive)
  (evil-end-of-line))

;;; Advice

(advice-add 'evil-ex-search-next
            :after 'evil-scroll-to-center-advice)
(advice-add 'evil-ex-search-previous
            :after 'evil-scroll-to-center-advice)

;;; Configuration

(setq evil-escape-key-sequence "jk")
(setq evil-escape-unordered-key-sequence "true")

;;; Bindings

(evil-global-set-key 'normal "Q" 'evil-execute-q-macro)

(evil-global-set-keys
 '(normal visual motion)
 "H" 'evil-first-non-blank
 "L" 'evil-end-of-line-interactive
 "0" 'evil-jump-item)
