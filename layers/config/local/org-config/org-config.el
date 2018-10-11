(require 'org)
(require 'org-contacts)
(require 'org-bullets)

(provide 'org-config)

;;; Bindings

(spacemacs/set-leader-keys "aof" 'org-open-at-point-global)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "r" 'org-refile)

(defun org-sort-entries-priorities () (interactive) (org-sort-entries nil ?p))
(spacemacs/set-leader-keys-for-major-mode
  'org-mode "s p" 'org-sort-entries-priorities)

(evil-define-key '(normal visual motion) org-mode-map
  "gh" 'outline-up-heading
  "gj" 'outline-forward-same-level
  "gk" 'outline-backward-same-level
  "gl" 'outline-next-visible-heading
  "gu" 'outline-previous-visible-heading)

;;; Hooks

(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'org-mode-hook 'flyspell-mode)

;;; Theming

(setq org-priority-faces '((65 :inherit org-priority :foreground "red")
                           (66 :inherit org-priority :foreground "brown")
                           (67 :inherit org-priority :foreground "blue")))
(setq org-ellipsis "")
(setq org-bullets-bullet-list '("" "" "" ""))

;;; Templates

(setq org-structure-template-alist
      '(;; Standard Blocks
        ("n" "#+NAME: ?")
        ("q" "#+BEGIN_QUOTE\n\n#+END_QUOTE")

        ;; Language Blocks
        ("c"  "#+BEGIN_SRC clojure\n\n#+END_SRC")
        ("d"  "#+BEGIN_SRC dot\n\n#+END_SRC")
        ("e"  "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC")
        ("h"  "#+BEGIN_SRC haskell\n\n#+END_SRC")
        ("la" "#+BEGIN_SRC latex\n\n#+END_SRC")
        ("l"  "#+BEGIN_SRC lisp\n\n#+END_SRC")
        ("p"  "#+BEGIN_SRC python\n\n#+END_SRC")

        ;; html-export org-html-themese collapse properties slug
        ("clps" ":PROPERTIES:\n :HTML_CONTAINER_CLASS: hsCollapsed\n :END:\n")

        ;; Hugo title slug template
        ("b" "#+TITLE: \n#+SLUG: \n#+DATE: 2018-mm-dd
#+CATEGORIES: \n#+SUMMARY: \n#+DRAFT: false")))

;;; Babel

(setq org-confirm-babel-evaluate   nil)
(setq org-src-fontify-natively     t)
(setq org-src-tab-acts-natively    t)
(setq org-src-preserve-indentation t)
(setq org-src-window-setup         'current-window)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((latex .   t)
                               (python .  t)
                               (haskell . t)
                               (clojure . t)
                               (dot .     t)))

;;; Files

(setq org-file-apps '((auto-mode . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . "/usr/bin/firefox %s")
                      ("\\.pdf\\'" . default)))

(setq org-contacts-files '("~/Dropbox/contacts.org"))
(setq org-agenda-files   '("~/Dropbox/schedule.org"))
