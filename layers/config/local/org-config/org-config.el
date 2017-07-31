(require 'org)
(require 'org-contacts)
(require 'org-bullets)
(require 'ox-bibtex)
(require 'ox-extra)

(provide 'org-config)

;;; Bindings and Hooks

(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'org-toggle-blocks)

(spacemacs/set-leader-keys "aof" 'org-open-at-point-global)

(define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)

(evil-define-key '(normal visual motion) org-mode-map
  "gh" 'outline-up-heading
  "gj" 'outline-forward-same-level
  "gk" 'outline-backward-same-level
  "gl" 'outline-next-visible-heading
  "gu" 'outline-previous-visible-heading)

;; Quick refile of project tasks
(setq org-refile-targets '((nil :regexp . "Week of")))

(spacemacs/set-leader-keys-for-major-mode 'org-mode "r" 'org-refile)

;;; Theming

(setq org-priority-faces '((65 :inherit org-priority :foreground "red")
                           (66 :inherit org-priority :foreground "brown")
                           (67 :inherit org-priority :foreground "blue")))
(setq org-ellipsis "")
(setq org-bullets-bullet-list '("" "" "" ""))

;;; Templates

(setq
 org-structure-template-alist
 '(("n" "#+NAME: ?")
   ("q" "#+BEGIN_QUOTE\n\n#+END_QUOTE")

   ;; Language Blocks
   ("c" "#+BEGIN_SRC clojure\n\n#+END_SRC")
   ("d" "#+BEGIN_SRC dot\n\n#+END_SRC")
   ("e" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC")
   ("h" "#+BEGIN_SRC haskell\n\n#+END_SRC")
   ("l" "#+BEGIN_SRC lisp\n\n#+END_SRC")
   ("p" "#+BEGIN_SRC python\n\n#+END_SRC")

   ;; Collapse previous header by default in themed html export
   ("clps" ":PROPERTIES:\n :HTML_CONTAINER_CLASS: hsCollapsed\n :END:\n")
   ;; Hugo title template
   ("b" "#+TITLE: \n#+SLUG: \n#+DATE: 2017-mm-dd
#+CATEGORIES: \n#+SUMMARY: \n#+DRAFT: false")))

;;; Org Blocks

;; Hide all org-blocks, including src, quote, etc. blocks, on buffer load
(defvar org-blocks-hidden nil)
(defun org-toggle-blocks ()
  (interactive)
  (if org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq-local org-blocks-hidden (not org-blocks-hidden)))

;;; Export

(ox-extras-activate '(ignore-headlines))

(setq org-contacts-files (list (os-path "~/Dropbox/contacts.org")))
(setq org-agenda-files (list (os-path "~/Dropbox/schedule.org")))

(when is-linuxp
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . "/usr/bin/firefox %s")
                        ("\\.pdf\\'" . default))))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-minted-options '(("frame" "lines")
                                 ("fontsize" "\\scriptsize")
                                 ("xleftmargin" "\\parindent")
                                 ("linenos" "")))
(setq
 org-latex-pdf-process
 '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;; Babel

(org-babel-do-load-languages
 'org-babel-load-languages '((python .  t)
                             (haskell . t)
                             (clojure . t)
                             (dot .     t)))

(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-src-window-setup 'current-window)
(setq org-babel-default-header-args:python
      (cons '(:results . "output file replace")
            (assq-delete-all :results org-babel-default-header-args)))
