(setq config-packages
      '(
        avy
        ob-async
        org
        evil
        ispell
        ivy
        gnus
        neotree
        projectile
        yasnippet
        ))

(defun config/init-ob-async ()
  (use-package ob-async
    :config
    (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block)))

(defun config/post-init-org ()
  ;; TODO figure out how to remove these three requires

  (require 'ox-bibtex)

  (require 'org-bullets)
  (setq org-priority-faces '((65 :inherit org-priority :foreground "red")
                             (66 :inherit org-priority :foreground "brown")
                             (67 :inherit org-priority :foreground "blue")))
  (setq org-ellipsis "▼")
  (setq org-bullets-bullet-list '("" "" "" ""))

  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))

  (require 'org-contacts)
  (setq org-contacts-files (list (os-path "~/Dropbox/contacts.org")))
  (setq org-agenda-files (list (os-path "~/Dropbox/schedule.org")))

  (spacemacs/set-leader-keys "aof" 'org-open-at-point-global)
  (add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))

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
     ("b" "#+TITLE: \n#+SLUG: \n#+DATE: 2017-mm-dd\n#+CATEGORIES: \n#+SUMMARY: \n#+DRAFT: false")))

  ;; Enable flyspell in org-mode
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; Outline style navigation
  (evil-define-key '(normal visual motion) org-mode-map
    "gh" 'outline-up-heading
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gl" 'outline-next-visible-heading
    "gu" 'outline-previous-visible-heading)

  ;; Quick refile of project tasks
  (setq org-refile-targets '((nil :regexp . "Week of")))

  ;; Hide all org-blocks, including src, quote, etc. blocks, on buffer load
  (defvar org-blocks-hidden nil)
  (defun org-toggle-blocks ()
    (interactive)
    (if org-blocks-hidden
        (org-show-block-all)
      (org-hide-block-all))
    (setq-local org-blocks-hidden (not org-blocks-hidden)))

  (add-hook 'org-mode-hook 'org-toggle-blocks)
  (define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)

  ;; File-apps
  (when is-linuxp
   (setq org-file-apps '((auto-mode . emacs)
                         ("\\.mm\\'" . default)
                         ("\\.x?html?\\'" . "/usr/bin/firefox %s")
                         ("\\.pdf\\'" . default))))

  ;; Org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages '((python .  t)
                               (haskell . t)
                               (clojure . t)
                               (dot .     t)  ; Graphviz
                               ))

  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-window-setup 'current-window)
  (setq org-babel-default-header-args:python
        (cons '(:results . "output file replace")
              (assq-delete-all :results org-babel-default-header-args)))

  ;; Ox-latex and Ox-bibtex
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
  )

(defun config/post-init-ivy ()
  (setq ivy-format-function 'ivy-format-function-arrow)
  (setq ivy-height 20)
  (setq completion-in-region-function 'ivy-completion-in-region)

  ;; Resume last ivy session
  (spacemacs/set-leader-keys
    "ai" 'ivy-resume)

  (let ((kmap ivy-minibuffer-map))
    ;; Perform default action on avy-selected minibuffer line
    (define-key kmap (kbd "C-l") 'ivy-avy)

    ;; Evil-like scrolling of ivy minibuffer
    (define-key kmap (kbd "C-u") 'ivy-scroll-down-command)
    (define-key kmap (kbd "C-d") 'ivy-scroll-up-command)

    ;; Rebind C-n/C-y/C-p to narrow/yank from buffer/paste into buffer
    (define-key kmap (kbd "C-n") 'ivy-restrict-to-matches)
    (define-key kmap (kbd "C-y") 'ivy-yank-word)

    ;; Read-only buffer of candidates with shortcuts to dispatches
    (define-key kmap (kbd "C-o") 'ivy-occur)

    ;; Non-exiting default action
    (define-key kmap (kbd "C-<return>") 'ivy-call)

    ;; Dispatch actions
    (define-key kmap (kbd "C-SPC") 'ivy-dispatching-done)
    (define-key kmap (kbd "C-S-SPC") 'ivy-dispatching-call)))

(defun config/post-init-avy ()
  (setq avy-timeout-seconds 0.35)
  (evil-global-set-key 'normal (kbd "s") 'avy-goto-char-timer)

  (global-set-key (kbd "C-h") 'avy-pop-mark)
  (global-set-key (kbd "C-l") 'evil-avy-goto-line))

(defun config/post-init-evil ()
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence "true")

  (advice-add 'evil-ex-search-next :after 'config/scroll-to-center-advice)
  (advice-add 'evil-ex-search-previous :after 'config/scroll-to-center-advice)

  ;; Interactive is needed for visual mode end-of-line to not take the \n
  (evil-global-set-keys
   '(normal visual motion)
   "H" 'evil-first-non-blank
   "L" (lambda () (interactive) (evil-end-of-line))
   "0" 'evil-jump-item))

(defun config/post-init-yasnippet ()
  (global-set-key (kbd "C-SPC") 'hippie-expand))

(defun config/post-init-projectile ()
  (setq projectile-indexing-method 'native))

(defun config/post-init-neotree ()
  (setq neo-theme 'icons
        neo-window-width 28)

  (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$"
                                 ;; Pycache and init rarely want to see
                                 "__pycache__" "__init__\\.py"))

  (evil-global-set-key 'normal (kbd "M-f") 'winum-select-window-0)
  (evil-global-set-key 'normal (kbd "M-p") 'neotree-find-project-root))

(defun config/post-init-gnus ()
  (setq user-mail-address	"ekaschalk@gmail.com"
        user-full-name	"Eric Kaschalk"

        ;; Get mail
        gnus-secondary-select-methods
        '((nnimap "gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl))
          (nntp "gmane"
                (nntp-address "news.gmane.org"))
          (nntp "news.gwene.org"))

        ;; Send mail
        message-send-mail-function 'smtpmail-send-it

        ;; Archive outgoing email in Sent folder on imap.gmail.com
        gnus-message-archive-method '(nnimap "imap.gmail.com")
        gnus-message-archive-group "[Gmail]/Sent Mail"

        ;; Auth
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                     "ekaschalk@gmail.com" nil))

        ;; SMPT Server config
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587

        ;; set return email address based on incoming email address
        gnus-posting-styles
        '(((header "to" "address@outlook.com")
           (address  "address@outlook.com"))
          ((header "to" "address@gmail.com")
           (address "address@gmail.com")))

        ;; store email in ~/gmail directory
        nnml-directory "~/gmail"
        message-directory "~/gmail"

        ;; Full size images
        mm-inline-large-images 'resize))

(defun config/post-init-ispell ()
  (setq ispell-program-name "aspell"))
