;; -*- mode: emacs-lisp -*-

;; TEMP TODOS
;; make pretty-fonts macro require fonts to be installed
;; use :if is-linuxp
;; sort out using :variables in layers config for eg python


;;; Introduction

;; -- Eric Kaschalk's Spacemacs Configuration --
;; -- Emacs 25.2.1 --
;; -- Dev Branch - Release 0.200.9.x - pulled: 5/29 - packages updated: 5/29 --
;; -- Dual config for Windows and Arch Linux --
;; -- MIT License --
;; -- Contact: ekaschalk@gmail.com --
;;
;; See README for details
;; See TASKS for project management such as known bugs, planned updates, history
;; Theme is solarized-light or solarized-dark dependent on time of day
;;
;; Organization
;; ---------
;; Configuration is grouped by theme. The current groups are:
;; Display - Ivy - Configuration - Misc - Navigation - Python - Org - Outshine
;;
;; Each group is broken into further components for targetted enabling/disabling
;; Some groups require a specific execution ordering. Ordering requirements are
;; specifed with Group x comments. Within the group, the packages are lexical.

(setq time-to-use-dark 0)

;;; OS-Config

;; Utilities for integrating Windows and Linux.
;; Used in spacemacs initialization - must load before layers

(setq is-linuxp (eq system-type 'gnu/linux))
(defun if-linux (x y) (if is-linuxp x y))
(defun if-linux-call (x y) (if is-linuxp (funcall x) (funcall y)))
(defun when-linux (x) (when is-linuxp x))
(defun when-linux-call (x) (when is-linuxp (funcall x)))
(defun unless-linux (x) (unless is-linuxp x))
(defun unless-linux-call (x) (unless is-linuxp (funcall x)))
(defun os-path (x) (if is-linuxp x (concat "c:/" x)))

;;; Spacemacs-Layers
;;;; Layers

(setq dotspacemacs/layers/core
      '(better-defaults
        git
        gnus
        org
        ranger
        syntax-checking
        (auto-completion :variables
                         auto-completion-return-key-behavior 'complete
                         auto-completion-tab-key-behavior 'complete
                         auto-completion-enable-snippets-in-popup t)
        (evil-snipe :variables
                    evil-snipe-enable-alternate-f-and-t-behaviors t)
        (ibuffer :variables
                 ibuffer-group-buffers-by 'projects)
        (ivy :variables
             ivy-extra-directories nil)
        (shell :variables
               shell-default-shell 'eshell)
        (version-control :variables
                         version-control-global-margin t
                         version-control-diff-tool 'git-gutter+))

      dotspacemacs/layers/langs
      `(emacs-lisp
        html
        javascript
        rust  ; I only use atm for .toml configuration files
        (clojure :variables
                 clojure-enable-fancify-symbols t)
        (haskell :variables
                 haskell-completion-backend 'intero)
        (python :variables
                python-sort-imports-on-save t
                python-test-runner 'pytest))

      dotspacemacs/layers/rare
      '(
        markdown    ; Markdown mode for viewing outside documentation
        graphviz    ; Graphviz mode for usage with org-babel
        )

      ;; OS-Specific and Local Packages
      dotspacemacs/layers/local
      '(
        (display :location local)
        (langs :location local)
        (macros :location local)
        (outlines :location local)
        (personal :location local)
        )

      dotspacemacs/layers/linux '()
      dotspacemacs/layers/windows '())

;;;; Additional Packages

(setq dotspacemacs/additional/packages
      '(
        ;; Misc
        virtualenvwrapper        ; Python environment management
        ob-async                 ; Asynchronous org-babel source block execution
        (dash-functional         ; More dash functional programming utils
         :location (recipe :fetcher github
                           :repo "magnars/dash.el"))

        ;; Themes
        solarized-theme
        ))

;;;; Spacemacs

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/"
                                           "c:/~/.spacemacs.d/layers/")
   dotspacemacs-additional-packages dotspacemacs/additional/packages
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-but-keep-unused
   dotspacemacs-configuration-layers
   (append dotspacemacs/layers/core
           dotspacemacs/layers/langs
           dotspacemacs/layers/rare
           dotspacemacs/layers/local
           (when-linux dotspacemacs/layers/linux)
           (unless-linux dotspacemacs/layers/windows))))

;;; Spacemacs-Init
;;;; Utilities

(setq theme-to-use (if (< (string-to-number
                           (substring
                            (current-time-string) 11 13))
                          time-to-use-dark)
                       'solarized-light
                     'solarized-dark))

;;;; Configuration

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-themes `(,theme-to-use)
   dotspacemacs-default-font `("operator mono medium"
                               ;; "Fira Code Retina"
                               :size ,(if-linux 18 12)
                               :powerline-scale 1.5)
;;;; Static

   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '()
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-generate-layout-names t
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup (if-linux nil t)
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "rg" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-frame-title-format "%I@%S"
   dotspacemacs-icon-title-format nil
   dotspacemacs-whitespace-cleanup 'trailing
   dotspacemacs-zone-out-when-idle nil))

;;;; User-init

(defun dotspacemacs/user-init ()
  "Special settings to run before user-config runs."
  ;; Rids the verbose custom settings from being written to .spacemacs
  (setq custom-file "./elisp/.custom-settings.el")

  ;; Custom Packages
  (push "c:/~/elisp/" load-path))

;;; Spacemacs-User-config

(defun dotspacemacs/user-config ()
  "Require for .spacemacs, evaluates modules."

  (require 'dash-functional)  ; dash/s.el loaded by default, not dash-functional

  ;; Group 2
  (module/display)

  ;; Rest
  (module/configuration)
  (module/ivy)
  (module/misc)
  (module/navigation)
  (module/org)

  ;; Experimenting
  (spacemacs/set-leader-keys "bb" 'ibuffer)
  )

;;; Display

(defun module/display ()
  (unless-linux-call 'module/display/windows-frame-size-fix)
  )

;;;; Windows-frame-size-fix

(defun module/display/windows-frame-size-fix ()
  "Surface has 200% scaling, doesn't apply to emacs, f2 to fix init zooming."

  (add-to-list 'default-frame-alist '(font . "operator mono medium"))
  (set-face-attribute 'default t :font "operator mono medium")

  (global-set-key (kbd "<f2>") (xis (interactive) (zoom-frm-out) (zoom-frm-out))))

;;; Ivy

(defun module/ivy ()
  "Ivy completion framework configuration."

  ;; Perform default action on avy-selected minibuffer line
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-avy)
  ;; Evil-like scrolling of ivy minibuffer
  (define-key ivy-minibuffer-map (kbd "C-u") 'ivy-scroll-down-command)
  (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-scroll-up-command)

  ;; Rebind C-n/C-y/C-p to narrow/yank from buffer/paste into buffer
  (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-restrict-to-matches)
  (define-key ivy-minibuffer-map (kbd "C-y") 'ivy-yank-word)
  ;; Read-only buffer of candidates with shortcuts to dispatches
  (define-key ivy-minibuffer-map (kbd "C-o") 'ivy-occur)

  ;; Non-exiting default action
  (define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-call)
  ;; Dispatch actions
  (define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-dispatching-done)
  (define-key ivy-minibuffer-map (kbd "C-S-SPC") 'ivy-dispatching-call)

  ;; Resume last ivy session
  (spacemacs/set-leader-keys (kbd "ai") 'ivy-resume)

  (setq ivy-format-function 'ivy-format-function-arrow
        ivy-height 20
        completion-in-region-function 'ivy-completion-in-region))

;;; Configuration

(defun module/configuration ()
  (module/configuration/editing)
  (module/configuration/evil)
  (module/configuration/visual))

;;;; Editing

(defun module/configuration/editing ()
  "Editing toggles."

  (hungry-delete-mode 1)                                ; cut contiguous space
  (spacemacs/toggle-aggressive-indent-globally-on)      ; auto-indentation
  (add-hook 'org-mode-hook (lambda () (auto-fill-mode 1))))  ; SPC splits past 80

;;;; Evil

(defun module/configuration/evil ()
  "Update evil settings."

  (setq-default evil-escape-key-sequence "jk"
                evil-escape-unordered-key-sequence "true"))

;;;; Visual

(defun module/configuration/visual ()
  "Visual toggles."

  (spacemacs/toggle-highlight-long-lines-globally-on)
  (fringe-mode '(1 . 1))                         ; Minimal left padding
  (rainbow-delimiters-mode-enable)               ; Paren color based on depth
  (global-highlight-parentheses-mode 1)          ; Highlight containing parens
  (spacemacs/toggle-mode-line-minor-modes-off))  ; no uni symbs next to major

;;; Misc

(defun module/misc ()
  (module/misc/aspell)
  (module/misc/auto-completion)
  (module/misc/eww)
  (module/misc/gnus)
  (module/misc/lisp-state)
  (module/misc/macros)
  (module/misc/neotree)
  (module/misc/projectile)
  (module/misc/shell)
  (module/misc/windows)
  (module/misc/yassnippet))

;;;; Aspell

(defun module/misc/aspell ()
  "Setup aspell."

  (setq ispell-program-name "aspell"))

;;;; Auto-completion

(defun module/misc/auto-completion ()
  "Autocompletion face modifications."

  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))

;;;; Eww

(defun module/misc/eww ()
  ;; Auto-rename new eww buffers
  (defun rename-eww-hook ()
    "Rename eww browser's buffer so sites open in new page."
    (rename-buffer "eww" t))
  (add-hook 'eww-mode-hook #'rename-eww-hook)

  ;; eventually add a func that opens current docs im using all at once
  ;; http://toolz.readthedocs.io/en/latest/api.html#toolz.itertoolz.interleave
  ;; http://docs.hylang.org/en/stable/language/api.html
  )

;;;; GNUs

(defun module/misc/gnus ()
  "GNUS setup and user details. Nothing significant atm."

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

;;;; Lisp-state

(defun module/misc/lisp-state ()
  "Add lisp state shortcut to Clojure and Hy."

  (spacemacs/set-leader-keys-for-major-mode
    'clojure-mode (kbd ",") 'lisp-state-toggle-lisp-state)
  (spacemacs/set-leader-keys-for-major-mode
    'hy-mode (kbd ",") 'lisp-state-toggle-lisp-state))

;;;; Macros

(defun module/misc/macros ()
  "Evil Q shortcut for vim macros set at @q."

  (evil-global-set-key 'normal (kbd "Q")
                       (lambda () (interactive) (evil-execute-macro 1 "@q"))))

;;;; Neotree
(defun module/misc/neotree ()
  "Neotree configuration."

  (setq neo-theme 'icons
        neo-window-width 28)

  (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$"
                                 ;; Pycache and init rarely want to see
                                 "__pycache__" "__init__\\.py"))

  (evil-global-set-key 'normal (kbd "M-f") 'winum-select-window-0)
  (evil-global-set-key 'normal (kbd "M-p") 'neotree-find-project-root))

;;;; Projectile

(defun module/misc/projectile ()
  "Project config, respect .projectile files."

  (setq projectile-indexing-method 'native))

;;;; Shell

(defun module/misc/shell ()
  "Quick eshell with vim interaction."

  (defun my-spacemacs/shell-pop-eshell ()
    (interactive)
    (spacemacs/shell-pop-eshell nil)
    (if (string= major-mode "eshell-mode")
        (evil-insert 1)
      (evil-escape)))

  (evil-global-set-key 'normal (kbd "C-e") 'my-spacemacs/shell-pop-eshell)
  (evil-global-set-key 'insert (kbd "C-e") 'my-spacemacs/shell-pop-eshell)

  ;; Enables Python shell to print unicode
  ;; TODO might have to make this pyvenv hook
  (setenv "PYTHONIOENCODING" "utf-8")
  (setenv "LANG" "en_US.UTF-8"))

;;;; Windows

(defun module/misc/windows ()
  "Additional window management bindings."

  (evil-define-key 'normal outline-minor-mode-map (kbd "C-M-i")  ; M-tab
    'spacemacs/alternate-buffer)

  (global-set-key (kbd "M-d") 'spacemacs/delete-window))

;;;; Yassnippet

(defun module/misc/yassnippet ()
  "Yassnippet bindings and config."

  (global-set-key (kbd "C-SPC") 'hippie-expand))

;;; Navigation

(defun module/navigation ()
  (module/navigation/avy)
  (module/navigation/extra-bindings)
  (module/navigation/file-links)
  (module/navigation/searching))

;;;; Avy

(defun module/navigation/avy ()
  "Avy keybindings and custom motions."

  (setq avy-timeout-seconds 0.35)
  (evil-global-set-key 'normal (kbd "s") 'avy-goto-char-timer)

  (global-set-key (kbd "C-h") 'avy-pop-mark)
  (global-set-key (kbd "C-l") 'evil-avy-goto-line))

;;;; Extra-bindings

(defun module/navigation/extra-bindings ()
  "Rebind H, L, and 0 to BOL, EOL, old %."

  ;; H and L move to modified BOL and EOL
  (evil-global-set-key 'normal (kbd "H") 'evil-first-non-blank)
  (evil-global-set-key 'visual (kbd "H") 'evil-first-non-blank)
  (evil-global-set-key 'motion (kbd "H") 'evil-first-non-blank)

  (evil-global-set-key 'normal (kbd "L") 'evil-end-of-line)
  (evil-global-set-key 'visual (kbd "L")
                       (lambda () (interactive)  ; otherwise it goes past EOL
                         (evil-end-of-line)))
  (evil-global-set-key 'motion (kbd "L") 'evil-end-of-line)

  ;; I find '%' very useful but an annoying to reach binding.
  ;; Since H is bound to BOL, we can rebind it to 0.
  (evil-global-set-key 'normal (kbd "0") 'evil-jump-item)
  (evil-global-set-key 'visual (kbd "0") 'evil-jump-item)
  (evil-global-set-key 'motion (kbd "0") 'evil-jump-item))

;;;; File-links

(defun module/navigation/file-links ()
  "Quick binding for opening org-formatted links anywhere."

  (spacemacs/set-leader-keys (kbd "aof") 'org-open-at-point-global))

;;;; Searching

(defun module/navigation/searching ()
  "Evil searching scrolls to center of match."

  (advice-add 'evil-ex-search-next :after
              (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
  (advice-add 'evil-ex-search-previous :after
              (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos)))))

;;; Org

(defun module/org ()
  (with-eval-after-load 'org
    (when-linux-call 'module/org/linux-file-apps)
    (module/org/babel)
    (module/org/exports)
    (module/org/gcal)
    (module/org/misc)
    (module/org/templates)
    (module/org/theming)))

;;;; Linux-file-apps

(defun module/org/linux-file-apps ()
  "Modify default file apps for Linux."

  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . "/usr/bin/firefox %s")
                        ("\\.pdf\\'" . default))))

;;;; Babel

(defun module/org/babel ()
  "Org babel languages and config."

  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t  ; Otherwise python is painful
        org-src-window-setup 'current-window)  ; `, ,` opens in same window
  (org-babel-do-load-languages
   'org-babel-load-languages '((python .  t)
                               (haskell . t)
                               (clojure . t)
                               (dot .     t)  ; Graphviz
                               ))

  ;; Enables interactive plotting
  (setq org-babel-default-header-args:python
        (cons '(:results . "output file replace")
              (assq-delete-all :results org-babel-default-header-args)))

  ;; Blocks with :async will be executed asynchronously
  (require 'ob-async)
  (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block))

;;;; Exports

(defun module/org/exports ()
  "Org exporting setup."

  (with-eval-after-load 'ox-bibtex  ; This eval might not be needed
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq
     org-latex-listings 'minted
     org-latex-minted-options
     '(("frame" "lines")
       ("fontsize" "\\scriptsize")
       ("xleftmargin" "\\parindent")
       ("linenos" ""))
     org-latex-pdf-process
     '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       ))))

;;;; Gcal

(defun module/org/gcal ()
  "Org google calendar integration. Not actively using atm."

  (require 'org-contacts)
  ;; (require 'org-gcal)
  ;; (load (if-linux "~/Dropbox/secrets.el"
  ;;                 "c:/~/Dropbox/secrets.el") t)
  ;; (setq org-gcal-file-alist
  ;;       `(("ekaschalk@gmail.com" .
  ;;          ,(if-linux "~/Dropbox/schedule.org" "c:/~/Dropbox/schedule.org"))))
  (setq org-contacts-files
        `(,(if-linux "~/Dropbox/contacts.org" "c:/~/Dropbox/contacts.org")))
  (setq org-agenda-files
        `(,(if-linux "~/Dropbox/schedule.org" "c:/~/Dropbox/schedule.org")))
  )

;;;; Misc

(defun module/org/misc ()
  "Misc org-mode bindings and improvements."

  ;; Header property ignore for true no-export of header and its contents
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))

  ;; Quick refile of project tasks
  (setq org-refile-targets
        '((nil :regexp . "Week of")))

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

  ;; Enable flyspell in org-mode
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; Outline style navigation
  (evil-define-key '(normal visual motion) org-mode-map
    "gh" 'outline-up-heading
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gl" 'outline-next-visible-heading
    "gu" 'outline-previous-visible-heading))

;;;; Templates

(defun module/org/templates ()
  "Org-babel template code-block expansions."

  (mapc (lambda (x) (add-to-list 'org-structure-template-alist x))
        (list
         ;; Name block
         '("n" "#+NAME: ?")
         ;; Language Blocks
         '("c" "#+BEGIN_SRC clojure\n\n#+END_SRC")
         '("e" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC")
         '("l" "#+BEGIN_SRC lisp\n\n#+END_SRC")
         '("h" "#+BEGIN_SRC haskell\n\n#+END_SRC")
         '("p" "#+BEGIN_SRC python\n\n#+END_SRC")
         ;; Graphviz Block
         '("d" "#+BEGIN_SRC dot\n\n#+END_SRC")
         ;; Collapse previous header by default in themed html export
         '("clps" ":PROPERTIES:\n :HTML_CONTAINER_CLASS: hsCollapsed\n :END:\n")
         ;; Hugo title template
         '("b" "#+TITLE: \n#+SLUG: \n#+DATE: 2017-mm-dd\n#+CATEGORIES: \n#+SUMMARY: \n#+DRAFT: false")
         )))

;;;; Theming

(defun module/org/theming ()
  "Org theming updates."

  (require 'org-bullets)
  (setq org-priority-faces '((65 :inherit org-priority :foreground "red")
                             (66 :inherit org-priority :foreground "brown")
                             (67 :inherit org-priority :foreground "blue"))
        org-ellipsis "▼"
        org-bullets-bullet-list '("" "" "" "")))
