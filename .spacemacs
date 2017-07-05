;; -*- mode: emacs-lisp -*-

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
(setq is-linuxp (eq system-type 'gnu/linux))
(defun if-linux (x y) (if is-linuxp x y))
(defun if-linux-call (x y) (if is-linuxp (funcall x) (funcall y)))
(defun when-linux (x) (when is-linuxp x))
(defun when-linux-call (x) (when is-linuxp (funcall x)))
(defun unless-linux (x) (unless is-linuxp x))
(defun unless-linux-call (x) (unless is-linuxp (funcall x)))

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
      '(pandoc      ; Pandoc for more export options, used for blogging
        markdown    ; Markdown mode for viewing outside documentation
        graphviz    ; Graphviz mode for usage with org-babel
        restclient  ; REST client for usage with org-babel
        )

      ;; OS-Specific and Local Packages
      dotspacemacs/layers/local '()
      dotspacemacs/layers/linux '()
      dotspacemacs/layers/windows '())

;;;; Additional Packages

(setq dotspacemacs/additional/packages
      '(;; Outline mode base package enhancements
        outshine                 ; Required for navi-mode
        navi-mode                ; Navbar on buffer outlines

        ;; Org
        org-gcal                 ; google calendar syncing
        org-vcard                ; Import/export google contacts

        ;; Misc
        helm-spotify-plus        ; Spotify improvements
        virtualenvwrapper        ; Python environment management
        ob-async                 ; Asynchronous org-babel source block execution
        (dash-functional         ; More dash functional programming utils
         :location (recipe :fetcher github
                           :repo "magnars/dash.el"))

        ;; Visual Enhancements
        all-the-icons-ivy        ; Ivy prompts use file icons
        pretty-mode              ; Adds onto prettify-mode
        spaceline-all-the-icons  ; Spaceline integrates file and other icons
        (prettify-utils          ; Useful add pretty symbols macro
         :location (recipe :fetcher github
                           :repo "Ilazki/prettify-utils.el"))

        ;; Themes
        solarized-theme

        ;; Testing
        hierarchy
        ))

;;;; Spacemacs

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path nil
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
  (setq custom-file "./elisp/.custom-settings.el"))

;;; Spacemacs-User-config

(defun dotspacemacs/user-config ()
  "Require for .spacemacs, evaluates modules."

  (require 'dash-functional)  ; dash/s.el loaded by default, not dash-functional

  ;; Group 1
  (module/macros)

  ;; Group 2
  (module/display)

  ;; Rest
  (module/configuration)
  (module/ivy)
  (module/misc)
  (module/navigation)
  (module/org)
  (module/outshine)
  (module/python)

  ;; My Packages (Too large to keep in config)
  (load-file (if-linux "~/elisp/outline-ivy.el" "c:/~/elisp/outline-ivy.el"))
  (global-set-key (kbd "C-j") 'oi-jump)

  ;; Personal use
  (module/blog))

;;; Macros

(defun module/macros ()
  "Macros utilized throughout my configuration."

  (defun xi-find-args (seq)
    "Collect xi args."
    (seq-sort
     (lambda (sym1 sym2)
       (< (string-to-number (substring (symbol-name sym1) 1))
          (string-to-number (substring (symbol-name sym2) 1))))
     (seq-filter
      (lambda (x)
        (and (symbolp x) (equal 0 (string-match "x[0-9]+" (symbol-name x)))))
      (-flatten seq))))

  (defmacro xi (&rest BODY)
    "Anonymous func maco, see https://ekaschalk.github.io/post/xi-macro/."
    `(lambda ,(xi-find-args BODY) ,BODY))

  (defmacro xis (&rest BODY)
    "Anonymous func maco without collecting next form, for progns."
    `(lambda ,(xi-find-args BODY) ,@BODY)))

;;; Display

(defun module/display ()
  ;; Group 1
  (unless-linux-call 'module/display/windows-frame-size-fix)

  ;; Group 2
  (module/display/fontsets)
  (module/display/font-locks)

  ;; Rest
  (module/display/all-the-icons)
  (module/display/extra-syntax-highlighting)
  (module/display/modeline)
  (module/display/outline-ellipsis-modification)
  (module/display/outline-bullets)
  (module/display/prettify-magit)
  (module/display/prettify-symbols)
  (module/display/shell)
  (module/display/theme-updates))

;;;; Windows-frame-size-fix

(defun module/display/windows-frame-size-fix ()
  "Surface has 200% scaling, doesn't apply to emacs, f2 to fix init zooming."

  (add-to-list 'default-frame-alist '(font . "operator mono medium"))
  (set-face-attribute 'default t :font "operator mono medium")

  (global-set-key (kbd "<f2>") (xis (interactive) (zoom-frm-out) (zoom-frm-out))))

;;;; Fontsets

(defun module/display/fontsets ()
  "Set right fonts for missing and all-the-icons unicode points."

  ;; Fira code ligatures. Fira Code Symbol is a different font than Fira Code!
  ;; You can use any font you wish with just the ligatures
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

  (defun set-icon-fonts (CODE-FONT-ALIST)
    "Utility to associate many unicode points with specified fonts."
    (--each CODE-FONT-ALIST
      (-let (((font . codes) it))
        (--each codes
          (set-fontset-font t `(,it . ,it) font)))))

  ;; The icons you see are not the correct icons until this is evaluated!
  (set-icon-fonts
   '(("fontawesome"
      ;;                         
      #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

     ("all-the-icons"
      ;;    
      #xe907 #xe928)

     ("github-octicons"
      ;;                          
      #xf091 #xf059 #xf076 #xf075 #xe192  #xf016)

     ("material icons"
      ;;        
      #xe871 #xe918 #xe3e7)

     ("Symbola"
      ;; 𝕊    ⨂      ∅      ⟻    ⟼     ⊙      𝕋       𝔽
      #x1d54a #x2a02 #x2205 #x27fb #x27fc #x2299 #x1d54b #x1d53d
      ;; 𝔹    𝔇       𝔗
      #x1d539 #x1d507 #x1d517))))

;;;; Font-locks
;;;;; Core

(defun module/display/font-locks ()
  "Enable following font-locks for appropriate modes."

  (defun -add-font-lock-kwds (FONT-LOCK-ALIST)
    (font-lock-add-keywords
     nil (--map (-let (((rgx uni-point) it))
               `(,rgx (0 (progn
                           (compose-region
                            (match-beginning 1) (match-end 1)
                            ,(concat "\t" (list uni-point)))
                           nil))))
              FONT-LOCK-ALIST)))

  (defmacro add-font-locks (FONT-LOCK-HOOKS-ALIST)
    `(--each ,FONT-LOCK-HOOKS-ALIST
       (-let (((font-locks . mode-hooks) it))
         (--each mode-hooks
           (add-hook it (-partial '-add-font-lock-kwds
                                  (symbol-value font-locks)))))))

  (add-font-locks
   '(;; Ligatures
     (fira-font-lock-alist prog-mode-hook org-mode-hook)

     ;; Language updates not possible with prettify-symbols
     (hy-font-lock-alist     hy-mode-hook))))

;;;;; Language-font-locks

(defconst hy-font-lock-alist
  ;; self does not work as a prettify symbol for hy, unlike python
  '(("\\(self\\)"   ?⊙)))

;;;;; Fira-font-locks

(defconst fira-font-lock-alist
  '(;; OPERATORS
    ;; Pipes
    ("\\(<|\\)" #Xe14d) ("\\(<>\\)" #Xe15b) ("\\(<|>\\)" #Xe14e) ("\\(|>\\)" #Xe135)

    ;; Brackets
    ("\\(<\\*\\)" #Xe14b) ("\\(<\\*>\\)" #Xe14c) ("\\(\\*>\\)" #Xe104)
    ("\\(<\\$\\)" #Xe14f) ("\\(<\\$>\\)" #Xe150) ("\\(\\$>\\)" #Xe137)
    ("\\(<\\+\\)" #Xe155) ("\\(<\\+>\\)" #Xe156) ("\\(\\+>\\)" #Xe13a)

    ;; Equality
    ("\\(!=\\)" #Xe10e) ("\\(!==\\)"         #Xe10f) ("\\(=/=\\)" #Xe143)
    ("\\(/=\\)" #Xe12c) ("\\(/==\\)"         #Xe12d)
    ("\\(===\\)"#Xe13d) ("[^!/]\\(==\\)[^>]" #Xe13c)

    ;; Equality Special
    ("\\(||=\\)"  #Xe133) ("[^|]\\(|=\\)" #Xe134)
    ("\\(~=\\)"   #Xe166)
    ("\\(\\^=\\)" #Xe136)
    ("\\(=:=\\)"  #Xe13b)

    ;; Comparisons
    ("\\(<=\\)" #Xe141) ("\\(>=\\)" #Xe145)
    ("\\(</\\)" #Xe162) ("\\(</>\\)" #Xe163)

    ;; Shifts
    ("[^-=]\\(>>\\)" #Xe147) ("\\(>>>\\)" #Xe14a)
    ("[^-=]\\(<<\\)" #Xe15c) ("\\(<<<\\)" #Xe15f)

    ;; Dots
    ("\\(\\.-\\)"    #Xe122) ("\\(\\.=\\)" #Xe123)
    ("\\(\\.\\.<\\)" #Xe125)

    ;; Hashes
    ("\\(#{\\)"  #Xe119) ("\\(#(\\)"   #Xe11e) ("\\(#_\\)"   #Xe120)
    ("\\(#_(\\)" #Xe121) ("\\(#\\?\\)" #Xe11f) ("\\(#\\[\\)" #Xe11a)

    ;; REPEATED CHARACTERS
    ;; 2-Repeats
    ("\\(||\\)" #Xe132)
    ("\\(!!\\)" #Xe10d)
    ("\\(%%\\)" #Xe16a)
    ("\\(&&\\)" #Xe131)

    ;; 2+3-Repeats
    ("\\(##\\)"       #Xe11b) ("\\(###\\)"         #Xe11c) ("\\(####\\)" #Xe11d)
    ("\\(--\\)"       #Xe111) ("\\(---\\)"         #Xe112)
    ("\\({-\\)"       #Xe108) ("\\(-}\\)"          #Xe110)
    ("\\(\\\\\\\\\\)" #Xe106) ("\\(\\\\\\\\\\\\\\)" #Xe107)
    ("\\(\\.\\.\\)"   #Xe124) ("\\(\\.\\.\\.\\)"   #Xe126)
    ("\\(\\+\\+\\)"   #Xe138) ("\\(\\+\\+\\+\\)"   #Xe139)
    ("\\(//\\)"       #Xe12f) ("\\(///\\)"         #Xe130)
    ("\\(::\\)"       #Xe10a) ("\\(:::\\)"         #Xe10b)

    ;; ARROWS
    ;; Direct
    ("[^-]\\(->\\)" #Xe114) ("[^=]\\(=>\\)" #Xe13f)
    ("\\(<-\\)"     #Xe152)
    ("\\(-->\\)"    #Xe113) ("\\(->>\\)"    #Xe115)
    ("\\(==>\\)"    #Xe13e) ("\\(=>>\\)"    #Xe140)
    ("\\(<--\\)"    #Xe153) ("\\(<<-\\)"    #Xe15d)
    ("\\(<==\\)"    #Xe158) ("\\(<<=\\)"    #Xe15e)
    ("\\(<->\\)"    #Xe154) ("\\(<=>\\)"    #Xe159)

    ;; Branches
    ("\\(-<\\)"  #Xe116) ("\\(-<<\\)" #Xe117)
    ("\\(>-\\)"  #Xe144) ("\\(>>-\\)" #Xe148)
    ("\\(=<<\\)" #Xe142) ("\\(>>=\\)" #Xe149)
    ("\\(>=>\\)" #Xe146) ("\\(<=<\\)" #Xe15a)

    ;; Squiggly
    ("\\(<~\\)" #Xe160) ("\\(<~~\\)" #Xe161)
    ("\\(~>\\)" #Xe167) ("\\(~~>\\)" #Xe169)
    ("\\(-~\\)" #Xe118) ("\\(~-\\)"  #Xe165)

    ;; MISC
    ("\\(www\\)"                   #Xe100)
    ("\\(<!--\\)"                  #Xe151)
    ("\\(~@\\)"                    #Xe164)
    ("[^<]\\(~~\\)"                #Xe168)
    ("\\(\\?=\\)"                  #Xe127)
    ("[^=]\\(:=\\)"                #Xe10c)
    ("\\(/>\\)"                    #Xe12e)
    ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
    ("[^:=]\\(:\\)[^:=]"           #Xe16c)
    ("\\(<=\\)"                    #Xe157)
  ))

;;;; All-the-icons

(defun module/display/all-the-icons ()
  "Add hylang icon to all-the-icons for neotree and modeline integration."

  ;; Both all-the-icons-icon-alist and all-the-icons-mode-icon-alist
  ;; need to be updated for either modification to take effect.
  (with-eval-after-load 'all-the-icons
    (add-to-list
     'all-the-icons-icon-alist
     '("\\.hy$" all-the-icons-fileicon "lisp" :face all-the-icons-orange))
    (add-to-list
     'all-the-icons-mode-icon-alist
     '(hy-mode all-the-icons-fileicon "lisp" :face all-the-icons-orange))))

;;;; Extra-syntax-highlighting

(defun module/display/extra-syntax-highlighting ()
  "Extra syntax highlighting for desired keywords."

  (defun hy-extra-syntax ()
    (font-lock-add-keywords
     nil '(;; self is not defined by hy-mode as a keyword
         ("\\<\\(self\\)" . 'font-lock-constant-face)

         ;; Highlight entire line for decorators
         ("\\(#@.*$\\)" . 'font-lock-function-name-face)

         ;; Syntax highlighting for reader-macros
         ("\\(#.\\)" . 'font-lock-function-name-face)

         ;; Highlight with macros
         ("\\(with[^ ]*\\)" . 'font-lock-keyword-face)

         ;; Highlight functions after specific macros
         ("\-fixture \\([^ ]*\\)" 1 'font-lock-function-name-face)
         ("\-fixtures \\([^ ]*\\)" 1 'font-lock-function-name-face)

         ;; Fixture macros
         ("\\(deffixture\\)" . 'font-lock-keyword-face)
         ("deffixture \\([^ ]*\\)" 1 'font-lock-function-name-face)

         ;; Asserts
         ("(\\(assert[^ ]*\\)" 1 font-lock-keyword-face)
         )))

  (add-hook 'hy-mode-hook 'hy-extra-syntax))

;;;; Modeline

(defun module/display/modeline ()
  "Minimalistic spaceline-all-the-icons configuration."

  (use-package spaceline-all-the-icons
    :after spaceline  ; eval-after-load doesn't work for this setup
    :config (progn
              ;; Initialization
              (spaceline-all-the-icons--setup-neotree)
              (spaceline-all-the-icons-theme)

              ;; Configuration
              (setq spaceline-highlight-face-func 'spaceline-highlight-face-default
                    spaceline-all-the-icons-icon-set-modified 'chain
                    spaceline-all-the-icons-icon-set-window-numbering 'circle
                    spaceline-all-the-icons-separator-type 'none
                    spaceline-all-the-icons-primary-separator "")

              ;; Toggles
              (spaceline-toggle-all-the-icons-buffer-size-off)
              (spaceline-toggle-all-the-icons-buffer-position-off)
              (spaceline-toggle-all-the-icons-vc-icon-off)
              (spaceline-toggle-all-the-icons-vc-status-off)
              (spaceline-toggle-all-the-icons-git-status-off)
              (spaceline-toggle-all-the-icons-flycheck-status-off)
              (spaceline-toggle-all-the-icons-time-off)
              (spaceline-toggle-all-the-icons-battery-status-off)
              (spaceline-toggle-hud-off))))

;;;; Outline-ellipsis-modification

(defun module/display/outline-ellipsis-modification ()
  "Org-ellipsis but for outline-minor-mode headings"

  (defvar outline-display-table (make-display-table))
  (set-display-table-slot outline-display-table 'selective-display
                          ;; (vector (make-glyph-code ?▼ 'escape-glyph)))
                          (vector (make-glyph-code ? 'escape-glyph)))
  (defun set-outline-display-table ()
    (setf buffer-display-table outline-display-table))

  (add-hook 'outline-mode-hook 'set-outline-display-table)
  (add-hook 'outline-minor-mode-hook 'set-outline-display-table))

;;;; Outline-bullets

(defun module/display/outline-bullets ()
  "Update outline bullets similarly to `org-bullets-bullet-list'."

  (require 'org-bullets)  ; Improve this require

  (set-icon-fonts
   '(("material icons" #xe3d0 #xe3d1 #xe3d2 #xe3d4)))

  (setq org-bullets-bullet-list '("" "" "" ""))
  (setq-default outline-bullets-bullet-list org-bullets-bullet-list)

  (defun font-lock-display-updates (FONT-LOCK-ALIST)
    "Put text property for FONT-LOCK-ALIST for var-width replacements."
    (font-lock-add-keywords
     nil (--map (-let (((rgx uni-point) it))
               `(,rgx (0 (progn
                           (put-text-property
                            (match-beginning 1) (match-end 1)
                            'display
                            ,uni-point)
                           nil))))
             FONT-LOCK-ALIST)))

  (defun outline-bullets-rgx-at-level (LEVEL)
    "Calculate regex or outline-bullets at LEVEL."
    (concat "\\(^"
            (-> LEVEL
               outshine-calc-outline-string-at-level
               s-trim-right)
            "\\) "))

  (defun propertize-bullet (LEVEL BULLET)
    "Add LEVEL-dependent face to BULLET."
    (with-face BULLET
               (pcase LEVEL
                 (0 '(:inherit outline-1 :underline nil))
                 (1 '(:inherit outline-2 :underline nil))
                 (2 '(:inherit outline-3 :underline nil))
                 (3 '(:inherit outline-4 :underline nil))
                 (_ nil))))

  (defun add-outline-font-locks ()
    "Use with `add-hook' to enable outline-bullets-bullet-list for mode."
    (font-lock-display-updates
     (--map-indexed
      (list
       (outline-bullets-rgx-at-level (+ 1 it-index))
       (concat
        (s-repeat it-index " ")
        (propertize-bullet it-index it)))
      (-take 8 (-cycle outline-bullets-bullet-list)))))

  (add-hook 'emacs-lisp-mode-hook 'add-outline-font-locks)
  (add-hook 'hy-mode-hook 'add-outline-font-locks)
  (add-hook 'python-mode-hook 'add-outline-font-locks))

;;;; Prettify-magit

(defun module/display/prettify-magit ()
  "Add faces to Magit manually for things like commit headers eg. (Add: ...).

Adding faces to Magit is non-trivial since any use of font-lock will break
fontification of the buffer. This is due to Magit doing all styling with
`propertize' and black magic. So we apply the faces the manual way.

Adds Ivy integration so a prompt of (Add, Docs, ...) appears when commiting.
Can explore icons by evaluating eg.: (all-the-icons-insert-icons-for 'material)"

  (defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
    "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
    `(progn
       (add-to-list 'pretty-magit-alist
                    (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
                          ,ICON ',PROPS))
       (unless ,NO-PROMPT?
         (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

  (setq pretty-magit-alist nil)
  (setq pretty-magit-prompt nil)
  (pretty-magit "Feature" ? (:foreground "slate gray" :height 1.2))
  (pretty-magit "Add"     ? (:foreground "#375E97" :height 1.2))
  (pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.2))
  (pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.2))
  (pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "master"  ? (:box t :height 1.2) t)
  (pretty-magit "origin"  ? (:box t :height 1.2) t)

  (defun add-magit-faces ()
    "Add face properties and compose symbols for buffer from pretty-magit."
    (interactive)
    (with-silent-modifications
      (--each pretty-magit-alist
        (-let (((rgx icon props) it))
          (save-excursion
            (goto-char (point-min))
            (while (search-forward-regexp rgx nil t)
              (compose-region
               (match-beginning 1) (match-end 1) icon)
              (when props
                (add-face-text-property
                 (match-beginning 1) (match-end 1) props))))))))

  (setq use-magit-commit-prompt-p nil)
  (defun use-magit-commit-prompt (&rest args)
    (setq use-magit-commit-prompt-p t))

  (defun magit-commit-prompt ()
    "Magit prompt and insert commit header with faces."

    (interactive)
    (when use-magit-commit-prompt-p
      (setq use-magit-commit-prompt-p nil)
      (insert (ivy-read "Commit Type " pretty-magit-prompt
                        :require-match t :sort t :preselect "Add: "))
      (add-magit-faces)
      (evil-insert 1)))

  ;; Now due to the delayed use of minibuffer in commit buffers, we cannot
  ;; use add-advice and instead use `git-commit-setup-hook' to run the prompt.
  ;; However, we only want the prompt for c-c `magit-commit' and not its
  ;; variants. The only way to distinguish the calling commit mode is through
  ;; the caller, so we use advice add on `magit-commit' for a prompt predicate.

  (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
  (add-hook 'git-commit-setup-hook 'magit-commit-prompt)

  (advice-add 'magit-status :after 'add-magit-faces)
  (advice-add 'magit-refresh-buffer :after 'add-magit-faces)
  (advice-add 'magit-commit :after 'use-magit-commit-prompt))

;;;; Prettify-symbols

(defun module/display/prettify-symbols ()
  "Visually replace text with unicode.

Ivy keybinding has 'SPC i u' for consel-unicode-char for exploring options."

  (setq pretty-options
        (-flatten
         (prettify-utils-generate
          ;;;;; Functional
          (:lambda      "λ") (:def         "ƒ")
          (:composition "∘")

          ;;;;; Types
          (:null        "∅") (:true        "𝕋") (:false       "𝔽")
          (:int         "ℤ") (:float       "ℝ")
          (:str         "𝕊") (:bool        "𝔹")

          ;;;;; Flow
          (:in          "∈") (:not-in      "∉")
          (:return     "⟼") (:yield      "⟻")
          (:and         "∧") (:or          "∨")
          (:not         "￢")
          (:for         "∀")
          (:some        "∃")

          ;;;;; Other
          (:tuple       "⨂")
          (:pipe        "")
          )))

  (defun get-pretty-pairs (KWDS)
    "Utility to build an alist for prettify-symbols-alist from components.

KWDS is a plist of pretty option and the text to be replaced for it."
    (-non-nil
     (--map (when-let (major-mode-sym (plist-get KWDS it))
             `(,major-mode-sym
               ,(plist-get pretty-options it)))
           pretty-options)))

  (setq hy-pretty-pairs
        (append
         (get-pretty-pairs
          '(:lambda "fn" :def "defn"
                    :composition "comp"
                    :null "None" :true "True" :false "False"
                    :in "in" :not "not"
                    :and "and" :or "or"
                    :some "some"
                    :tuple "#t"
                    :pipe "ap-pipe"
                    ))
         (prettify-utils-generate
          ("#l"    " ")))

        python-pretty-pairs
        (append
         (get-pretty-pairs
          '(:lambda "lambda" :def "def"
                    :null "None" :true "True" :false "False"
                    :int "int" :float "float" :str "str" :bool "bool"
                    :not "not" :for "for" :in "in" :not-in "not in"
                    :return "return" :yield "yield"
                    :and "and" :or "or"
                    :tuple "Tuple"
                    :pipe "tz-pipe"
                    ))
         (prettify-utils-generate
          ;; Mypy Stuff
          ("Dict"     "𝔇") ("List"     "ℒ")
          ("Callable" "ℱ") ("Mapping"  "ℳ") ("Iterable" "𝔗")
          ("Union"    "⋃") ("Any"      "❔")))
        )

  (defun set-pretty-pairs (HOOK-PAIRS-ALIST)
    "Utility to set pretty pairs for many modes.

MODE-HOOK-PAIRS-ALIST is an alist of the mode hoook and its pretty pairs."
    (mapc (lambda (x)
            (lexical-let ((pretty-pairs (cadr x)))
              (add-hook (car x)
                        (lambda ()
                          (setq prettify-symbols-alist pretty-pairs)))))
          HOOK-PAIRS-ALIST))

  (set-pretty-pairs `((hy-mode-hook     ,hy-pretty-pairs)
                      (python-mode-hook ,python-pretty-pairs)))

  (global-prettify-symbols-mode 1)
  (global-pretty-mode t)

  ;; Activate pretty groups
  (pretty-activate-groups
   '(:arithmetic-nary :greek))

  ;; Deactivate pretty groups conflicting with Fira Code ligatures
  (pretty-deactivate-groups  ; Replaced by Fira Code
   '(:equality :ordering :ordering-double :ordering-triple
               :arrows :arrows-twoheaded :punctuation
               :logic :sets :sub-and-superscripts)))

;;;; Shell

(defun module/display/shell ()
  "Eshell prettification."

  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))

  (defmacro esh-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,NAME
           (lambda () (when ,FORM
                   (-> ,ICON
                      (concat esh-section-delim ,FORM)
                      (with-face ,@PROPS))))))

  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))

  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))

  (esh-section esh-dir
               "\xf07c"  ; 
               (abbreviate-file-name (eshell/pwd))
               '(:foreground "gold" :bold ultra-bold :underline t))

  (esh-section esh-git
               "\xe907"  ; 
               (magit-get-current-branch)
               '(:foreground "pink"))

  (esh-section esh-python
               "\xe928"  ; 
               pyvenv-virtual-env-name)

  (esh-section esh-clock
               "\xf017"  ; 
               (format-time-string "%H:%M" (current-time))
               '(:foreground "forest green"))

  (setq esh-prompt-num 0)
  (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))

  (esh-section esh-num
               "\xf0c9"  ; 
               (number-to-string esh-prompt-num)
               '(:foreground "brown"))

  (setq esh-sep "  ")
  (setq esh-section-delim " ")
  (setq esh-header "\n ")
  (setq eshell-prompt-regexp " ")
  (setq eshell-prompt-string " ")
  (setq eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num))

  (setq eshell-prompt-function 'esh-prompt-func)
  )

;;;; Theme-updates

(defun module/display/theme-updates ()
  "Face configuration for themes, atm solarized-light."

  (defun update-solarize-dark ()
    (setq org-src-block-faces `(("python" (:background "#073642"))))

    (custom-theme-set-faces
     'solarized-dark

     ;; Italicized faces for Operator Mono font
     `(font-lock-comment-face ((t (:foreground "#586e75" :italic t))))
     `(avy-background-face ((t (:foreground "#586e75"))))
     `(font-lock-doc-face ((t (:foreground "#2aa198"
                               :italic t))))

     ;; Makes matching parens obvious
     `(sp-show-pair-match-face ((t (:inherit sp-show-pair-match-face
                                             :background "#586e75"))))

     ;; active modeline has no colors
     `(mode-line ((t (:inherit mode-line :background "#002b36"))))
     `(mode-line-inactive ((t (:inherit mode-line :background "#002b36"))))
     `(spaceline-highlight-face ((t (:inherit mode-line :background "#002b36"))))
     `(powerline-active1 ((t (:inherit mode-line :background "#002b36"))))
     `(powerline-active2 ((t (:inherit mode-line :background "#002b36"))))

     ;; Inactive modeline has tint
     `(powerline-inactive2 ((t (:inherit powerline-inactive1))))

     ;; Org and outline header updates
     `(outline-1 ((t (:height 1.25
                              :foreground "#C3A29E"
                              :weight ultra-bold
                              :italic t
                              :underline t))))

     `(outline-2 ((t (:height 1.15
                              :foreground "#8D6B94"
                              :weight extra-bold
                              :italic t
                              :underline t))))

     `(outline-3 ((t (:height 1.15
                              :foreground "#8C5F66"
                              :weight bold
                              :italic t
                              :underline t))))

     `(org-level-1 ((t (:height 1.25 :foreground "#268bd2"
                                :underline t
                                :weight ultra-bold))))
     `(org-level-2 ((t (:height 1.15 :foreground "#2aa198"
                                :underline t
                                :weight ultra-bold))))
     `(org-level-3 ((t (:height 1.05 :foreground "#b58900"
                                :underline t
                                :weight ultra-bold))))

     ;; #586e75
     `(org-block-begin-line ((t (:height 1.05 :foreground "#576e75"
                                         :box t :weight bold))))
     )

    (setq my-black "#1b1b1e"))

  (defun update-solarize-light ()
    (custom-theme-set-faces
     'solarized-light

     ;; Makes matching parens obvious
     `(sp-show-pair-match-face ((t (:inherit sp-show-pair-match-face
                                             :background "light gray"))))

     ;; active modeline has no colors
     `(mode-line ((t (:inherit mode-line :background "#fdf6e3"))))
     `(mode-line-inactive ((t (:inherit mode-line :background "#fdf6e3"))))
     `(spaceline-highlight-face ((t (:inherit mode-line :background "#fdf6e3"))))
     `(powerline-active1 ((t (:inherit mode-line :background "#fdf6e3"))))
     `(powerline-active2 ((t (:inherit mode-line :background "#fdf6e3"))))

     ;; Inactive modeline has tint
     `(powerline-inactive2 ((t (:inherit powerline-inactive1))))

     ;; Org and outline header updates
     `(org-level-1 ((t (:height 1.25 :foreground ,my-black
                                :background "#C9DAEA"
                                :weight bold))))
     `(org-level-2 ((t (:height 1.15 :foreground ,my-black
                                :background "#7CDF64"
                                :weight bold))))
     `(org-level-3 ((t (:height 1.05 :foreground ,my-black
                                :background "#F8DE7E"
                                :weight bold))))

     '(outline-1 ((t (:inherit org-level-1))))
     '(outline-2 ((t (:inherit org-level-2))))
     '(outline-3 ((t (:inherit org-level-3))))
     '(outline-4 ((t (:inherit org-level-4))))

     `(org-todo ((t (:foreground ,my-black :weight extra-bold
                                 :background "light gray"))))
     `(org-priority ((t (:foreground ,my-black :weight bold
                                     :background "light gray"))))
     ))

  (if (string= 'solarized-dark (car custom-enabled-themes))
      (update-solarize-dark)
    (update-solarize-light)))

;;; Ivy

(defun module/ivy ()
  "Ivy completion framework configuration."

  (defun ivy-file-transformer-fixed-for-files (s)
    "Gets file icon for string, fixing bug for folders and windows box."
    (format "%s\t%s"
            (if (and is-linuxp (s-ends-with? "/" s))
                (propertize "\t" 'display "" 'face 'all-the-icons-silver)
              (propertize "\t" 'display (all-the-icons-icon-for-file s)))
            s))

  (advice-add 'all-the-icons-ivy-file-transformer
              :override 'ivy-file-transformer-fixed-for-files)

  (all-the-icons-ivy-setup)

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
  (when-linux-call 'module/misc/spotify)
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

;;;; Spotify

(defun module/misc/spotify ()
  "Spotify-plus bindings."

  ;; TODO must overwrite navi mode for M-s s
  (global-set-key (kbd "M-s s") 'helm-spotify-plus)
  (global-set-key (kbd "M-s j") 'helm-spotify-plus-play)
  (global-set-key (kbd "M-s SPC") 'helm-spotify-plus-pause)
  (global-set-key (kbd "M-s l") 'helm-spotify-plus-next)
  (global-set-key (kbd "M-s h") 'helm-spotify-plus-previous))

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
                               (http .    t)  ; Requests
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

  ;; TODO setup dropbox daemon on linux, try calfw, bind stuff
  (require 'org-gcal)
  (require 'org-contacts)
  (load (if-linux "~/Dropbox/secrets.el"
                  "c:/~/Dropbox/secrets.el") t)
  (setq org-gcal-file-alist
        `(("ekaschalk@gmail.com" .
           ,(if-linux "~/Dropbox/schedule.org" "c:/~/Dropbox/schedule.org"))))
  (setq org-contacts-files
        `(,(if-linux "~/Dropbox/contacts.org" "c:/~/Dropbox/contacts.org")))
  (setq org-agenda-files
        `(,(if-linux "~/Dropbox/schedule.org" "c:/~/Dropbox/schedule.org"))))

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

  (setq org-priority-faces '((65 :inherit org-priority :foreground "red")
                             (66 :inherit org-priority :foreground "brown")
                             (67 :inherit org-priority :foreground "blue"))
        org-ellipsis "▼"))

;;; Outshine

(defun module/outshine ()
  "Outline/Outshine mode bindings and Navi integration."

  (require 'outshine)

  ;; Narrowing now works within the headline rather than requiring to be on it
  (advice-add 'outshine-narrow-to-subtree :before
              (lambda (&rest args) (unless (outline-on-heading-p t)
                                (outline-previous-visible-heading 1))))

  (spacemacs/set-leader-keys
    ;; Narrowing
    "nn" 'outshine-narrow-to-subtree
    "nw" 'widen

    ;; Structural edits
    "nj" 'outline-move-subtree-down
    "nk" 'outline-move-subtree-up
    "nh" 'outline-promote
    "nl" 'outline-demote)

  (let ((kmap outline-minor-mode-map))
    (define-key kmap (kbd "M-RET") 'outshine-insert-heading)
    (define-key kmap (kbd "<backtab>") 'outshine-cycle-buffer)

    ;; Evil outline navigation keybindings
    (evil-define-key '(normal visual motion) kmap
      "gh" 'outline-up-heading
      "gj" 'outline-forward-same-level
      "gk" 'outline-backward-same-level
      "gl" 'outline-next-visible-heading
      "gu" 'outline-previous-visible-heading))

  ;; Required for outshine
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  ;; Enables outline-minor-mode for *ALL* programming buffers!
  (add-hook 'prog-mode-hook 'outline-minor-mode))

;;; Python

(defun module/python ()
  (require 'python)
  (unless-linux-call 'module/python/windows-pytest)
  (module/python/fixes)
  (module/python/hy)
  (module/python/mypy)
  (module/python/venvs))

;;;; Windows-pytest

(defun module/python/windows-pytest ()
  "Pytest is broken on Windows. Basic functionality is provided for Windows."

  (defun ek-pytest-module ()
    (interactive)
    (shell-command (format "py.test -x -s %s&" buffer-file-name)))

  (defun ek-pytest-one ()
    (interactive)
    (save-excursion
      (let ((test-name
             (progn
               (re-search-backward "^[ ]*def \\(test_[a-zA-Z0-9_]*\\)")
               (match-string 1))))
        (shell-command
         (format "py.test -x -s %s::%s&" buffer-file-name test-name)))))

  (spacemacs/set-leader-keys-for-major-mode
    'python-mode (kbd "t m") 'ek-pytest-module)
  (spacemacs/set-leader-keys-for-major-mode
    'python-mode (kbd "t t") 'ek-pytest-one))

;;;; Fixes

(defun module/python/fixes ()
  "Various python bugfixes."

  ;; Sometimes ipython shells trigger a bad error to popup
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_")))

  ;; No log output in pytests
  ;; (setq pytest-cmd-flags "-x --no-print-logs")
  (setq pytest-cmd-flags "-x -s")

  ;; Remove flyspell from python buffers
  (dolist (hook '(python-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1)))))

;;;; Hy

(defun module/python/hy ()
  "Hylang integration improvements."

  (defun hy-insert-pdb ()
    (interactive)
    (insert "(do (import pdb) (pdb.set-trace))"))

  (defun hy-insert-thread-pdb ()
    (interactive)
    (insert "((tz.do (do (import pdb) (pdb.set-trace))))"))

  (spacemacs/set-leader-keys-for-major-mode
    'hy-mode (kbd "dd") 'hy-insert-pdb)
  (spacemacs/set-leader-keys-for-major-mode
    'hy-mode (kbd "dt") 'hy-insert-thread-pdb))

;;;; Mypy

(defun module/python/mypy ()
  "Enable mypy flycheck integration in-tandem with pylint."

  (flycheck-define-checker
      python-mypy ""
      :command ("mypy"
                "--ignore-missing-imports" "--fast-parser"
                "--python-version" "3.6"
                source-original)
      :error-patterns
      ((error line-start (file-name) ":" line ": error:" (message) line-end))
      :modes python-mode)

  (add-to-list 'flycheck-checkers 'python-mypy t)
  (flycheck-add-next-checker 'python-pylint 'python-mypy t))

;;;; Venvs

(defun module/python/venvs ()
  "Initialize virtual environment management for Python."

  (require 'virtualenvwrapper)
  (pyvenv-mode 1)

  ;; Fixes hy-mode environment when pyvenv is activated
  (add-hook 'pyvenv-post-activate-hooks 'python/init-hy-mode)

  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

;;; Blog

(defun module/blog ()
  "Hugo blog utilities. Hosted at https://ekaschalk.github.io."

  (setq blog-dir (if is-linuxp
                     "~/dev/blog" "c:/~/dev/blog")
        public-blog-dir (if is-linuxp
                            "~/dev/public-blog" "c:/~/dev/public-blog")
        hugo-process "Hugo Server"
        hugo-server-site "http://localhost:1313/")

  (defmacro with-dir (DIR &rest FORMS)
    "Execute FORMS in DIR."
    (let ((orig-dir (gensym)))
      `(progn (setq ,orig-dir default-directory)
              (cd ,DIR) ,@FORMS (cd ,orig-dir))))

  (defun deploy-blog ()
    "Run hugo and push changes upstream."
    (interactive)
    (with-dir public-blog-dir
              (shell-command "git rm -rf .")
              (shell-command "git clean -fxd")
              (with-temp-file "CNAME"
                (insert "www.modernemacs.com\nmodernemacs.com"))

              (with-dir blog-dir (->> public-blog-dir
                                    (concat "hugo -d ")
                                    shell-command))

              (shell-command "git add .")
              (--> (current-time-string)
                 (concat "git commit -m \"" it "\"")
                 (shell-command it))
              (magit-push-current-to-upstream nil)))

  (defun start-blog-server ()
    "Run hugo server if not already running and open its webpage."
    (interactive)
    (with-dir blog-dir
              (unless (get-process hugo-process)
                (start-process hugo-process nil "hugo" "server"))
              (browse-url hugo-server-site)))

  (defun end-blog-server ()
    "End hugo server process if running."
    (interactive)
    (--when-let (get-process hugo-process)
      (delete-process it)))

  (spacemacs/set-leader-keys (kbd "ab") 'deploy-blog)
  (spacemacs/set-leader-keys (kbd "aa") 'start-blog-server)
  (spacemacs/set-leader-keys (kbd "ae") 'end-blog-server))
