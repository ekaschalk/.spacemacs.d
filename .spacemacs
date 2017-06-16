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
;;
;; Organization
;; ---------
;; Configuration is grouped by theme. The current groups are:
;; Display - Ivy - Configuration - Misc - Navigation - Python - Org - Outshine
;;
;; Each group is broken into further components for targetted enabling/disabling
;; Some groups require a specific execution ordering. Ordering requirements are
;; specifed with Group x comments. Within the group, the packages are lexical.

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

        ;; Visual Enhancements
        all-the-icons-ivy        ; Ivy prompts use file icons
        pretty-mode              ; Adds onto prettify-mode
        spaceline-all-the-icons  ; Spaceline integrates file and other icons
        (prettify-utils          ; Useful add pretty symbols macro
         :location (recipe :fetcher github
                           :repo "Ilazki/prettify-utils.el"))

        ;; Themes
        solarized-theme
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
                          20)
                       'solarized-light
                     'solarized-dark))

;;;; Configuration

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-themes `(,theme-to-use)
   dotspacemacs-default-font `("Hack"
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
  ;; Group 1
  (module/display)

  ;; Rest
  (module/configuration)
  (module/ivy)
  (module/misc)
  (module/navigation)
  (module/org)
  (module/outshine)
  (module/python)

  ;; Personal use
  (module/blog))

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
  (module/display/prettify-magit)
  (module/display/prettify-symbols)
  (module/display/shell)
  (module/display/theme-updates))

;;;; Windows-frame-size-fix

(defun module/display/windows-frame-size-fix ()
  "Surface has 200% scaling, doesn't apply to emacs, fixes with push of `f2`."

  (add-to-list 'default-frame-alist '(font . "Hack"))
  (set-face-attribute 'default t :font "Hack")
  (global-set-key (kbd "<f2>")
                  (lambda () (interactive) (mapc (lambda (x) (zoom-frm-out)) '(1 2)))))

;;;; Fontsets

(defun module/display/fontsets ()
  "Set right fonts for missing and all-the-icons unicode points."

  ;; Fira code ligatures. Fira Code Symbol is a different font than Fira Code!
  ;; You can use any font you wish with just the ligatures, I use Hack.
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

  (defun set-icon-fonts (CODE-FONT-ALIST)
    "Utility to associate unicode points with a chosen font.

CODE-FONT-ALIST is an alist of a font and unicode points to force to use it."
    (mapc (lambda (x)
            (let ((font (car x))
                  (codes (cdr x)))
              (mapc (lambda (code)
                      (set-fontset-font t `(,code . ,code) font))
                    codes)))
          CODE-FONT-ALIST))

  ;; NOTE The icons you see are not the correct icons until this is evaluated
  (set-icon-fonts
   '(("fontawesome"
      ;;              
      #xf07c #xf0c9 #xf0c4 #xf0cb)

     ("all-the-icons"
      ;;    
      #xe907 #xe928)

     ("github-octicons"
      ;;                          
      #xf091 #xf059 #xf076 #xf075 #xe192  #xf016)

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
    "Add unicode font lock replacements.

FONT-LOCK-ALIST is an alist of a regexp and the unicode point to replace with.
Used as: (add-hook 'a-mode-hook (-partial '-add-font-lock-kwds the-alist))"
    (defun -build-font-lock-alist (REGEX-CHAR-PAIR)
      "Compose region for each REGEX-CHAR-PAIR in FONT-LOCK-ALIST."
      `(,(car REGEX-CHAR-PAIR)
        (0 (prog1 ()
             (compose-region
              (match-beginning 1)
              (match-end 1)
              ,(concat "	"
                       (list (cadr REGEX-CHAR-PAIR))))))))
    (font-lock-add-keywords nil (mapcar '-build-font-lock-alist FONT-LOCK-ALIST)))

  (defun add-font-locks (FONT-LOCK-HOOKS-ALIST)
    "Utility to add font lock alist to many hooks.

FONT-LOCK-HOOKS-ALIST is an alist of a font-lock-alist and its desired hooks."
    (mapc (lambda (x)
            (lexical-let ((font-lock-alist (car x))
                          (mode-hooks (cdr x)))
              (mapc (lambda (mode-hook)
                      (add-hook mode-hook
                                (-partial '-add-font-lock-kwds font-lock-alist)))
                    mode-hooks)))
          FONT-LOCK-HOOKS-ALIST))

  (require 'navi-mode)  ; TODO handle this require better for the navi font-locks
  (add-font-locks
   `((,fira-font-lock-alist        prog-mode-hook  org-mode-hook)
     (,python-font-lock-alist      python-mode-hook)
     (,emacs-lisp-font-lock-alist  emacs-lisp-mode-hook)
     (,hy-font-lock-alist          hy-mode-hook)
     (,navi-font-lock-alist        navi-mode-hook)
     )))

;;;;; Fira-font-locks

(defconst fira-font-lock-alist
  '(;;;; OPERATORS
    ;;;;; Pipes
    ("\\(<|\\)" #Xe14d) ("\\(<>\\)" #Xe15b) ("\\(<|>\\)" #Xe14e) ("\\(|>\\)" #Xe135)

    ;;;;; Brackets
    ("\\(<\\*\\)" #Xe14b) ("\\(<\\*>\\)" #Xe14c) ("\\(\\*>\\)" #Xe104)
    ("\\(<\\$\\)" #Xe14f) ("\\(<\\$>\\)" #Xe150) ("\\(\\$>\\)" #Xe137)
    ("\\(<\\+\\)" #Xe155) ("\\(<\\+>\\)" #Xe156) ("\\(\\+>\\)" #Xe13a)

    ;;;;; Equality
    ("\\(!=\\)" #Xe10e) ("\\(!==\\)"         #Xe10f) ("\\(=/=\\)" #Xe143)
    ("\\(/=\\)" #Xe12c) ("\\(/==\\)"         #Xe12d)
    ("\\(===\\)"#Xe13d) ("[^!/]\\(==\\)[^>]" #Xe13c)

    ;;;;; Equality Special
    ("\\(||=\\)"  #Xe133) ("[^|]\\(|=\\)" #Xe134)
    ("\\(~=\\)"   #Xe166)
    ("\\(\\^=\\)" #Xe136)
    ("\\(=:=\\)"  #Xe13b)

    ;;;;; Comparisons
    ("\\(<=\\)" #Xe141) ("\\(>=\\)" #Xe145)
    ("\\(</\\)" #Xe162) ("\\(</>\\)" #Xe163)

    ;;;;; Shifts
    ("[^-=]\\(>>\\)" #Xe147) ("\\(>>>\\)" #Xe14a)
    ("[^-=]\\(<<\\)" #Xe15c) ("\\(<<<\\)" #Xe15f)

    ;;;;; Dots
    ("\\(\\.-\\)"    #Xe122) ("\\(\\.=\\)" #Xe123)
    ("\\(\\.\\.<\\)" #Xe125)

    ;;;;; Hashes
    ("\\(#{\\)"  #Xe119) ("\\(#(\\)"   #Xe11e) ("\\(#_\\)"   #Xe120)
    ("\\(#_(\\)" #Xe121) ("\\(#\\?\\)" #Xe11f) ("\\(#\\[\\)" #Xe11a)

    ;;;; REPEATED CHARACTERS
    ;;;;; 2-Repeats
    ("\\(||\\)" #Xe132)
    ("\\(!!\\)" #Xe10d)
    ("\\(%%\\)" #Xe16a)
    ("\\(&&\\)" #Xe131)

    ;;;;; 2+3-Repeats
    ("\\(##\\)"       #Xe11b) ("\\(###\\)"         #Xe11c) ("\\(####\\)" #Xe11d)
    ("\\(--\\)"       #Xe111) ("\\(---\\)"         #Xe112)
    ("\\({-\\)"       #Xe108) ("\\(-}\\)"          #Xe110)
    ("\\(\\\\\\\\\\)" #Xe106) ("\\(\\\\\\\\\\\\\\)" #Xe107)
    ("\\(\\.\\.\\)"   #Xe124) ("\\(\\.\\.\\.\\)"   #Xe126)
    ("\\(\\+\\+\\)"   #Xe138) ("\\(\\+\\+\\+\\)"   #Xe139)
    ("\\(//\\)"       #Xe12f) ("\\(///\\)"         #Xe130)
    ("\\(::\\)"       #Xe10a) ("\\(:::\\)"         #Xe10b)

    ;;;; ARROWS
    ;;;;; Direct
    ("[^-]\\(->\\)" #Xe114) ("[^=]\\(=>\\)" #Xe13f)
    ("\\(<-\\)"     #Xe152)
    ("\\(-->\\)"    #Xe113) ("\\(->>\\)"    #Xe115)
    ("\\(==>\\)"    #Xe13e) ("\\(=>>\\)"    #Xe140)
    ("\\(<--\\)"    #Xe153) ("\\(<<-\\)"    #Xe15d)
    ("\\(<==\\)"    #Xe158) ("\\(<<=\\)"    #Xe15e)
    ("\\(<->\\)"    #Xe154) ("\\(<=>\\)"    #Xe159)

    ;;;;; Branches
    ("\\(-<\\)"  #Xe116) ("\\(-<<\\)" #Xe117)
    ("\\(>-\\)"  #Xe144) ("\\(>>-\\)" #Xe148)
    ("\\(=<<\\)" #Xe142) ("\\(>>=\\)" #Xe149)
    ("\\(>=>\\)" #Xe146) ("\\(<=<\\)" #Xe15a)

    ;;;;; Squiggly
    ("\\(<~\\)" #Xe160) ("\\(<~~\\)" #Xe161)
    ("\\(~>\\)" #Xe167) ("\\(~~>\\)" #Xe169)
    ("\\(-~\\)" #Xe118) ("\\(~-\\)"  #Xe165)

    ;;;; MISC
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

;;;;; Language-font-locks

(defconst emacs-lisp-font-lock-alist
  ;; Outlines not using * so better overlap with in-the-wild packages.
  ;; Intentionally not requiring BOL for eg. fira config modularization
  '(("\\(^;;;\\)"                   ?■)
    ("\\(^;;;;\\)"                  ?○)
    ("\\(^;;;;;\\)"                 ?✸)
    ("\\(^;;;;;;\\)"                ?✿)))

(defconst navi-font-lock-alist
  ;; TODO ideally this would be major-mode specific, atm elisp
  '(;; Outlines
    ;; Hides numbers (numbers still needed for internal navi methods)
    ("\\([ ]+[0-9]+:;;;\\) "                   ?■)
    ("\\([ ]+[0-9]+:;;;;\\) "                  ?○)
    ("\\([ ]+[0-9]+:;;;;;\\) "                 ?✸)
    ("\\([ ]+[0-9]+:;;;;;;\\) "                ?✿)

    ;; Hide first line
    ("\\(.*matches.*$\\)"            ? )
    ))

(defconst python-font-lock-alist
  ;; Outlines
  '(("\\(^# \\*\\)[ \t\n]"          ?■)
    ("\\(^# \\*\\*\\)[ \t\n]"       ?○)
    ("\\(^# \\*\\*\\*\\)[ \t\n]"    ?✸)
    ("\\(^# \\*\\*\\*\\*\\)[^\\*]"  ?✿)))

(defconst hy-font-lock-alist
  ;; Outlines
  '(("\\(^;; \\*\\)[ \t\n]"          ?■)
    ("\\(^;; \\*\\*\\)[ \t\n]"       ?○)
    ("\\(^;; \\*\\*\\*\\)[ \t\n]"    ?✸)
    ("\\(^;; \\*\\*\\*\\*\\)[^\\*]"  ?✿)

    ;; self does not work as a prettify symbol for hy, unlike python
    ("\\(self\\)"   ?⊙)))


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
         ("\\(#.\\)" . 'font-lock-function-name-face))))

  (defun navi-extra-syntax ()
    (font-lock-add-keywords
     nil '(("\\([ ]+[0-9]+:;;;\\) .*$" .    'org-level-1)
         ("\\([ ]+[0-9]+:;;;;\\) .*$" .   'org-level-2)
         ("\\([ ]+[0-9]+:;;;;;\\) .*$" .  'org-level-3)
         ("\\([ ]+[0-9]+:;;;;;\\) .*$" .  'org-level-4))))

  (add-hook 'hy-mode-hook 'hy-extra-syntax)
  (add-hook 'navi-mode-hook 'navi-extra-syntax))

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
                    powerline-default-separator 'bar
                    spaceline-all-the-icons-icon-set-modified 'chain
                    spaceline-all-the-icons-icon-set-window-numbering 'circle
                    spaceline-all-the-icons-separators-type 'arrow
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
                          (vector (make-glyph-code ?▼ 'escape-glyph)))
  (defun set-outline-display-table ()
    (setf buffer-display-table outline-display-table))

  (add-hook 'outline-mode-hook 'set-outline-display-table)
  (add-hook 'outline-minor-mode-hook 'set-outline-display-table))

;;;; Prettify-magit

(defun module/display/prettify-magit ()
  "Add faces to Magit manually for things like commit headers eg. (Add: ...).

Adding faces to Magit is non-trivial since any use of font-lock will break
fontification of the buffer. This is due to Magit doing all styling with
`propertize' and black magic. So we apply the faces the manual way.

Adds Ivy integration so a prompt of (Add, Docs, ...) appears when commiting.
Can explore icons by evaluating eg.: (all-the-icons-insert-icons-for 'material)
"

  (setq my-magit-colors '(:feature "silver"
                          :fix "#FB6542"    ; sunset
                          :add "#375E97"    ; sky
                          :clean "#FFBB00"  ; sunflower
                          :docs "#3F681C"   ; grass
                          ))

  (defface my-magit-base-face
    `((t :weight bold  :height 1.2))
    "Base face for magit commit headers."
    :group 'magit-faces)

  (defface my-magit-feature-face
    `((t :foreground ,(plist-get my-magit-colors :feature)
         :inherit my-magit-base-face))
    "Feature commit header face.")

  (defface my-magit-fix-face
    `((t :foreground ,(plist-get my-magit-colors :fix)
         :inherit my-magit-base-face))
    "Fix commit header face.")

  (defface my-magit-add-face
    `((t :foreground ,(plist-get my-magit-colors :add)
         :inherit my-magit-base-face))
    "Add commit header face.")

  (defface my-magit-clean-face
    `((t :foreground ,(plist-get my-magit-colors :clean)
         :inherit my-magit-base-face))
    "Clean commit header face.")

  (defface my-magit-docs-face
    `((t :foreground ,(plist-get my-magit-colors :docs)
         :inherit my-magit-base-face))
    "Docs commit header face.")

  (defface my-magit-master-face
    `((t :box t
         :inherit my-magit-base-face))
    "Docs commit header face.")

  (defface my-magit-origin-face
    `((t :box t
         :inherit my-magit-base-face))
    "Docs commit header face.")

  (setq pretty-magit-faces '(("\\<\\(Feature:\\)"         'my-magit-feature-face)
                             ("\\<\\(Add:\\)"             'my-magit-add-face)
                             ("\\<\\(Fix:\\)"             'my-magit-fix-face)
                             ("\\<\\(Clean:\\)"           'my-magit-clean-face)
                             ("\\<\\(Docs:\\)"            'my-magit-docs-face)
                             ("\\<\\(master\\)\\>"        'my-magit-master-face)
                             ("\\<\\(origin/master\\)\\>" 'my-magit-origin-face))

        pretty-magit-symbols '(("\\<\\(Feature:\\)"      ?)
                               ("\\<\\(Add:\\)"          ?)
                               ("\\<\\(Fix:\\)"          ?)
                               ("\\<\\(Clean:\\)"        ?)
                               ("\\<\\(Docs:\\)"         ?)
                               ("\\<\\(master\\)\\>"     ?)
                               ("\\<\\(origin/master\\)" ?)))

  (defun add-magit-faces ()
    "Apply `pretty-magit-faces' and `pretty-magit-symbols' to magit buffers."
    (interactive)
    (with-silent-modifications
      (--each pretty-magit-faces
        (save-excursion
          (evil-goto-first-line)
          (while (search-forward-regexp (car it) nil t)
            (add-face-text-property
             (match-beginning 1) (match-end 1) (cdr it)))))
      (--each pretty-magit-symbols
        (save-excursion
          (evil-goto-first-line)
          (while (search-forward-regexp (car it) nil t)
            (compose-region
             (match-beginning 1) (match-end 1) (cdr it)))))))

  (setq use-magit-commit-prompt-p nil)
  (defun use-magit-commit-prompt (&rest args)
    (setq use-magit-commit-prompt-p t))

  (defun magit-commit-prompt ()
    "Magit prompt and insert commit header with faces."
    (interactive)
    (when use-magit-commit-prompt-p
      (setq use-magit-commit-prompt-p nil)
      (insert (ivy-read "Commit Type "
                        '("Feature: " "Add: " "Fix: " "Clean: " "Docs: ")
                        :require-match t
                        :sort t
                        :preselect "Add: "))
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
        (get-pretty-pairs
         '(:lambda "fn" :def "defn"
                   :composition "comp"
                   :null "None" :true "True" :false "False"
                   :in "in" :not "not"
                   :some "some"
                   :tuple "#t"
                   :pipe "ap-pipe"
                   ))

        python-pretty-pairs
        (append
         (get-pretty-pairs
          '(:lambda "lambda" :def "def"
                    :null "None" :true "True" :false "False"
                    :int "int" :float "float" :str "str" :bool "bool"
                    :not "not" :for "for" :in "in" :not-in "not in"
                    :return "return" :yield "yield"
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

  (setq eshell-prompt-number 0)
  (add-hook 'eshell-exit-hook (lambda () (setq eshell-prompt-number 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args)
                (setq eshell-prompt-number (+ 1 eshell-prompt-number))))

  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))

  (defun set-eshell-prompt-icon (ICON PROPS)
    "Update eshell prompt with ICON propertized with PROPS."
    (let ((prompt (concat ICON " ")))
      (setq eshell-prompt-regexp prompt)
      (concat "\n" (with-face prompt PROPS))))

  (defun eshell-section (ICON STR &rest PROPS)
    "Return eshell section string with ICON header for STR with PROPS."
    (when STR
      (with-face (concat ICON " " STR) PROPS)))

  (defun esh-prompt-function ()
    "Custom `eshell-prompt-function'."
    (let* ((esh-header "\n ")
           (esh-header-face nil)
           (esh-prompt "")
           (esh-prompt-face nil)
           (esh-sep " | ")
           (esh-sep-face nil)

           (esh-dir-section (abbreviate-file-name (eshell/pwd)))
           (esh-dir-face nil)

           (esh-git-section (magit-get-current-branch))
           (esh-git-face nil)

           (esh-venv-section pyvenv-virtual-env-name)
           (esh-venv-face nil)

           (esh-time-section (format-time-string "%H:%M" (current-time)))
           (esh-time-face nil)

           (esh-prompt-num-section (number-to-string eshell-prompt-number))
           (esh-prompt-num-face nil)

           (esh-sections (list
                          (eshell-section "" esh-dir-section esh-dir-face)
                          (eshell-section "" esh-git-section esh-git-face)
                          (eshell-section "" esh-venv-section esh-venv-face)
                          (eshell-section "" esh-time-section esh-time-face)
                          (eshell-section "" esh-prompt-num-section esh-prompt-num-face))))
      (concat
       (with-face esh-header esh-header-face)
       (s-join (with-face esh-sep esh-sep-face)
               (-non-nil esh-sections))
       (set-eshell-prompt-icon esh-prompt esh-prompt-face))))

  (setq eshell-prompt-function 'esh-prompt-function))

;;;; Theme-updates

(defun module/display/theme-updates ()
  "Face configuration for themes, atm solarized-light."

  (defun update-solarize-dark ()
    (custom-theme-set-faces
     'solarized-dark

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
     `(org-level-1 ((t (:height 1.25 :foreground ,my-black
                                :background "#268bd2"
                                :weight bold))))
     `(org-level-2 ((t (:height 1.15 :foreground ,my-black
                                :background "#2aa198"
                                :weight bold))))
     `(org-level-3 ((t (:height 1.05 :foreground ,my-black
                                :background "#b58900"
                                :weight bold))))

     '(outline-1 ((t (:inherit org-level-1))))
     '(outline-2 ((t (:inherit org-level-2))))
     '(outline-3 ((t (:inherit org-level-3))))
     '(outline-4 ((t (:inherit org-level-4))))
     ))

  (setq my-black "#1b1b1e")

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

  (require 'avy)  ; TODO must require for to get avy--generic-jump loaded

  (global-set-key (kbd "C-h") 'avy-pop-mark)
  (global-set-key (kbd "C-j") 'evil-avy-goto-char-2)
  (global-set-key (kbd "C-k") 'evil-avy-goto-word-or-subword-1)
  (global-set-key (kbd "C-l") 'evil-avy-goto-line)

  ;; TODO this motion should be major-mode specific and handle navi altogether
  (defun avy-navi-goto-outline ()
    (interactive)
    (avy--generic-jump "[;]+\\( \\)" nil 'post))  ; must be post

  (defun avy-navi-goto-comment ()
    (interactive)
    (avy--generic-jump comment-start-skip nil 'pre))

  (global-set-key (kbd "C-;") 'avy-navi-goto-outline)
  ;; TODO probably better way to do multi evil global set
  (evil-global-set-key 'normal (kbd "C-o") 'avy-navi-goto-outline)
  (evil-global-set-key 'visual (kbd "C-o") 'avy-navi-goto-outline)
  (evil-global-set-key 'replace (kbd "C-o") 'avy-navi-goto-outline)
  (evil-global-set-key 'operator (kbd "C-o") 'avy-navi-goto-outline)
  (evil-global-set-key 'motion (kbd "C-o") 'avy-navi-goto-outline)
  (evil-global-set-key 'emacs (kbd "C-o") 'avy-navi-goto-outline)

  (with-eval-after-load 'flyspell
    (evil-define-key '(normal insert visual replace operator motion emacs)
      flyspell-mode-map (kbd "C-;") 'avy-navi-goto-comment))

  (with-eval-after-load 'org
    (evil-define-key '(normal insert visual replace operator motion emacs)
      org-mode-map (kbd "C-j") 'evil-avy-goto-char-2)
    (evil-define-key '(normal insert visual replace operator motion emacs)
      org-mode-map (kbd "C-k") 'evil-avy-goto-word-or-subword-1))

  (with-eval-after-load 'python
    (evil-define-key '(normal insert visual replace operator motion emacs)
      python-mode-map (kbd "C-j") 'evil-avy-goto-char-2)))

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
                               )))

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
  (with-eval-after-load 'ox-extra
    (ox-extras-activate '(ignore-headlines)))

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
         )))

;;;; Theming

(defun module/org/theming ()
  "Org theming updates."

  (setq org-bullets-bullet-list '("■" "○" "✸" "✿")
        org-priority-faces '((65 :inherit org-priority :foreground "red")
                             (66 :inherit org-priority :foreground "brown")
                             (67 :inherit org-priority :foreground "blue"))
        org-ellipsis "▼"))

;;; Outshine

(defun module/outshine ()
  (module/outshine/navi-mode)
  (module/outshine/outshine-mode))

;;;; Navi-mode

(defun module/outshine/navi-mode ()
  "Navi mode bar vim bindings and improvements."

  (require 'navi-mode)

  (add-to-list 'navi-key-mappings
               '("python" .
                 ((:FUN . "f")
                  (:OBJ . "x"))))
  (add-to-list 'navi-keywords
               '("python" .
                 ((:FUN . "\\(^[ ]*def[a-zA-Z0-9_ ]*\\|^[ ]*class[a-zA-Z0-9_ ]*\\)")
                  (:OBJ . "^[ ]*\\(class[a-zA-Z0-9_ ]*\\)"))))

  (defun my-outline-show-context ()
    "Helper utility for evil navi bindings."
    (interactive)
    (outline-show-entry)
    (outline-show-branches))

  (let ((map (make-sparse-keymap)))
    ;; Cycle Navi
    (define-key map (kbd "TAB") 'navi-cycle-subtree)
    (define-key map (kbd "<backtab>") 'navi-cycle-buffer)
    ;; Modify subtree hierarchy
    (define-key map (kbd "M-h") 'navi-promote-subtree)
    (define-key map (kbd "M-j") 'navi-move-down-subtree)
    (define-key map (kbd "M-k") 'navi-move-up-subtree)
    (define-key map (kbd "M-l") 'navi-demote-subtree)
    ;; another way to exit
    (define-key map (kbd "M-n") 'spacemacs/delete-window)

    ;; Custom vim bindings for navi-mode
    ;; Also fixes various bugs related to narrowing/context/scrolling
    (evil-define-key '(normal visual motion) map
      "f" (lambda () (interactive) (navi-generic-command ?f current-prefix-arg))
      "v" (lambda () (interactive) (navi-generic-command ?v current-prefix-arg))
      "x" (lambda () (interactive) (navi-generic-command ?x current-prefix-arg))
      "a" (lambda () (interactive) (navi-generic-command ?a current-prefix-arg))

      "1" (lambda () (interactive) (navi-generic-command ?1 current-prefix-arg))
      "2" (lambda () (interactive) (navi-generic-command ?2 current-prefix-arg))
      "3" (lambda () (interactive) (navi-generic-command ?3 current-prefix-arg))
      "4" (lambda () (interactive) (navi-generic-command ?4 current-prefix-arg))

      ;; Narrow on occurrence
      "n" (lambda () (interactive)
            (navi-narrow-to-thing-at-point)
            (other-window 1)
            (my-outline-show-context)
            (other-window 1))
      ;; Open occurence but do not goto
      "d" (lambda () (interactive)
            (occur-mode-display-occurrence)
            (other-window 1)
            (my-outline-show-context)
            (recenter 3)
            (other-window 1))
      ;; Open and goto occurrence. Capital for closing navi
      "o" (lambda () (interactive)
            (navi-goto-occurrence-other-window)
            (my-outline-show-context)
            (recenter 3))
      "O" (lambda () (interactive)
            (navi-goto-occurrence-other-window)
            (delete-other-windows)
            (my-outline-show-context)
            (recenter 3))
      ;; Exit Navi
      "q" 'spacemacs/delete-window
      ;; Widen narrowed navi buffer
      "w" 'navi-widen
      ;; Undo modifications to headers done within navi buffer
      "u" 'navi-undo)

    (setq navi-mode-map map)))

;;;; Outshine-mode

(defun module/outshine/outshine-mode ()
  "Outline/Outshine mode bindings and Navi integration."

  (require 'outshine)

  (defun my-outshine-navi ()
    "Enhanced narrowing and popwin-like functionality to start navi mode."
    (interactive)
    (let ((line nil))
      (widen)  ; Otherwise broken on narrowed buffers
      (save-excursion
        (unless (outline-on-heading-p t)
          (outline-previous-visible-heading 1))
        (setq line
              (replace-regexp-in-string "\n$" ""
                                        (thing-at-point 'line t))))
      ;; window stuff
      (split-window-below)
      (outshine-navi)
      (evil-window-move-far-left)
      (shrink-window-horizontally (- (window-width) 35))
      ;; default to 3 heading levels
      (navi-generic-command ?3 nil)
      (search-forward-regexp line)))

  (define-key org-mode-map (kbd "M-n") 'my-outshine-navi)

  ;; Outline minor mode vim keybindings
  (let ((map outline-minor-mode-map))
    ;; Core functions
    (define-key map (kbd "M-n") 'my-outshine-navi)
    (define-key map (kbd "<backtab>") 'outshine-cycle-buffer)
    (define-key map (kbd "M-h") 'outline-promote)
    (define-key map (kbd "M-l") 'outline-demote)

    ;; Insert Heading
    (define-key map (kbd "M-RET") 'outshine-insert-heading)
    ;; Insert Subheading
    (define-key map (kbd "C-M-<return>")
      (lambda ()
        (interactive)
        (let ((line nil) (str nil))
          (save-excursion
            (outline-previous-visible-heading 1)
            (setq level (outshine-calc-outline-level))
            (setq str (outshine-calc-outline-string-at-level (+ 1 level))))
          (evil-unimpaired/insert-space-below 1)
          (evil-next-line 1)
          (insert str))))

    ;; Bring org-mode g-based evil navigation to outline-minor-mode
    (evil-define-key '(normal visual motion) map
      "gh" 'outline-up-heading
      "gj" 'outline-forward-same-level
      "gk" 'outline-backward-same-level
      "gl" 'outline-next-visible-heading
      "gu" 'outline-previous-visible-heading

      ;; Narrows buffer without needing to have cursor on heading
      (kbd "SPC n n") (lambda ()
                        (interactive)
                        (save-excursion
                          (unless (outline-on-heading-p t)
                            (outline-previous-visible-heading 1))
                          (outshine-narrow-to-subtree)))
      (kbd "SPC n j") 'outline-move-subtree-down
      (kbd "SPC n k") 'outline-move-subtree-up))

  (setq outshine-use-speed-commands t)

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

  (defun deploy-blog ()
    "Run hugo and push changes upstream from anywhere."
    (interactive)
    (let ((original-dir default-directory)
          (run-hugo (concat "hugo -d " public-blog-dir)))
      (cd blog-dir)
      (shell-command run-hugo)
      (cd public-blog-dir)

      (shell-command "git add .")
      (shell-command (concat "git commit -m \"" (current-time-string) "\""))
      (magit-push-current-to-upstream nil)

      (cd original-dir)))

  (defun start-blog-server ()
    "Run hugo server if not already running and open its webpage."
    (interactive)
    (let ((original-dir default-directory))
      (unless (get-process hugo-process)
        (cd blog-dir)
        (start-process hugo-process nil "hugo" "server")
        (cd original-dir))

      (browse-url hugo-server-site)))

  (defun end-blog-server ()
    "End hugo server process if running."
    (interactive)
    (--when-let (get-process hugo-process)
      (delete-process it)))

  (spacemacs/set-leader-keys (kbd "ab") 'deploy-blog)
  (spacemacs/set-leader-keys (kbd "aa") 'start-blog-server)
  (spacemacs/set-leader-keys (kbd "ae") 'end-blog-server))
