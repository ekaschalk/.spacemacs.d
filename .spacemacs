;; -*- mode: emacs-lisp -*-

;;; Introduction

;; -- Eric Kaschalk's Spacemacs Configuration --
;; -- Emacs 25.2.1 --
;; -- Dev Branch - Release 0.200.9.x - pulled: 5/29 - packages updated: 5/29 --
;; -- Dual config for Windows and Arch Linux --
;; -- Contact: ekaschalk@gmail.com --
;;
;; Organization
;; ---------
;; Literate configs with org-mode are not natively supported by spacemacs
;; due to org-mode's interaction with spacemacs layers systems.
;;
;; The approach taken is to use the `outline-minor-mode' in conjuction
;; with `outshine-mode' and `navi-mode' to maintain benefits of literate
;; documentation and org-modes navigation, collapsing, and narrowing facilities.
;;
;; Configuration is grouped by theme. The current groups are:
;; Display - Ivy - Configuration - Navigation - Misc - Python - Org - Outshine
;; - GNUS
;;
;; Each group is broken into further components for targetted enabling/disabling
;; Some groups require a specific execution ordering. Ordering requirements are
;; specifed with Group x comments. Within the group, the packages are lexical.
;;
;; Notable
;; ---------
;; 1. Outline-minor-mode + Navi + Outshine mode enhancements.
;;    - Org-like headings, navigation, faces in programming buffers.
;;    - Vim bindings for outline-minor-mode and navi-mode.
;;    - Enhanced narrowing.
;; 2. Unique visual enhancements.
;;    - Fira code ligature integration (Fira Code font not required, I use Hack)
;;    - Math and other custom symbols for major modes
;;    - Pretty magit commits with ivy integration
;;    - Pretty eshell
;; x. Miscellaneous small snippets.
;;    - Mypy flychecking integrated with pylint.
;;    - Unicode ellipsis for outline headings
;;    - Many premium keybindings (C-SPC, C-h, C-e, 0, M-d...) have been rebound
;;    - Extra avy motions

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
        (clojure :variables
                 clojure-enable-fancify-symbols t)
        (haskell :variables
                 haskell-completion-backend 'intero)
        (python :variables
                python-sort-imports-on-save t
                python-test-runner 'pytest))

      dotspacemacs/layers/rare
      '(;; Pandoc for more export options, used for blogging
        pandoc
        ;; Markdown mode for viewing outside documentation
        markdown
        ;; Graphviz mode for usage with org-babel
        graphviz
        ;; REST client for usage with org-babel
        restclient)

      ;; OS-Specific and Local Packages
      dotspacemacs/layers/windows '()
      dotspacemacs/layers/linux '()
      dotspacemacs/layers/local '()
      )

;;;; Additional Packages

(setq dotspacemacs/additional/packages
      '(;; Enhancements to outline minor mode
        outshine
        ;; Navigation buffer for outline minor mode
        navi-mode
        ;; Virtualenv management for Python
        virtualenvwrapper
        ;; Additional ligature replacements for eg. greeks
        pretty-mode
        ;; Spotify layer improvements behind SPC a m s prefix
        helm-spotify-plus
        ;; Doom theme
        doom-themes
        ;; All-the-icons integration with mode line
        spaceline-all-the-icons
        ;; Org google calendar integration
        org-gcal
        ;; Org vcard for contact export/import
        org-vcard
        ))

;;;; Spacemacs
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '(".layers/")
   dotspacemacs-additional-packages dotspacemacs/additional/packages
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-but-keep-unused
   dotspacemacs-configuration-layers
   (append dotspacemacs/layers/core
           dotspacemacs/layers/langs
           dotspacemacs/layers/rare
           dotspacemacs/layers/local
           (unless-linux dotspacemacs/layers/windows)
           (when-linux dotspacemacs/layers/linux))))

;;; Spacemacs-Init
;;;; Configuration
(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-themes '(
                         doom-one
                         )
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
  ;; Rids the verbose custom settings from being written to .spacemacs
  (setq custom-file "./elisp/.custom-settings.el")
  (load (if-linux "~/elisp/.custom-settings.el"
                  "c:/~/elisp/.custom-settings.el"))
  (load (if-linux "~/elisp/prettify-utils.el"
                  "c:/~/elisp/prettify-utils.el"))
  ;; (load (if-linux "~/elisp/all-the-icons-ivy.el"
  ;;                 "c:/~/elisp/all-the-icons-ivy.el"))
  )

;;; Spacemacs-User-config
(defun dotspacemacs/user-config ()
  ;; Dash is a general purpose, modern list and functional api for emacs lisp
  (with-eval-after-load 'dash
    ;; Group 1
    (dotspacemacs/user-config/display)

    ;; Rest
    (dotspacemacs/user-config/configuration)
    (dotspacemacs/user-config/ivy)
    (dotspacemacs/user-config/gnus)
    (dotspacemacs/user-config/misc)
    (dotspacemacs/user-config/navigation)
    (dotspacemacs/user-config/org)
    (dotspacemacs/user-config/org-gcal)
    (dotspacemacs/user-config/outshine)
    (dotspacemacs/user-config/python)))

;;; Display
(defun dotspacemacs/user-config/display ()
  ;; Group 1
  (unless-linux-call 'dotspacemacs/user-config/display/windows-frame-size-fix)

  ;; Group 2
  (dotspacemacs/user-config/display/init-doom-theme)

  ;; Group 3
  (dotspacemacs/user-config/display/font-locks)

  ;; Rest
  (dotspacemacs/user-config/display/all-the-icons)
  (dotspacemacs/user-config/display/extra-syntax-highlighting)
  (dotspacemacs/user-config/display/face-updates)
  (dotspacemacs/user-config/display/modeline)
  (dotspacemacs/user-config/display/outline-ellipsis-modification)
  (dotspacemacs/user-config/display/prettify-magit)
  (dotspacemacs/user-config/display/prettify-symbols)
  (dotspacemacs/user-config/display/shell))

;;;; Windows-frame-size-fix
(defun dotspacemacs/user-config/display/windows-frame-size-fix ()
  "Surface has 200% scaling, doesn't apply to emacs, fixes with push of `f2`."
  (add-to-list 'default-frame-alist '(font . "Hack"))
  (set-face-attribute 'default t :font "Hack")
  (global-set-key (kbd "<f2>")
                  (lambda () (interactive) (mapc (lambda (x) (zoom-frm-out)) '(1 2)))))

;;;; Init-doom-theme
(defun dotspacemacs/user-config/display/init-doom-theme ()
  "Doom theme configuration."
  ;; Note to readers: there is a bug with doom-vibrant-theme and spacemacs
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-one-brighter-modeline t
        doom-one-brighter-comments t)

  ;; Default doom-mode brightens source files.
  ;; I prefer the opposite - brighter special buffers and dark source buffers.
  (defun opposite-doom-buffer-mode-maybe ()
    (unless buffer-file-name
      (doom-buffer-mode +1)))

  (add-hook 'after-change-major-mode-hook #'opposite-doom-buffer-mode-maybe)
  (add-hook 'after-revert-hook #'opposite-doom-buffer-mode-maybe)
  (add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer))

;;;; Font-locks
;;;;; Core
(defun dotspacemacs/user-config/display/font-locks ()
  "Enable following font-locks for appropriate modes."
  ;; Use Fira Code's ligatures, does not require Fira Code font to be in use
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

  (defun -add-font-lock-kwds (font-lock-alist)
    (defun -build-font-lock-alist (regex-char-pair)
      `(,(car regex-char-pair)
        (0 (prog1 ()
             (compose-region
              (match-beginning 1)
              (match-end 1)
              ,(concat "	"
                       (list (cadr regex-char-pair))))))))
    (font-lock-add-keywords nil (mapcar '-build-font-lock-alist font-lock-alist)))

  ;; Fira Code
  (add-hook 'prog-mode-hook
            (-partial '-add-font-lock-kwds fira-font-lock-alist))
  (add-hook 'org-mode-hook
            (-partial '-add-font-lock-kwds fira-font-lock-alist))
  ;; Python
  (add-hook 'python-mode-hook
            (-partial '-add-font-lock-kwds python-font-lock-alist))
  ;; Emacs Lisp
  (add-hook 'emacs-lisp-mode-hook
            (-partial '-add-font-lock-kwds emacs-lisp-font-lock-alist))
  ;; Hy
  (add-hook 'hy-mode-hook
            (-partial '-add-font-lock-kwds hy-font-lock-alist))
  ;; Navi-mode
  (add-hook 'navi-mode-hook
            (-partial '-add-font-lock-kwds navi-font-lock-alist)))

;;;;; Fira-font-locks
(defconst fira-font-lock-alist
  '(;;;; OPERATORS
    ;;;;;; Pipes
    ("\\(<|\\)" #Xe14d) ("\\(<>\\)" #Xe15b) ("\\(<|>\\)" #Xe14e) ("\\(|>\\)" #Xe135)

    ;;;;;; Brackets
    ("\\(<\\*\\)" #Xe14b) ("\\(<\\*>\\)" #Xe14c) ("\\(\\*>\\)" #Xe104)
    ("\\(<\\$\\)" #Xe14f) ("\\(<\\$>\\)" #Xe150) ("\\(\\$>\\)" #Xe137)
    ("\\(<\\+\\)" #Xe155) ("\\(<\\+>\\)" #Xe156) ("\\(\\+>\\)" #Xe13a)

    ;;;;;; Equality
    ("\\(!=\\)" #Xe10e) ("\\(!==\\)"         #Xe10f) ("\\(=/=\\)" #Xe143)
    ("\\(/=\\)" #Xe12c) ("\\(/==\\)"         #Xe12d)
    ("\\(===\\)"#Xe13d) ("[^!/]\\(==\\)[^>]" #Xe13c)

    ;;;;;; Equality Special
    ("\\(||=\\)"  #Xe133) ("[^|]\\(|=\\)" #Xe134)
    ("\\(~=\\)"   #Xe166)
    ("\\(\\^=\\)" #Xe136)
    ("\\(=:=\\)"  #Xe13b)

    ;;;;;; Comparisons
    ("\\(<=\\)" #Xe141) ("\\(>=\\)" #Xe145)
    ("\\(</\\)" #Xe162) ("\\(</>\\)" #Xe163)

    ;;;;;; Shifts
    ("[^-=]\\(>>\\)" #Xe147) ("\\(>>>\\)" #Xe14a)
    ("[^-=]\\(<<\\)" #Xe15c) ("\\(<<<\\)" #Xe15f)

    ;;;;;; Dots
    ("\\(\\.-\\)"    #Xe122) ("\\(\\.=\\)" #Xe123)
    ("\\(\\.\\.<\\)" #Xe125)

    ;;;;;; Hashes
    ("\\(#{\\)"  #Xe119) ("\\(#(\\)"   #Xe11e) ("\\(#_\\)"   #Xe120)
    ("\\(#_(\\)" #Xe121) ("\\(#\\?\\)" #Xe11f) ("\\(#\\[\\)" #Xe11a)

    ;;;; REPEATED CHARACTERS
    ;;;;;; 2-Repeats
    ("\\(||\\)" #Xe132)
    ("\\(!!\\)" #Xe10d)
    ("\\(%%\\)" #Xe16a)
    ("\\(&&\\)" #Xe131)

    ;;;;;; 2+3-Repeats
    ("\\(##\\)"       #Xe11b) ("\\(###\\)"         #Xe11c) ("\\(####\\)" #Xe11d)
    ("\\(--\\)"       #Xe111) ("\\(---\\)"         #Xe112)
    ("\\({-\\)"       #Xe108) ("\\(-}\\)"          #Xe110)
    ("\\(\\\\\\\\\\)" #Xe106) ("\\(\\\\\\\\\\\\\\)" #Xe107)
    ("\\(\\.\\.\\)"   #Xe124) ("\\(\\.\\.\\.\\)"   #Xe126)
    ("\\(\\+\\+\\)"   #Xe138) ("\\(\\+\\+\\+\\)"   #Xe139)
    ("\\(//\\)"       #Xe12f) ("\\(///\\)"         #Xe130)
    ("\\(::\\)"       #Xe10a) ("\\(:::\\)"         #Xe10b)

    ;;;; ARROWS
    ;;;;;; Direct
    ("[^-]\\(->\\)" #Xe114) ("[^=]\\(=>\\)" #Xe13f)
    ("\\(<-\\)"     #Xe152)
    ("\\(-->\\)"    #Xe113) ("\\(->>\\)"    #Xe115)
    ("\\(==>\\)"    #Xe13e) ("\\(=>>\\)"    #Xe140)
    ("\\(<--\\)"    #Xe153) ("\\(<<-\\)"    #Xe15d)
    ("\\(<==\\)"    #Xe158) ("\\(<<=\\)"    #Xe15e)
    ("\\(<->\\)"    #Xe154) ("\\(<=>\\)"    #Xe159)

    ;;;;;; Branches
    ("\\(-<\\)"  #Xe116) ("\\(-<<\\)" #Xe117)
    ("\\(>-\\)"  #Xe144) ("\\(>>-\\)" #Xe148)
    ("\\(=<<\\)" #Xe142) ("\\(>>=\\)" #Xe149)
    ("\\(>=>\\)" #Xe146) ("\\(<=<\\)" #Xe15a)

    ;;;;;; Squiggly
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
    ("\\(:;;;\\) "                   ?■)
    ("\\(:;;;;\\) "                  ?○)
    ("\\(:;;;;;\\) "                 ?✸)
    ("\\(:;;;;;;\\) "                ?✿)
    ;; Hide first line
    ("\\(.*matches.*$\\)"            ? )
    ;; Hide numbers (numbers still needed for internal navi methods)
    ("\\([ ]+[0-9]+\\)"              ? )
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
(defun dotspacemacs/user-config/display/all-the-icons ()
  "Add hylang icon to all-the-icons for neotree and modeline integration."
  (with-eval-after-load 'all-the-icons
    ;; Both all-the-icons-icon-alist and all-the-icons-mode-icon-alist
    ;; Need to be updated for either modification to take effect.
    (add-to-list
     'all-the-icons-icon-alist
     '("\\.hy$" all-the-icons-fileicon "lisp" :face all-the-icons-orange))
    (add-to-list
     'all-the-icons-mode-icon-alist
     '(hy-mode all-the-icons-fileicon "lisp" :face all-the-icons-orange))))

;;;; Extra-syntax-highlighting
(defun dotspacemacs/user-config/display/extra-syntax-highlighting ()
  "Extra syntax highlighting for desired keywords."
  (defun hy-extra-syntax ()
    (font-lock-add-keywords
     nil '(("\\<\\(self\\)" . 'font-lock-keyword-face)
         ("\\<\\(staticmethod\\)\\>" . 'font-lock-function-name-face)
         ("\\<\\(classmethod\\)\\>" . 'font-lock-function-name-face)
         ("\\<\\(property\\)\\>" . 'font-lock-function-name-face)
         ("\\<\\(composite\\)\\>" . 'font-lock-function-name-face)
         ("\\<\\(import\\)\\>" . 'font-lock-function-name-face)
         ("\\<\\(require\\)\\>" . 'font-lock-function-name-face)

         ;; Syntax highlighting for reader-macros
         ("\\(#.\\)" . 'font-lock-function-name-face))))

  (defun navi-extra-syntax ()
    (font-lock-add-keywords
     nil '((":\\(;;;\\) .*$" .    'org-level-1)
         (":\\(;;;;\\) .*$" .   'org-level-2)
         (":\\(;;;;;\\) .*$" .  'org-level-3)
         (":\\(;;;;;\\) .*$" .  'org-level-4))))

  (add-hook 'hy-mode-hook 'hy-extra-syntax)
  (add-hook 'navi-mode-hook 'navi-extra-syntax))

;;;; Face-updates
(defun dotspacemacs/user-config/display/face-updates ()
  "Face configuration."
  (defun -update-faces ()
    (custom-theme-set-faces
     (car custom-enabled-themes)
     ;; Fixes bad tint for mode-line background in doom-one theme
     '(mode-line ((t (:inherit mode-line :background "#21242b"))))

     ;; Matching parenthesis much more obvious when underlined
     '(show-paren-match ((t (:inherit show-paren-match :underline t))))

     ;; Org-level-3 and org-level-2 were too similar with color-blindness
     '(org-level-2 ((t (:height 1.10 :foreground "forest green"
                                :weight ultra-bold))))
     '(org-level-3 ((t (:height 1.03 :foreground "light slate gray"
                                :weight ultra-bold))))

     ;; Since outlines are necessarily further apart than org-mode headers
     ;; We box the outlines to make them stand out in programming buffers.
     '(outline-1 ((t (:inherit org-level-1 :box t))))
     '(outline-2 ((t (:inherit org-level-2 :box t))))
     '(outline-3 ((t (:inherit org-level-3 :box t :height 1.03))))
     '(outline-4 ((t (:inherit org-level-4 :underline t))))))

  ;; Apply face updates on emacs initialization
  (-update-faces)
  ;; Apply face updates update whenever theme is toggled
  (add-hook 'spacemacs-post-theme-change-hook '-update-faces))

;;;; Modeline
(defun dotspacemacs/user-config/display/modeline ()
  "Minimalistic spaceline-all-the-icons configuration."
  (use-package spaceline-all-the-icons
    :after spaceline  ; eval-after-load doesn't work for this setup
    :config (progn
              ;; Initialization
              (spaceline-all-the-icons--setup-neotree)
              (spaceline-all-the-icons-theme)

              ;; Configuration
              (setq spaceline-highlight-face-func 'spaceline-highlight-face-default
                    powerline-default-separator 'arrow
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
(defun dotspacemacs/user-config/display/outline-ellipsis-modification ()
  "Org-ellipsis but for outline-minor-mode headings"
  ;; Modified org-ellipsis implementation

  (add-hook
   'outline-minor-mode-hook
   (lambda ()
     (let ((display-table
            (if buffer-display-table
                buffer-display-table
              (make-display-table))))
       (unless buffer-display-table
         (setq buffer-display-table display-table))
       (set-display-table-slot
        display-table 4
        (vconcat
         (mapcar (lambda (c) (make-glyph-code c 'font-lock-keyword-face)) "▼")))))))

;;;; Pretty-magit
(defun dotspacemacs/user-config/display/prettify-magit ()
  "Add faces to Magit manually for things like commit headers eg. (Add: ...)."

  ;; https://github.com/domtronn/all-the-icons.el
  ;; Can look through the icons by opening a scratch a buffer and running eg.
  ;; (all-the-icons-insert-icons-for 'material)

  ;; Requires all-the-icons font, uni numbers for magit commit headers
  ;; Note that the unicode symbols you see are likely *not* the right symbols
  ;; Setting the fontset will show the correct symbol
  (set-fontset-font t '(#xf091 . #xf091) "github-octicons") ; 
  (set-fontset-font t '(#xf059 . #xf059) "github-octicons") ; 
  (set-fontset-font t '(#xf076 . #xf076) "github-octicons") ; 
  (set-fontset-font t '(#xf075 . #xf075) "github-octicons") ; 
  (set-fontset-font t '(#xf0c4 . #xf0c4) "fontawesome")     ; 
  (set-fontset-font t '(#xf09b . #xf09b) "fontawesome")     ; 
  (set-fontset-font t '(#xe907 . #xe907) "all-the-icons")   ; 

  (setq my-magit-colors '(:feature "silver"
                          :fix "#FB6542"  ; sunset
                          :add "#375E97"  ; sky
                          :clean "#FFBB00"  ; sunflower
                          :docs "#3F681C" ; grass
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
                               ("\\<\\(origin/master\\)" ?)))

  ;; Font-lock mode breaks magit. Magit uses propertized strings and does lots
  ;; of magic since it assumes a read-only buffer. Doing anything with
  ;; font-lock-keywords will break styling of the whole status buffer.
  ;; So we update the magit faces mannually.
  (defun add-magit-faces ()
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

  ;; Ivy prompt for commit keywords.
  ;; Now due to the delayed use of minibuffer in commit buffers, we cannot
  ;; use add-advice and instead use `git-commit-setup-hook' to run the prompt.
  ;; However, we only want the prompt for c-c `magit-commit' and not its
  ;; variants. The only way to distinguish the calling commit mode is through
  ;; the caller, so we use advice add on `magit-commit' for a prompt predicate.
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

  ;; TODO Escaping ivy-read in commit prompt will make the next call
  ;; to commit jump to commit template without prompt. This is an issue
  ;; between integrating ivy and magit exiting, not simple.
  ;; So use , k rather than ESC on a commit prompt to cancel commit
  ;; TODO the symbol stays but the face is overwritten in commit buffers
  ;; In fact it is overwritten continuously! Magit uses font lock here.
  ;; However doing anything (any trivial mod) with font-lock-add-keywords
  ;; will break highlighting the buffer. We can get the symbols added
  ;; but not the coloring, sizing, and other face attributes.
  (with-eval-after-load 'magit
    ;; Hook is overwritten by ivy prompt
    (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)

    (advice-add 'magit-status :after 'add-magit-faces)
    (advice-add 'magit-refresh-buffer :after 'add-magit-faces)
    (advice-add 'magit-commit :after 'use-magit-commit-prompt)

    (add-hook 'git-commit-setup-hook 'magit-commit-prompt)))

;;;; Prettify-symbols
(defun dotspacemacs/user-config/display/prettify-symbols ()
  "Visually replace text with unicode."
  ;; Ivy keybinding has 'SPC i u' for consel-unicode-char
  ;; This function is extremely useful when exploring symbols

  ;; Try `what-cursor-position' if the symbol doesnt render.
  ;; Then see the fontsets below for choosing the offending symbol's fonts
  ;; NOTE This plist approach doesn't preserve spaces in unicode str
  (setq pretty-options
        (-flatten
         (prettify-utils-generate
          ;;;;; Functional
          (:lambda      "λ") (:def         "ƒ")

          ;;;;; Types
          (:null        "∅") (:true        "𝕋") (:false       "𝔽")
          (:int         "ℤ") (:float       "ℝ")
          (:str         "𝕊") (:bool        "𝔹")

          ;;;;; Flow
          (:in          "∈") (:not-in      "∉")
          (:return     "⟼") (:yield      "⟻")
          (:not         "￢")
          (:for         "∀")

          ;;;;; Other
          (:tuple       "⨂")
          (:pipe        "")
          )))

  (defun get-pairs (KWDS)
    "KWDS '(:def-symb mode-symb), returns alist for prettify-symbols-alist."
    (-non-nil
     (--map (when-let (major-mode-sym (plist-get KWDS it))
             `(,major-mode-sym
               ,(plist-get pretty-options it)))
           pretty-options)))

  (setq hy-pretty-choices
        (get-pairs
         '(:lambda "fn" :def "defn"
                   :null "None" :true "True" :false "False"
                   :in "in" :not "not"
                   :tuple "#t"
                   :pipe "ap-pipe"
                   ))

        python-pretty-choices
        (get-pairs
         '(:lambda "lambda" :def "def"
                   :null "None" :true "True" :false "False"
                   :int "int" :float "float" :str "str" :bool "bool"
                   :not "not" :for "for" :in "in" :not-in "not in"
                   :return "return" :yield "yield"
                   :tuple "Tuple"
                   :pipe "tz-pipe"
                   ))
        )

  ;; Pretty pairs for modes
  (defun set-hy-pretty-pairs ()
    (setq prettify-symbols-alist hy-pretty-choices))

  (defun set-python-pretty-pairs ()
    (setq prettify-symbols-alist
          (append python-pretty-choices
                  (prettify-utils-generate
                   ;; Self for hy necessarily done through font-lock
                   ("self"     "⊙")

                   ;; Mypy Stuff
                   ("Dict"     "𝔇") ("List"     "ℒ")
                   ("Callable" "ℱ") ("Mapping"  "ℳ") ("Iterable" "𝔗")
                   ("Union"    "⋃") ("Any"      "❔")
                   ))))

  ;; Force specified font for some symbols
  (set-fontset-font t '(#x1d54a . #x1d54a) "Symbola")  ; 𝕊
  (set-fontset-font t '(#x2a02 . #x2a02) "Symbola")    ; ⨂
  (set-fontset-font t '(#x2205 . #x2205) "Symbola")    ; ∅
  (set-fontset-font t '(#x27fb . #x27fc) "Symbola")    ; ⟻, ⟼
  (set-fontset-font t '(#x2299 . #x2299) "Symbola")    ; ⊙
  (set-fontset-font t '(#x1d54b . #x1d54b) "Symbola")  ; 𝕋
  (set-fontset-font t '(#x1d53d . #x1d53d) "Symbola")  ; 𝔽
  (set-fontset-font t '(#x1d539 . #x1d539) "Symbola")  ; 𝔹
  (set-fontset-font t '(#x1d507 . #x1d507) "Symbola")  ; 𝔇
  (set-fontset-font t '(#x1d517 . #x1d517) "Symbola")  ; 𝔗

  ;; Enable pretty modes
  (add-hook 'hy-mode-hook 'set-hy-pretty-pairs)
  (add-hook 'python-mode-hook 'set-python-pretty-pairs)

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
(defun dotspacemacs/user-config/display/shell ()
  "Eshell prettification."
  (require 'virtualenvwrapper)  ; TODO integrate these better way
  (pyvenv-mode 1)
  (load (if-linux "~/elisp/eshell-git.el" "c:/~/elisp/eshell-git.el"))

  (set-fontset-font t '(#xe192 . #xe192) "material")       ; Clock 
  (set-fontset-font t '(#xf07c . #xf07c) "fontawesome")    ; Folder 
  (set-fontset-font t '(#xf115 . #xf115) "fontawesome")    ; Folder 
  (set-fontset-font t '(#xf0da . #xf0da) "fontawesome")    ; Prompt 
  (set-fontset-font t '(#xf101 . #xf101) "all-the-icons")  ; Prompt 
  (set-fontset-font t '(#xe928 . #xe928) "all-the-icons")  ; Py 
  (set-fontset-font t '(#xe907 . #xe907) "all-the-icons")  ; Git 

  (defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))

  (defun set-eshell-prompt-icon (icon)
    (let ((prompt (concat icon " ")))
      (setq eshell-prompt-regexp prompt)
      (concat "\n" (with-face prompt eshell-prompt-face))))

  (defun eshell-section (icon str &rest properties)
    (when str
      (concat
       (with-face eshell-section-sep eshell-sep-face)
       (with-face (concat icon eshell-icon-sep str " ") properties))))

  (setq eshell-prompt-number 0)
  (add-hook 'eshell-exit-hook (lambda () (setq eshell-prompt-number 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args)
                (setq eshell-prompt-number (+ 1 eshell-prompt-number))))

  (setq eshell-prompt-face '(:foreground "steel blue")
        eshell-sep-face '(:foreground "light slate gray")
        eshell-section-sep ""
        eshell-icon-sep " "

        ;; new modeline style
        eshell-git-face '(:background "indian red")
        eshell-dir-face '(:foreground "ivory"
                                      :background "steel blue" :weight bold)
        eshell-venv-face '(:background "slate gray")
        eshell-time-face '(:background "#007849")  ; greenish
        seg-sep " "
        seg-sep-face-dir '(:foreground "steel blue" :background "indian red")
        seg-sep-face-git '(:foreground "indian red" :background "slate gray")
        seg-sep-face-time '(:foreground "slate gray" :background "#007849")
        seg-sep-face-end '(:foreground "#007849")

        eshell-prompt-function
        (lambda ()
          (concat
           (with-face "\n┌─" eshell-prompt-face)

           (eshell-section " " (abbreviate-file-name (eshell/pwd))
                           eshell-dir-face)

           (with-face seg-sep seg-sep-face-dir)
           (eshell-section "" (eshell-git-prompt--branch-name)
                           eshell-git-face)

           (with-face seg-sep seg-sep-face-git)
           (eshell-section "" pyvenv-virtual-env-name
                           eshell-venv-face)

           (with-face seg-sep seg-sep-face-time)
           (eshell-section "" (format-time-string "%H:%M" (current-time))
                           eshell-time-face)

           (with-face seg-sep seg-sep-face-end)

           (with-face (concat "\n|" (number-to-string eshell-prompt-number))
             eshell-prompt-face)
           (set-eshell-prompt-icon "└─")
           ))))

;;; Ivy
(defun dotspacemacs/user-config/ivy ()
  "Ivy completion framework configuration."

  ;; TODO handle loading better
  (require 's)
  (load (if-linux "~/elisp/all-the-icons-ivy.el"
                  "c:/~/elisp/all-the-icons-ivy.el"))
  (set-fontset-font t '(#xf016 . #xf016) "fileicons")  ; 
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
(defun dotspacemacs/user-config/configuration ()
  (dotspacemacs/user-config/configuration/editing)
  (dotspacemacs/user-config/configuration/evil)
  (dotspacemacs/user-config/configuration/visual))

;;;; Editing
(defun dotspacemacs/user-config/configuration/editing ()
  (hungry-delete-mode 1)  ; in edit mode back gets all contiguous whitespace
  (spacemacs/toggle-aggressive-indent-globally-on)  ; auto-indentation
  (add-hook 'org-mode-hook (lambda () (auto-fill-mode 1))))  ; SPC splits past 80

;;;; Evil
(defun dotspacemacs/user-config/configuration/evil ()
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-unordered-key-sequence "true"))

;;;; Visual
(defun dotspacemacs/user-config/configuration/visual ()
  (spacemacs/toggle-highlight-long-lines-globally-on)
  (fringe-mode '(1 . 1))  ; Minimal left padding and ~ end newline markers
  (rainbow-delimiters-mode-enable)  ; Paren color based on depth
  (global-highlight-parentheses-mode 1)  ; Highlight containing parens
  (spacemacs/toggle-mode-line-minor-modes-off))  ; no uni symbs next to major

;;; Navigation
(defun dotspacemacs/user-config/navigation ()
  (dotspacemacs/user-config/navigation/avy)
  (dotspacemacs/user-config/navigation/extra-bindings)
  (dotspacemacs/user-config/navigation/file-links)
  (dotspacemacs/user-config/navigation/searching))

;;;; Avy
(defun dotspacemacs/user-config/navigation/avy ()
  "Avy keybindings and custom commands."
  (global-set-key (kbd "C-h") 'avy-pop-mark)
  (global-set-key (kbd "C-j") 'evil-avy-goto-char-2)
  (global-set-key (kbd "C-k") 'evil-avy-goto-word-or-subword-1)
  (global-set-key (kbd "C-l") 'evil-avy-goto-line)

  ;; TODO for some reason must require avy to get avy--generic-jump loaded
  (require 'avy)
  ;; TODO this should be major-mode specific and handle navi altogether
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
(defun dotspacemacs/user-config/navigation/extra-bindings ()
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
(defun dotspacemacs/user-config/navigation/file-links ()
  (spacemacs/set-leader-keys (kbd "aof") 'org-open-at-point-global))

;;;; Searching
(defun dotspacemacs/user-config/navigation/searching ()
  ;; Evil searching scrolls to center of match
  (advice-add 'evil-ex-search-next :after
              (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
  (advice-add 'evil-ex-search-previous :after
              (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos)))))

;;; Misc
(defun dotspacemacs/user-config/misc ()
  (when-linux-call 'dotspacemacs/user-config/misc/spotify)
  (dotspacemacs/user-config/misc/aspell)
  (dotspacemacs/user-config/misc/auto-completion)
  (dotspacemacs/user-config/misc/lisp-state)
  (dotspacemacs/user-config/misc/macros)
  (dotspacemacs/user-config/misc/neotree)
  (dotspacemacs/user-config/misc/projectile)
  (dotspacemacs/user-config/misc/shell)
  (dotspacemacs/user-config/misc/windows)
  (dotspacemacs/user-config/misc/yassnippet))

;;;; Spotify
(defun dotspacemacs/user-config/misc/spotify ()
  (global-set-key (kbd "M-s s") 'helm-spotify-plus)
  (global-set-key (kbd "M-s j") 'helm-spotify-plus-play)
  (global-set-key (kbd "M-s SPC") 'helm-spotify-plus-pause)
  (global-set-key (kbd "M-s l") 'helm-spotify-plus-next)
  (global-set-key (kbd "M-s h") 'helm-spotify-plus-previous))

;;;; Aspell
(defun dotspacemacs/user-config/misc/aspell ()
  (setq ispell-program-name "aspell"))

;;;; Auto-completion
(defun dotspacemacs/user-config/misc/auto-completion ()
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))

;;;; Lisp-state
(defun dotspacemacs/user-config/misc/lisp-state ()
  "Add lisp state shortcut to Clojure and Hy."
  (spacemacs/set-leader-keys-for-major-mode
    'clojure-mode (kbd ",") 'lisp-state-toggle-lisp-state)
  (spacemacs/set-leader-keys-for-major-mode
    'hy-mode (kbd ",") 'lisp-state-toggle-lisp-state))

;;;; Macros
(defun dotspacemacs/user-config/misc/macros ()
  "Evil Q shortcut for vim macros set at @q."
  (evil-global-set-key 'normal (kbd "Q")
                       (lambda () (interactive) (evil-execute-macro 1 "@q"))))

;;;; Neotree
(defun dotspacemacs/user-config/misc/neotree ()
  (setq neo-theme 'icons
        neo-window-width 28)

  (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$"
                                 ;; Pycache and init rarely want to see
                                 "__pycache__" "__init__\\.py"))

  (evil-global-set-key 'normal (kbd "M-f") 'winum-select-window-0)
  (evil-global-set-key 'normal (kbd "M-p") 'neotree-find-project-root))

;;;; Projectile
(defun dotspacemacs/user-config/misc/projectile ()
  (setq projectile-indexing-method 'native))  ; respect .projectile files

;;;; Shell
(defun dotspacemacs/user-config/misc/shell ()
  "Quick eshell with vim interaction."
  (defun my-spacemacs/shell-pop-eshell ()
    (interactive)
    (spacemacs/shell-pop-eshell nil)
    (if (string= major-mode "eshell-mode")
        (evil-insert 1)
      (evil-escape)))

  (evil-global-set-key 'normal (kbd "C-e") 'my-spacemacs/shell-pop-eshell)
  (evil-global-set-key 'insert (kbd "C-e") 'my-spacemacs/shell-pop-eshell))

;;;; Windows
(defun dotspacemacs/user-config/misc/windows ()
  (global-set-key (kbd "M-d") 'spacemacs/delete-window))

;;;; Yassnippet
(defun dotspacemacs/user-config/misc/yassnippet ()
  (global-set-key (kbd "C-SPC") 'hippie-expand))

;;; Python
(defun dotspacemacs/user-config/python ()
  (with-eval-after-load 'python
    (unless-linux-call 'dotspacemacs/user-config/python/windows-pytest)
    (dotspacemacs/user-config/python/fixes)
    (dotspacemacs/user-config/python/mypy)
    (dotspacemacs/user-config/python/venvs)))

;;;; Windows-pytest
(defun dotspacemacs/user-config/python/windows-pytest ()
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
(defun dotspacemacs/user-config/python/fixes ()
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

  ;; Remove flyspell from python buffers
  (dolist (hook '(python-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1)))))

;;;; Mypy
(defun dotspacemacs/user-config/python/mypy ()
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
(defun dotspacemacs/user-config/python/venvs ()
  (with-eval-after-load 'virtualenvwrapper
    (pyvenv-mode 1)

    ;; Fixes hy-mode environment when pyvenv is activated
    (add-hook 'pyvenv-post-activate-hooks 'python/init-hy-mode)

    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)))

;;; Org
(defun dotspacemacs/user-config/org ()
  (with-eval-after-load 'org
    (when-linux-call 'dotspacemacs/user-config/org/linux-file-apps)
    (dotspacemacs/user-config/org/agenda)
    (dotspacemacs/user-config/org/babel)
    (dotspacemacs/user-config/org/exports)
    (dotspacemacs/user-config/org/misc)
    (dotspacemacs/user-config/org/templates)
    (dotspacemacs/user-config/org/theming)))

;;;; Linux-file-apps
(defun dotspacemacs/user-config/org/linux-file-apps ()
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . "/usr/bin/firefox %s")
                        ("\\.pdf\\'" . default))))

;;;; Agenda
(defun dotspacemacs/user-config/org/agenda ()
  ;; Agenda workflow integration being investigated
  ;; (setq org-agenda-files '("c:/~/.org"))
  )

;;;; Babel
(defun dotspacemacs/user-config/org/babel ()
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
(defun dotspacemacs/user-config/org/exports ()
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

;;;; Misc
(defun dotspacemacs/user-config/org/misc ()
  ;; Useful header navigation binding inspired from outline-mode
  (evil-define-key '(normal visual motion) org-mode-map
    "gu" 'outline-previous-visible-heading)

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
  (add-hook 'org-mode-hook 'flyspell-mode))

;;;; Templates
(defun dotspacemacs/user-config/org/templates ()
  (mapc (lambda (x) (add-to-list 'org-structure-template-alist x))
        (list
         ;; Name block
         '("n" "#+NAME: ?")
         ;; Language Blocks
         '("c" "#+begin_src clojure\n\n#+end_src")
         '("e" "#+begin_src emacs-lisp\n\n#+end_src")
         '("h" "#+begin_src haskell\n\n#+end_src")
         '("p" "#+begin_src python\n\n#+end_src")
         ;; Graphviz Block
         '("d" "#+begin_src dot\n\n#+end_src")
         ;; Collapse previous header by default in themed html export
         '("clps" ":PROPERTIES:\n :HTML_CONTAINER_CLASS: hsCollapsed\n :END:\n")
         )))

;;;; Theming
(defun dotspacemacs/user-config/org/theming ()
  (setq org-bullets-bullet-list '("■" "○" "✸" "✿")
        org-priority-faces '((65 :foreground "red")
                             (66 :foreground "yellow")
                             (67 :foreground "blue"))
        org-ellipsis "▼"))

;;; Outshine
(defun dotspacemacs/user-config/outshine ()
  ;; Group 1
  (dotspacemacs/user-config/outshine/navi-mode)

  ;; Rest
  (dotspacemacs/user-config/outshine/outshine-mode))

;;;; Navi-mode
(defun dotspacemacs/user-config/outshine/navi-mode ()
  ;; TODO doesnt work well with more than 2 windows
  ;; TODO if navi buffer exists then M-n doesnt do open window

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
(defun dotspacemacs/user-config/outshine/outshine-mode ()
  (require 'outshine)
  ;; 1. Adds functionality to run on narrowed buffers
  ;; 2. Shows up to including level 3 headings on load

  (defun my-outshine-navi ()
    ;; TODO Couldnt get popwin to work
    ;; However, managed neotree-like behavior but not 100% consistent
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
    ;; (define-key map (kbd "s-n") 'my-outshine-navi)
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

;;; GNUs
(defun dotspacemacs/user-config/gnus ()
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

;;; Org-gcal
(defun dotspacemacs/user-config/org-gcal ()
  ;; TODO use dropbox
  ;; TODO bind everything
  ;; TODO try calfw
  ;; TODO improve the load
  ;; https://github.com/myuhe/org-gcal.el
  ;; http://cestlaz.github.io/posts/using-emacs-26-gcal/#.WG52MOtj0wE.reddit

  (require 'org-gcal)
  (require 'org-contacts)
  (load (if-linux "~/Dropbox/secrets.el"
                  "c:/~/Dropbox/secrets.el"))
  (setq org-gcal-file-alist
        `(("ekaschalk@gmail.com" .
           ,(if-linux "~/Dropbox/schedule.org" "c:/~/Dropbox/schedule.org"))))
  (setq org-contacts-files
        `(,(if-linux "~/Dropbox/contacts.org" "c:/~/Dropbox/contacts.org")))

  ;; (org-gcal-sync)
  ;;   (add-to-list 'org-capture-templates
  ;;                '("c" "Contacts" entry (file "~/Org/contacts.org")
  ;;                  "* %(org-contacts-template-name)
  ;; :PROPERTIES:
  ;; :EMAIL: %(org-contacts-template-email)
  ;; :END:"))
  )
