;; -*- mode: emacs-lisp -*-

;;; Introduction

;; -- Eric Kaschalk's Spacemacs Configuration Organization --

;; Literate configs with org-mode are not natively supported by spacemacs
;; due to how org-mode is loaded within spacemacs layers systems.

;; The approach taken is to use the `outline-minor-mode` in conjuction
;; with `outshine-mode` and `navi-mode` to maintain benefits of literate
;; documentation and org-modes navigation, collapsing, and narrowing facilities.

;;; OS-Config
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
        helm
        org
        ranger
        syntax-checking
        (auto-completion :variables
                         auto-completion-return-key-behavior 'complete
                         auto-completion-tab-key-behavior 'complete
                         auto-completion-enable-snippets-in-popup t)
        (evil-snipe :variables
                    evil-snipe-enable-alternate-f-and-t-behaviors t)
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
      '(;; Markdown mode for viewing outside documentation
        markdown
        ;; Graphviz mode for usage with org-babel
        graphviz
        ;; REST client for usage with org-babel
        restclient)

      ;; OS-Specific and Local Packages
      dotspacemacs/layers/windows
      '(pandoc)
      dotspacemacs/layers/linux
      '(pdf-tools)
      dotspacemacs/layers/local
      '()
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
   dotspacemacs-themes '(spacemacs-dark
                         doom-vibrant)
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
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
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
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ())

;;; Spacemacs-Config
;;;; Display
(defun dotspacemacs/user-config/display ()
  ;; Group 1
  (unless-linux-call 'dotspacemacs/user-config/display/windows-frame-size-fix)
  (dotspacemacs/user-config/display/init-theme)

  ;; Group 2
  ;; (dotspacemacs/user-config/display/fira-code-ligatures)
  (dotspacemacs/user-config/display/font-locks)
  ;; (dotspacemacs/user-config/display/my-ligatures)

  ;; Rest
  (dotspacemacs/user-config/display/prettify-symbols)
  ;; (dotspacemacs/user-config/display/select-ligatures)
  (dotspacemacs/user-config/display/face-updates)

  (dotspacemacs/user-config/display/modeline)
  (dotspacemacs/user-config/display/all-the-icons)

  (setq neo-theme 'icons
        neo-window-width 28)
  )

;;;;; Windows-frame-size-fix
(defun dotspacemacs/user-config/display/windows-frame-size-fix ()
  "Surface uses 200% scaling, doesn't transfer to emacs, this fixes with `f2`."
  (add-to-list 'default-frame-alist '(font . "Hack"))
  (set-face-attribute 'default t :font "Hack")
  (global-set-key (kbd "<f2>")
                  (lambda () (interactive) (mapc (lambda (x) (zoom-frm-out)) '(1 2)))))

;;;;; Init-Theme
(defun dotspacemacs/user-config/display/init-theme ()
  "Doom theme must be initialized early in config or font breaks."
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-one-brighter-modeline nil)

  ;; Doom buffer mode tints distinguishes (*) buffers with a lighter background
  (add-hook 'find-file-hook
            #'doom-buffer-mode-maybe)
  (add-hook 'after-revert-hook
            #'doom-buffer-mode-maybe)
  (add-hook 'ediff-prepare-buffer-hook
            #'doom-buffer-mode)

  ;; TODO For some reason can't place in additional packages
  (spacemacs/cycle-spacemacs-theme))

;;;;; Modeline
(defun dotspacemacs/user-config/display/modeline ()
  (use-package spaceline-all-the-icons
    :after spaceline  ; eval-after-load doesn't work for this setup
    :config (progn
              ;; Initialization
              (spaceline-all-the-icons--setup-neotree)
              (spaceline-all-the-icons-theme)

              ;; Configuration
              (setq spaceline-highlight-face-func 'spaceline-highlight-face-default
                    powerline-default-separator 'arrow
                    spaceline-all-the-icons-icon-set-modified 'circle
                    spaceline-all-the-icons-icon-set-window-numbering 'solid
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
              (spaceline-toggle-hud-on))))

;;;;; All-the-icons
(defun dotspacemacs/user-config/display/all-the-icons ()
  "Add icon to all-the-icons for hylang for neotree and modeline integration."
  (with-eval-after-load 'all-the-icons
    ;; Both all-the-icons-icon-alist and all-the-icons-mode-icon-alist
    ;; Need to be updated for either modification to take effect.

    (add-to-list
     'all-the-icons-icon-alist
     '("\\.hy$" all-the-icons-fileicon "lisp" :face all-the-icons-orange))
    (add-to-list
     'all-the-icons-mode-icon-alist
     '(hy-mode all-the-icons-fileicon "lisp" :face all-the-icons-orange))))

;;;;; Face-updates
(defun dotspacemacs/user-config/display/face-updates ()
  (defun update-outline-font-faces ()
    (custom-theme-set-faces
     (car custom-enabled-themes)

     ;; Org-level-3 and org-level-2 were too similar with color-blindness
     '(org-level-3 ((t (:height 1.03 :foreground "light slate gray"
                                :weight ultra-bold))))

     ;; Since outlines are necessarily further apart than org-mode headers
     ;; We box the outlines to make them stand out in programming buffers.
     '(outline-1 ((t (:inherit org-level-1 :box t))))
     '(outline-2 ((t (:inherit org-level-2 :box t))))
     '(outline-3 ((t (:inherit org-level-3 :box t :height 1.03))))
     '(outline-4 ((t (:inherit org-level-4 :underline t))))))

  ;; Apply face updates on emacs initialization
  (update-outline-font-faces)
  ;; Apply face updates update whenever theme is toggled
  (add-hook 'spacemacs-post-theme-change-hook 'update-outline-font-faces))

;;;;; Fira-ligatures
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
    ;; Disabled
    ;; Dislike
    ;; ("\\(;;\\)"                    #Xe129)  ; Lisp `;;`, don't like spacing
    ;; ("\\(\\?\\?\\)"                #Xe128)  ; Comments `??`, don't like spacing
    ;; Conflicts with org-mode
    ;; ("[^/]\\(\\*\\*\\)[^/]"        #Xe101) ("\\(\\*\\*\\*\\)"             #Xe102)
    ;; ("\\(\\*\\*/\\)"               #Xe103) ("\\(/\\*\\)"                  #Xe12a)
    ;; ("\\(/\\*\\*\\)"               #Xe12b) ("[^*]\\(\\*/\\)"              #Xe105)
    ;; ("\\(\\[\\]\\)"                #Xe109) ;; ("\\(x\\)"                     #Xe16b)
  ))

;;;;; Outline-pairs
(defconst emacs-lisp-font-lock-alist
  '(("\\(^;;;\\)"                   ?■)
    ("\\(^;;;;\\)"                  ?○)
    ("\\(^;;;;;\\)"                 ?✸)
    ("\\(^;;;;;;\\)"                ?✿)))

(defconst python-font-lock-alist
  '(("\\(^# \\*\\)[ \t\n]"          ?■)
    ("\\(^# \\*\\*\\)[ \t\n]"       ?○)
    ("\\(^# \\*\\*\\*\\)[ \t\n]"    ?✸)
    ("\\(^# \\*\\*\\*\\*\\)[^\\*]"  ?✿)))

(defconst hy-font-lock-alist
  '(("\\(^;; \\*\\)[ \t\n]"          ?■)
    ("\\(^;; \\*\\*\\)[ \t\n]"       ?○)
    ("\\(^;; \\*\\*\\*\\)[ \t\n]"    ?✸)
    ("\\(^;; \\*\\*\\*\\*\\)[^\\*]"  ?✿)

    ("\\(self\\)"   ?⊙)
    ))

;;;;; Font-locks
(defun dotspacemacs/user-config/display/font-locks ()
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
  )

;;;;; Prettify-symbols
(defun dotspacemacs/user-config/display/prettify-symbols ()
  (set-fontset-font "fontset-default" '(#x2c7c . #x2c7c) "Courier New")
  (set-fontset-font "fontset-default" '(#x1d518 . #x1d57f) "Symbola")
  (set-fontset-font "fontset-default" '(#x1d4d0 . #x1d4e2) "Symbola")
  (set-fontset-font "fontset-default" '(#x1d4d0 . #x1d54a) "Symbola")
  (set-fontset-font "fontset-default" '(#x1d54a . #x1d572) "Symbola")

;;;;;; Utils

  (defun prettify-utils--list (l &optional glue)
    "Takes two lists and interleaves the (optional) second between each element of
the first.  Used to create multi-character sequences for use with the minor mode
'prettify-symbols'.  If not supplied, GLUE defaults to '(Br . Bl).  For more
information about GLUE, refer to the documentation for the 'compose-region
function and the 'reference-point-alist variable.
This function is used by prettify-utils-string to create the lists given to
prettify-symbols-alist.  Calling prettify-utils--list directly is probably not
what you want, check the documentation for prettify-utils-string and
prettify-utils-generate instead.
Example use:
(prettify-utils--list (string-to-list \"hello\") '(Br . Bl))
"

  (let ((glue (or glue '(Br . Bl)))
    (head (car l))
    (tail (cdr l)))
  (cond
   ((not (consp l))    '())
   ((not (consp tail))  (list head))
   (t (cons head
        (cons glue
          (prettify-utils--list tail glue)))))))

 (defun prettify-utils-string (s &optional glue)
  "Takes a string and an optional list, and returns a list of the string's
characters with GLUE interleaved between each character, for use with
prettify-symbols mode.  If no GLUE is supplied, uses the
prettify-utils--list default.  For more information about GLUE, refer to the
documentation for the 'compose-region function and the 'reference-point-alist
variable.
This function can be used to simplify multiple-character replacements when
manually constructing a prettify-symbols-alist.  For something more high-level,
consider using prettify-utils-generate to create the entire alist instead.
Example:
(prettify-utils-string \"example\" '(Br . Bl))
"
  (prettify-utils--list (append s nil) glue))

;; Was used during macro creation then removed
(defun prettify-utils-create-pair (old new &optional glue)
  "Takes two strings, OLD and NEW, and an optional GLUE list, and creates an
alist pair for use when creating a prettify-symbols-alist.  For more information
about GLUE, refer to the documentation for the 'compose-region function and the
'reference-point-alist variable.
This function is primarily for use by the user-friendly 'prettify-utils-generate
macro, but may be useful if manual alist creation is desired for some reason.
Example:
(setq prettify-symbols-alist `((\">=\" ?≥)
                               ,(prettify-utils-create-pair \"foo\" \"bar\" '(Br . Bl))))
"
  (cons old (prettify-utils-string new glue)))

(defmacro prettify-utils-generate (&rest pairs)
  "Generates an alist for use when setting prettify-symbols-alist.  Takes one or
more lists, each consisting of two strings and an optional GLUE list to be
interleaved between characters in the replacement list.  If the optional GLUE
list is not supplied, uses the prettify-list default of '(Br . Bl).  For more
information about GLUE, refer to the documentation for the 'compose-region
function and the 'reference-point-alist variable.
Example #1:
(setq prettify-symbols-alist
      (prettify-utils-generate (\"foo\" \"bar\")
                               (\">=\" \"≥\" (Br . Bl))
                               (\"->\"     \"→ \")))
Example #2:
(setq prettify-symbols-alist
      (prettify-generate
       (\"lambda\"  \"λ\")
       (\"|>\"      \"▷\")
       (\"<|\"      \"◁\")
       (\"->>\"     \"↠  \")
       (\"->\"      \"→ \")
       (\"<-\"      \"← \")
       (\"=>\"      \"⇒\")
       (\"<=\"      \"≤\")
       (\">=\"      \"≥\")))
"
  (let* ((head       (car   pairs))
         (tail       (cdr   pairs))
         (old-string (car   head))
     (new-string (cadr  head))
     (glue-list  (caddr head)))
  (if (not (consp head))
    '()
       `(cons (quote ,(prettify-utils-create-pair old-string new-string glue-list))
       (prettify-utils-generate ,@tail)))))

(defun prettify-utils-generate-f (&rest pairs)
  "Generates an alist for use when setting prettify-symbols-alist.  Takes one or
more lists, each consisting of two strings and an optional GLUE list to be
interleaved between characters in the replacement list.  If the optional GLUE
list is not supplied, uses the prettify-list default of '(Br . Bl).  For more
information about GLUE, refer to the documentation for the 'compose-region
function and the 'reference-point-alist variable.
This is a function equivalent of the prettify-utils-generate macro.  Unless
you specifically need a function, such as for use with a higher-order function,
you should use the 'prettify-utils-generate macro instead.
Example:
(prettify-utils-generate-f '(\"foo\" \"bar\")
                           '(\">=\" \"≥\" (Br . Bl))
                           '(\"->\"     \"→ \"))
"
  (let* ((head       (car   pairs))
         (tail       (cdr   pairs))
         (old-string (car   head))
     (new-string (cadr  head))
     (glue-list  (caddr head)))
  (if (not (consp head))
    '()
      (cons (prettify-utils-create-pair old-string new-string glue-list)
(apply 'prettify-utils-generate-f tail)))))

;;;;;; Main

(global-prettify-symbols-mode 1)

(set-fontset-font t '(#x2a02 . #x2a02) "Symbola")
(set-fontset-font t '(#x2205 . #x2205) "Symbola")
(set-fontset-font t '(#x27fb . #x22fc) "Symbola")
(set-fontset-font t '(#x2299 . #x2299) "Symbola")


(defun hy-pretty-symbols-alist ()
  (setq prettify-symbols-alist
        (prettify-utils-generate
         ("fn"      "λ")
         ("defn"    "𝓕")
         ("#t"      "⨂")
         ("ap-pipe" " ")
         ("True"    "𝕋")
         ("False"   "𝔽")
         ("None"    "∅")
         ;; ("map"     " ?")
         ;; ("*map"    " ?")j
         ;; ("#a"      " ")
         ;; ("or" )
         ;; ("and" )
         )))


(add-hook 'hy-mode-hook 'hy-pretty-symbols-alist)

(defun add-hy-kws()
  (font-lock-add-keywords
   nil '(
         ("\\<\\(self\\)" . 'font-lock-keyword-face)
         ("\\<\\(staticmethod\\)\\>" . 'font-lock-function-name-face)
         ("\\<\\(classmethod\\)\\>" . 'font-lock-function-name-face)
         ("\\<\\(property\\)\\>" . 'font-lock-function-name-face)
         ("\\<\\(composite\\)\\>" . 'font-lock-function-name-face)
         ("\\<\\(import\\)\\>" . 'font-lock-function-name-face)
         ("\\<\\(require\\)\\>" . 'font-lock-function-name-face)

         ("\\(#.\\)" . 'font-lock-function-name-face)
         )))

(add-hook 'hy-mode-hook 'add-hy-kws)

;; (defun test-fontlock-fix-before ()
;;   (font-lock-mode -1)
;;   (spacemacs/indent-region-or-buffer))

;; (defun test-fontlock-fix-after ()
;;   (font-lock-mode 1)
;;   (sit-for 1)
;;   (spacemacs/indent-region-or-buffer))

;; (add-hook 'before-save-hook 'test-fontlock-fix-before)
;; (add-hook 'after-save-hook 'test-fontlock-fix-after)


;; http://unicode.mayastudios.com/
(add-hook 'python-mode-hook
          (lambda ()
            (mapc (lambda (pair) (push pair prettify-symbols-alist))
                  '(;; Syntax
                    ("self" .     #x2299)   ; ⊙
                    ("def" .      #x1d4d5)  ; 𝓕
                    ("not" .      #xffe2)   ; ￢
                    ("for" .      #x2200)   ; ∀
                    ("in" .       #x2208)   ; ∈
                    ("not in" .   #x2209)   ; ∉
                    ("return" .   #x27fc)   ; ⟼
                    ("yield" .    #x27fb)   ; ⟻

                    ;; Types (Base)
                    ("int" .      #x2124)   ; ℤ
                    ("float" .    #x211d)   ; ℝ
                    ("str" .      #x1d54a)  ; 𝕊
                    ("bool" .     #x1d539)  ; 𝔹
                    ("True" .     #x1d54b)  ; 𝕋
                    ("False" .    #x1d53d)  ; 𝔽
                    ;; Types (Containers)
                    ;; ("list" .    #x1d543)   ; 𝕃
                    ;; ("dict" .    #x1d53b)   ; 𝔻

                    ;; Mypy (Abstract Types)
                    ("Callable" . #x2131)   ; ℱ
                    ("Mapping" .  #x2133)   ; ℳ
                    ("Iterable" . #x1d517)  ; 𝔗
                    ;; Mypy (Containers)
                    ("Dict" .     #x1d507)  ; 𝔇  𝓓
                    ("List" .     #x2112)   ; ℒ  𝓛
                    ("Generator" . #x1d50a) ; 𝔊  𝓖
                    ("Set" .      #x2126)   ; Ω  𝓢
                    ;; Mypy (operators, symbols)
                    ("Tuple" .    #x2a02)   ; ⨂
                    ("Union" .    #x22c3)   ; ⋃
                    ("Any" .      #x2754)   ; ❔

                    ;; Exploring
                    ("tz.pipe" .  #Xe135)   ; 
                    ;; ("tz.thread_first" . #Xe13e)  ; =>
                    ;; ("tz.thread_last" . #Xe140)   ; =>>
                    ))))

(global-pretty-mode t)

(pretty-deactivate-groups  ; Replaced by Fira Code
 '(:equality :ordering :ordering-double :ordering-triple
             :arrows :arrows-twoheaded :punctuation
             :logic :sets :sub-and-superscripts))

(pretty-activate-groups  ; :greek not enabled breaks 'Mapping' prettify symbol
 '(:arithmetic-nary))
)

;;;; Configuration
(defun dotspacemacs/user-config/configuration ()
  (dotspacemacs/user-config/configuration/evil)
  (dotspacemacs/user-config/configuration/editing)
  (dotspacemacs/user-config/configuration/visual))

;;;;; Evil
(defun dotspacemacs/user-config/configuration/evil ()
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-unordered-key-sequence "true"))

;;;;; Editing
(defun dotspacemacs/user-config/configuration/editing ()
  (hungry-delete-mode 1)  ; in edit mode back gets all contiguous whitespace
  (spacemacs/toggle-aggressive-indent-globally-on)  ; auto-indentation
  (add-hook 'org-mode-hook (lambda () (auto-fill-mode 1))))  ; SPC splits past 80

;;;;; Visual
(defun dotspacemacs/user-config/configuration/visual ()
  (spacemacs/toggle-highlight-long-lines-globally-on)
  (fringe-mode '(1 . 1))  ; Minimal left padding and ~ end newline markers
  (rainbow-delimiters-mode-enable)  ; Paren color based on depth
  (global-highlight-parentheses-mode 1)  ; Highlight containing parens
  (spacemacs/toggle-mode-line-minor-modes-off))  ; no unicode symbs next to major

;;;; Navigation
(defun dotspacemacs/user-config/navigation ()
  (dotspacemacs/user-config/navigation/avy)
  (dotspacemacs/user-config/navigation/file-links)

  ;; Think about S-hjkl as windows movement commands
  ;; (its available right now in spc w . transient mode)

  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             (make-variable-buffer-local 'evil-snipe-aliases)
  ;;             (push '(?: "def .+:") evil-snipe-aliases)))

  ;; (add-hook 'hy-mode-hook
  ;;           (lambda ()
  ;;             (make-variable-buffer-local 'evil-snipe-aliases)
  ;;             (push '(?: "defn .+:") evil-snipe-aliases)))
  )

;;;;; Avy
(defun dotspacemacs/user-config/navigation/avy ()
  (global-set-key (kbd "C-h") 'avy-pop-mark)
  (global-set-key (kbd "C-j") 'evil-avy-goto-char-2)
  (global-set-key (kbd "C-k") 'evil-avy-goto-word-or-subword-1)
  (global-set-key (kbd "C-l") 'evil-avy-goto-line)

  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-j") 'evil-avy-goto-char-2)
    (define-key org-mode-map (kbd "C-k") 'evil-avy-goto-word-or-subword-1))

  (with-eval-after-load 'python
    (evil-define-key '(normal insert visual replace operator motion emacs)
      python-mode-map (kbd "C-j") 'evil-avy-goto-char-2)))

;;;;; File-links
(defun dotspacemacs/user-config/navigation/file-links ()
  (define-key evil-normal-state-local-map (kbd "SPC a o f")
    'org-open-at-point-global))

;;;; Misc
(defun dotspacemacs/user-config/misc ()
  (dotspacemacs/user-config/misc/aspell)
  (dotspacemacs/user-config/misc/auto-completion)
  (dotspacemacs/user-config/misc/projectile)
  (dotspacemacs/user-config/misc/yassnippet)
  (when-linux-call 'dotspacemacs/user-config/misc/spotify)

  (evil-global-set-key 'normal (kbd "L") 'evil-end-of-line)
  (evil-global-set-key 'visual (kbd "L")
                       (lambda () (interactive)  ; different than 'evil-end-of-line
                         (evil-end-of-line)))
  (evil-global-set-key 'motion (kbd "L") 'evil-end-of-line)

  (evil-global-set-key 'normal (kbd "H") 'evil-first-non-blank)
  (evil-global-set-key 'visual (kbd "H") 'evil-first-non-blank)
  (evil-global-set-key 'motion (kbd "H") 'evil-first-non-blank)

  (evil-global-set-key 'normal (kbd "Q")
                       (lambda () (interactive) (evil-execute-macro 1 "@q")))
  (evil-global-set-key 'normal (kbd "C-f") 'winum-select-window-0)
  (evil-global-set-key 'normal (kbd "C-p") 'neotree-find-project-root)


  ;; CLOJURE
  (spacemacs/set-leader-keys-for-major-mode
    'clojure-mode (kbd ",") 'lisp-state-toggle-lisp-state)
  (spacemacs/set-leader-keys-for-major-mode
    'hy-mode (kbd ",") 'lisp-state-toggle-lisp-state)


  )

;;;;; Aspell
(defun dotspacemacs/user-config/misc/aspell ()
  (setq ispell-program-name "aspell"))

;;;;; Auto-completion
(defun dotspacemacs/user-config/misc/auto-completion ()
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))

;;;;; Projectile
(defun dotspacemacs/user-config/misc/projectile ()
  (setq projectile-indexing-method 'native))  ; respect .projectile files

;;;;; Yassnippet
(defun dotspacemacs/user-config/misc/yassnippet ()
  (global-set-key (kbd "C-SPC") 'hippie-expand))

;;;;; Spotify
(defun dotspacemacs/user-config/misc/spotify ()
  (global-set-key (kbd "C-c s s") 'helm-spotify-plus)
  (global-set-key (kbd "C-c s n") 'helm-spotify-plus-next)
  (global-set-key (kbd "C-c s N") 'helm-spotify-plus-previous)
  (global-set-key (kbd "C-c s f") 'helm-spotify-plus-play)
  (global-set-key (kbd "C-c s F") 'helm-spotify-plus-pause))

;;;; Python
(defun dotspacemacs/user-config/python ()
  ;; (when-linux-call 'dotspacemacs/user-config/python/linux)
  (dotspacemacs/user-config/python/linux)
  (unless-linux-call 'dotspacemacs/user-config/python/windows-pytest)
  (dotspacemacs/user-config/python/venvs)
  (dotspacemacs/user-config/python/mypy)

  ;; (defadvice python-shell-send-region)
  )

;;;;; Mypy
(defun dotspacemacs/user-config/python/mypy ()
  (flycheck-define-checker python-mypy ""
                           :command ("mypy"
                                     "--ignore-missing-imports" "--fast-parser"
                                     "--python-version" "3.6"
                                     source-original)
                           :error-patterns
                           ((error line-start (file-name) ":" line ": error:" (message) line-end))
                           :modes python-mode)

  (add-to-list 'flycheck-checkers 'python-mypy t)
  (flycheck-add-next-checker 'python-pylint 'python-mypy t))

;;;;; Windows-pytest
(defun dotspacemacs/user-config/python/windows-pytest ()
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

;;;;; Linux
(defun dotspacemacs/user-config/python/linux ()
  (with-eval-after-load 'python
    (defun python-shell-completion-native-try ()
      "Return non-nil if can trigger native completion."
      (let ((python-shell-completion-native-enable t)
            (python-shell-completion-native-output-timeout
             python-shell-completion-native-try-output-timeout))
        (python-shell-completion-native-get-completions
         (get-buffer-process (current-buffer))
         nil "_"))))

  (dolist (hook '(python-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1)))))

;;;;; Venvs
(defun dotspacemacs/user-config/python/venvs ()
  (require 'virtualenvwrapper)
  (pyvenv-mode 1)
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)

  (defun pyvenv-autoload ()
    (when (string= buffer-file-name "c:/~/dev/pop-synth/base.org")
      (pyvenv-workon "pop-synthvenv"))
    (when (string= buffer-file-name "c:/~/dev/health/base.org")
      (pyvenv-workon "healthvenv")))

  (add-hook 'org-mode-hook 'pyvenv-autoload))

;;;; Org
(defun dotspacemacs/user-config/org ()
  (dotspacemacs/user-config/org/core)
  (when-linux-call 'dotspacemacs/user-config/org/core-linux)
  (dotspacemacs/user-config/org/babel)
  (dotspacemacs/user-config/org/exporting)
  (dotspacemacs/user-config/org/templates))

;;;;; Core
(defun dotspacemacs/user-config/org/core ()
  ;; Agenda in-progress
  (setq org-agenda-files '("c:/~/.org" "c:/~/dev/pop-synth/base.org"))


  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (setq org-bullets-bullet-list '("■" "○" "✸" "✿")
        org-priority-faces '((65 :foreground "red")
                             (66 :foreground "yellow")
                             (67 :foreground "blue")))

  (setq org-refile-targets (quote ((nil :regexp . "Week of"))))

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
         (mapcar (lambda (c) (make-glyph-code c 'font-lock-keyword-face)) "▼"))))))

  (setq org-ellipsis "▼")

  (defvar org-blocks-hidden nil)
  (defun org-toggle-blocks ()
    (interactive)
    (if org-blocks-hidden
        (org-show-block-all)
      (org-hide-block-all))
    (setq-local org-blocks-hidden (not org-blocks-hidden)))

  (add-hook 'org-mode-hook 'flyspell-mode)  ; Async python, spelling
  (add-hook 'org-mode-hook 'org-toggle-blocks)
  (define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)

  (evil-define-key '(normal visual motion) org-mode-map
    "gu" 'outline-previous-visible-heading)
  )

;;;;; Core-linux
(defun dotspacemacs/user-config/org/core-linux ()
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . "/usr/bin/firefox %s")
                        ("\\.pdf\\'" . default))))

;;;;; Babel
(defun dotspacemacs/user-config/org/babel ()
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window)
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (dot . t)
                               (http . t)
                               (haskell . t))))

;;;;; Exporting
(defun dotspacemacs/user-config/org/exporting ()
  (require 'ox-bibtex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-html-htmlize-output-type 'inline-css
        org-latex-listings 'minted
        org-latex-minted-options
        '(("frame" "lines")
          ("fontsize" "\\scriptsize")
          ("xleftmargin" "\\parindent")
          ("linenos" ""))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

;;;;; Templates
(defun dotspacemacs/user-config/org/templates ()
  (mapc (lambda (x) (add-to-list 'org-structure-template-alist x))
        (list
         ;; Common
         '("n" "#+NAME: ?")
         ;; Haskell
         '("h" "#+begin_src haskell\n\n#+end_src")
         ;; Emacs-Lisp
         '("e" "#+begin_src emacs-lisp\n\n#+end_src")
         ;; Python
         '("p" "#+begin_src python\n\n#+end_src")
         '("pd" "#+begin_src python :tangle no :results output\n\n#+end_src")
         '("pt" "#+begin_src python :results silent :exports none\n\n#+end_src")
         ;; Misc
         '("c" " :PROPERTIES:\n :HTML_CONTAINER_CLASS: hsCollapsed\n :END:\n")
         `("d" ,(concat
                 "#+begin_src dot :tangle no :exports results :file static/imgs/"
                 "\n\n#+end_src"))
         ;; Project File header
         `("f" ,(concat
                 "# -*- org-use-tag-inheritance: nil"
                 " org-babel-use-quick-and-dirty-noweb-expansion: t-*-\n"
                 "#+BEGIN_QUOTE\n#+PROPERTY: header-args :eval never-export"
                 " :noweb no-export\n#+PROPERTY: header-args:python"
                 " :tangle (ek/file-path)\n#+END_QUOTE\n")))))

;;;; Outshine
(defun dotspacemacs/user-config/outshine ()
  (require 'outshine)
  (require 'navi-mode)

  (dotspacemacs/user-config/outshine/navi-mode)
  (dotspacemacs/user-config/outshine/outshine-mode))

;;;;; Navi-mode
(defun dotspacemacs/user-config/outshine/navi-mode ()
  (add-to-list 'navi-key-mappings
               '("python" .
                 ((:FUN . "f")
                  (:OBJ . "x"))))

  (add-to-list 'navi-keywords
               '("python" .
                 ((:FUN . "\\(^[ ]*def[a-zA-Z0-9_ ]*\\|^[ ]*class[a-zA-Z0-9_ ]*\\)")
                  (:OBJ . "^[ ]*\\(class[a-zA-Z0-9_ ]*\\)"))))

  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'navi-cycle-subtree)
    (define-key map (kbd "<backtab>") 'navi-cycle-buffer)

    (define-key map (kbd "M-h") 'navi-promote-subtree)
    (define-key map (kbd "M-j") 'navi-move-down-subtree)
    (define-key map (kbd "M-k") 'navi-move-up-subtree)
    (define-key map (kbd "M-l") 'navi-demote-subtree)
    (define-key map (kbd "M-n") 'navi-goto-occurrence-other-window)

    (evil-define-key '(normal visual motion) map
      "f" (lambda () (interactive) (navi-generic-command ?f current-prefix-arg)) ;Fun
      "v" (lambda () (interactive) (navi-generic-command ?v current-prefix-arg)) ;Var
      "x" (lambda () (interactive) (navi-generic-command ?x current-prefix-arg)) ;Obj
      "a" (lambda () (interactive) (navi-generic-command ?a current-prefix-arg)) ;All
      "1" (lambda () (interactive) (navi-generic-command ?1 current-prefix-arg))
      "2" (lambda () (interactive) (navi-generic-command ?2 current-prefix-arg))
      "3" (lambda () (interactive) (navi-generic-command ?3 current-prefix-arg))
      "4" (lambda () (interactive) (navi-generic-command ?4 current-prefix-arg))

      ;; TODO Check out org-reveal

      "u" 'navi-undo
      "n" (lambda () (interactive) (navi-narrow-to-thing-at-point)
            (other-window 1) (outline-show-entry) (outline-show-branches)
            (other-window 1))
      "w" 'navi-widen

      "d" (lambda () (interactive) (occur-mode-display-occurrence)
            (other-window 1) (outline-show-entry) (outline-show-branches)
            (other-window 1))
      "D" (lambda () (interactive) (occur-mode-display-occurrence)
            (other-window 1) (outline-show-entry) (outline-show-branches)
            (recenter 3) (other-window 1))

      "o" (lambda () (interactive) (navi-goto-occurrence-other-window)
            (outline-show-entry) (outline-show-branches))
      "O" (lambda () (interactive) (navi-goto-occurrence-other-window)
            (outline-show-entry) (outline-show-branches) (recenter 3))

      "q" (lambda () (interactive) (navi-quit-and-switch)
            (outline-show-entry) (outline-show-branches) (recenter 3))
      "Q" (lambda () (interactive) (navi-quit-and-switch)
            (delete-other-windows) (outline-show-entry) (outline-show-branches)
            (recenter 3)))

    (setq navi-mode-map map)))

;;;;; Outshine-mode
(defun dotspacemacs/user-config/outshine/outshine-mode ()
  (defun my-outshine-navi ()
    (interactive)
    (let ((line nil))
      (widen)  ; Otherwise broken on narrowed buffers
      (save-excursion
        (unless (outline-on-heading-p t)
          (outline-previous-visible-heading 1))
        (setq line
              (replace-regexp-in-string "\n$" ""
                                        (thing-at-point 'line t))))
      (outshine-navi)
      (navi-generic-command ?3 nil)  ; default to 3 heading levels
      (search-forward-regexp line)))

  ;; Org doesnt use outline minor mode but can utilize navi
  (define-key org-mode-map (kbd "M-n") 'my-outshine-navi)

  ;; Outline minor mode vim keybindings
  (let ((map outline-minor-mode-map))
    (define-key map (kbd "M-n") 'my-outshine-navi)

    (define-key map (kbd "C-M-<return>")  ; insert-subheading
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

    (define-key map (kbd "M-RET") 'outshine-insert-heading)
    (define-key map (kbd "<backtab>") 'outshine-cycle-buffer)
    (define-key map (kbd "M-h") 'outline-promote)
    (define-key map (kbd "M-l") 'outline-demote)

    (evil-define-key '(normal visual motion) map
      "gh" 'outline-up-heading
      "gj" 'outline-forward-same-level
      "gk" 'outline-backward-same-level
      "gl" 'outline-next-visible-heading
      "gu" 'outline-previous-visible-heading

      (kbd "SPC n n") (lambda ()
                        (interactive)
                        (save-excursion
                          (unless (outline-on-heading-p t)
                            (outline-previous-visible-heading 1))
                          (outshine-narrow-to-subtree)))
      (kbd "SPC n j") 'outline-move-subtree-down
      (kbd "SPC n k") 'outline-move-subtree-up))

  (setq outshine-use-speed-commands t)
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  )

;;;; Blog
(defun dotspacemacs/user-config/blog ()
  ;; Adapted from
  ;; http://whyarethingsthewaytheyare.com/setting-up-the-blog/#workflow
  ;; Requires pandoc layer and pandoc installed and on path

  ;; (concat "[\""
  ;;         (mapconcat
  ;;          'identity
  ;;          (remove ""
  ;;                  (split-string
  ;;                   (cdr (assoc "TAGS" properties)) ":"))
  ;;          "\",\"")
  ;;         "\"]")))

  (defun org-hugo-export ()
    (interactive)
    (save-excursion
      (unless (eq (org-current-level) 1)
        (outline-up-heading 10))
      ;; Set export format, pandoc options, post properties
      (let* ((org-pandoc-format 'markdown)
             (org-pandoc-options-for-markdown
              '((standalone . t) (atx-headers . t) (columns . 79)))
             (hl (org-element-at-point))
             (filename (org-element-property :EXPORT_TO hl))
             (title (concat "\"" (org-element-property :title hl) "\""))
             (slug (concat "\"" (org-element-property :SLUG hl) "\""))
             (date (concat "\"" (org-element-property :DATE hl) "\""))
             (categories "[\"emacs\"]")
             (tmp (concat (make-temp-name ".tmp") ".org")))
        (org-export-to-file 'pandoc
            (org-export-output-file-name tmp t)
          nil t nil nil nil
          (lambda (f) (org-pandoc-run-to-buffer-or-file f 'markdown t nil)))
        ;; Use advice-add to add advice to existing process sentinel
        ;; to modify file /after/ the export process has finished.
        (advice-add
         #'org-pandoc-sentinel
         :after
         `(lambda (process event)
            ;; Grab the file using with-temp-file, which saves our changes
            ;; after evaluation.
            (with-temp-file ,filename
              (insert-file-contents ,filename)
              (goto-char (point-min))
              ;; Remove default header
              (re-search-forward "---\\(.\\|\n\\)+?---\n\n")
              (replace-match "")
              (goto-char (point-min))
              ;; Insert new properties
              (insert
               (format
                "---\ntitle: %s\nslug: %s\ndate: %s\ncategories: %s\n---\n\n"
                ,title ,slug ,date ,categories))
              ;; Demote headings and tweak code blocks
              (dolist (reps '(("^#" . "##")
                              ("\n``` {\\.\\(.+?\\)}" . "```\\1")))
                (goto-char (point-min))
                (while (re-search-forward (car reps) nil t)
                  (replace-match (cdr reps))))))
         '((name . "hugo-advice")))
        ;; We don't want our advice to stick around afterwards
        (advice-remove #'org-pandoc-sentinel 'hugo-advice))))
  )
;;;; GNUs
(defun dotspacemacs/user-config/gnus ()
  (setq user-mail-address	"ekaschalk@gmail.com"
        user-full-name	"Eric Kaschalk")

  ;; Get email, and store in nnml
  (setq gnus-secondary-select-methods
        '((nnimap "gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl))
          (nntp "gmane"
                (nntp-address "news.gmane.org"))
          (nntp "news.gwene.org")))

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587 "ekaschalk@gmail.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  ;; Archive outgoing email in Sent folder on imap.gmail.com:
  (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
        gnus-message-archive-group "[Gmail]/Sent Mail")

  ;; set return email address based on incoming email address
  (setq gnus-posting-styles
        '(((header "to" "address@outlook.com")
           (address "address@outlook.com"))
          ((header "to" "address@gmail.com")
           (address "address@gmail.com"))))

  ;; store email in ~/gmail directory
  (setq nnml-directory "~/gmail")
  (setq message-directory "~/gmail")

  (setq mm-inline-large-images 'resize)  ; Full size images

  ;; https://github.com/paul-issartel/nnreddit
  )
;;;; Spacemacs
(defun dotspacemacs/user-config ()
  (with-eval-after-load 'dash
    ;; Group 1
    (dotspacemacs/user-config/display)

    ;; Rest
    (dotspacemacs/user-config/configuration)
    (dotspacemacs/user-config/misc)
    (dotspacemacs/user-config/navigation)
    (dotspacemacs/user-config/org)
    (dotspacemacs/user-config/python)
    (dotspacemacs/user-config/outshine)
    (dotspacemacs/user-config/blog)
    (dotspacemacs/user-config/gnus)))

;;; Spacemacs-Autogen
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(compilation-message-face (quote default))
 '(evil-want-Y-yank-to-eol t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(hl-sexp-background-color "#efebe9")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider seq queue clojure-mode spaceline-all-the-icons evil-snipe doom-vibrant-theme doom-themes all-the-icons memoize font-lock+ helm-spotify-plus multi intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode elfeed-web simple-httpd elfeed-org elfeed-goodies ace-jump-mode noflet elfeed pretty-mode pandoc-mode ox-pandoc ht flatland-theme tangotango-theme subatomic-theme spacegray-theme monokai-theme heroku-theme hc-zenburn-theme darkburn-theme cyberpunk-theme ample-theme ample-zen-theme color-theme-sanityinc-solarized material-theme mmm-mode markdown-toc markdown-mode gh-md multiple-cursors helm-company helm-c-yasnippet company-web web-completion-data company-statistics company-restclient know-your-http-well company-anaconda company auto-yasnippet yasnippet ac-ispell auto-complete navi-mode outshine outorg window-purpose imenu-list zenburn-theme yapfify xterm-color web-mode virtualenvwrapper unfill tagedit smeargle slim-mode shell-pop scss-mode sass-mode restclient-helm ranger pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements orgit org-projectile org-present org-pomodoro alert log4e gntp org-download ob-restclient restclient ob-http mwim multi-term magit-gitflow live-py-mode less-css-mode hy-mode htmlize helm-pydoc helm-gitignore helm-css-scss haml-mode graphviz-dot-mode gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor eshell-z eshell-prompt-extras esh-help emmet-mode diff-hl cython-mode anaconda-mode pythonic ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   (quote
    ((eval ek/startup-proj)
     (org-babel-use-quick-and-dirty-noweb-expansion . t)
     (org-use-tag-inheritance))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#bf616a")
     (40 . "#DCA432")
     (60 . "#ebcb8b")
     (80 . "#B4EB89")
     (100 . "#89EBCA")
     (120 . "#89AAEB")
     (140 . "#C189EB")
     (160 . "#bf616a")
     (180 . "#DCA432")
     (200 . "#ebcb8b")
     (220 . "#B4EB89")
     (240 . "#89EBCA")
     (260 . "#89AAEB")
     (280 . "#C189EB")
     (300 . "#bf616a")
     (320 . "#DCA432")
     (340 . "#ebcb8b")
     (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 108 :width normal))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
