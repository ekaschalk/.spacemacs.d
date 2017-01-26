;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     csv
     emacs-lisp
     helm
     html
     python
     django
     ipython-notebook
     git
     markdown
     org
     javascript
     syntax-checking
     version-control
     graphviz
     ranger
     (shell :variables shell-default-shell 'eshell)
     restclient
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(virtualenvwrapper)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of of
   ;; the form `(list-type . list-size)`. If nil it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         zenburn
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Code"
                               :size 14
                               :weight bold
                               :width condensed
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (add-to-list 'load-path "C:/Users/ekasc/~/org-ref")
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

  (defconst fira-code-font-lock-keywords-alist
    (mapcar (lambda (regex-char-pair)
              `(,(car regex-char-pair)
                (0 (prog1 ()
                     (compose-region (match-beginning 1)
                                     (match-end 1)
                                     ;; The first argument to concat is a string containing a literal tab
                                     ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
            '(("\\(www\\)"                   #Xe100)
              ;; ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
              ;; ("\\(\\*\\*\\*\\)"             #Xe102)
              ;; ("\\(\\*\\*/\\)"               #Xe103)
              ;; ("\\(\\*>\\)"                  #Xe104)
              ;; ("[^*]\\(\\*/\\)"              #Xe105)
              ("\\(\\\\\\\\\\)"              #Xe106)
              ("\\(\\\\\\\\\\\\\\)"          #Xe107)
              ("\\({-\\)"                    #Xe108)
              ("\\(\\[\\]\\)"                #Xe109)
              ("\\(::\\)"                    #Xe10a)
              ("\\(:::\\)"                   #Xe10b)
              ("[^=]\\(:=\\)"                #Xe10c)
              ("\\(!!\\)"                    #Xe10d)
              ("\\(!=\\)"                    #Xe10e)
              ("\\(!==\\)"                   #Xe10f)
              ("\\(-}\\)"                    #Xe110)
              ("\\(--\\)"                    #Xe111)
              ("\\(---\\)"                   #Xe112)
              ("\\(-->\\)"                   #Xe113)
              ("[^-]\\(->\\)"                #Xe114)
              ("\\(->>\\)"                   #Xe115)
              ("\\(-<\\)"                    #Xe116)
              ("\\(-<<\\)"                   #Xe117)
              ("\\(-~\\)"                    #Xe118)
              ("\\(#{\\)"                    #Xe119)
              ("\\(#\\[\\)"                  #Xe11a)
              ("\\(##\\)"                    #Xe11b)
              ("\\(###\\)"                   #Xe11c)
              ("\\(####\\)"                  #Xe11d)
              ("\\(#(\\)"                    #Xe11e)
              ("\\(#\\?\\)"                  #Xe11f)
              ("\\(#_\\)"                    #Xe120)
              ("\\(#_(\\)"                   #Xe121)
              ("\\(\\.-\\)"                  #Xe122)
              ("\\(\\.=\\)"                  #Xe123)
              ("\\(\\.\\.\\)"                #Xe124)
              ("\\(\\.\\.<\\)"               #Xe125)
              ("\\(\\.\\.\\.\\)"             #Xe126)
              ("\\(\\?=\\)"                  #Xe127)
              ("\\(\\?\\?\\)"                #Xe128)
              ("\\(;;\\)"                    #Xe129)
              ;; ("\\(/\\*\\)"                  #Xe12a)
              ;; ("\\(/\\*\\*\\)"               #Xe12b)
              ("\\(/=\\)"                    #Xe12c)
              ("\\(/==\\)"                   #Xe12d)
              ("\\(/>\\)"                    #Xe12e)
              ("\\(//\\)"                    #Xe12f)
              ("\\(///\\)"                   #Xe130)
              ("\\(&&\\)"                    #Xe131)
              ("\\(||\\)"                    #Xe132)
              ("\\(||=\\)"                   #Xe133)
              ("[^|]\\(|=\\)"                #Xe134)
              ("\\(|>\\)"                    #Xe135)
              ("\\(\\^=\\)"                  #Xe136)
              ("\\(\\$>\\)"                  #Xe137)
              ("\\(\\+\\+\\)"                #Xe138)
              ("\\(\\+\\+\\+\\)"             #Xe139)
              ("\\(\\+>\\)"                  #Xe13a)
              ("\\(=:=\\)"                   #Xe13b)
              ("[^!/]\\(==\\)[^>]"           #Xe13c)
              ("\\(===\\)"                   #Xe13d)
              ("\\(==>\\)"                   #Xe13e)
              ("[^=]\\(=>\\)"                #Xe13f)
              ("\\(=>>\\)"                   #Xe140)
              ("\\(<=\\)"                    #Xe141)
              ("\\(=<<\\)"                   #Xe142)
              ("\\(=/=\\)"                   #Xe143)
              ("\\(>-\\)"                    #Xe144)
              ("\\(>=\\)"                    #Xe145)
              ("\\(>=>\\)"                   #Xe146)
              ("[^-=]\\(>>\\)"               #Xe147)
              ("\\(>>-\\)"                   #Xe148)
              ("\\(>>=\\)"                   #Xe149)
              ("\\(>>>\\)"                   #Xe14a)
              ;; ("\\(<\\*\\)"                  #Xe14b)
              ;; ("\\(<\\*>\\)"                 #Xe14c)
              ("\\(<|\\)"                    #Xe14d)
              ("\\(<|>\\)"                   #Xe14e)
              ("\\(<\\$\\)"                  #Xe14f)
              ("\\(<\\$>\\)"                 #Xe150)
              ("\\(<!--\\)"                  #Xe151)
              ("\\(<-\\)"                    #Xe152)
              ("\\(<--\\)"                   #Xe153)
              ("\\(<->\\)"                   #Xe154)
              ("\\(<\\+\\)"                  #Xe155)
              ("\\(<\\+>\\)"                 #Xe156)
              ("\\(<=\\)"                    #Xe157)
              ("\\(<==\\)"                   #Xe158)
              ("\\(<=>\\)"                   #Xe159)
              ("\\(<=<\\)"                   #Xe15a)
              ("\\(<>\\)"                    #Xe15b)
              ("[^-=]\\(<<\\)"               #Xe15c)
              ("\\(<<-\\)"                   #Xe15d)
              ("\\(<<=\\)"                   #Xe15e)
              ("\\(<<<\\)"                   #Xe15f)
              ("\\(<~\\)"                    #Xe160)
              ("\\(<~~\\)"                   #Xe161)
              ("\\(</\\)"                    #Xe162)
              ("\\(</>\\)"                   #Xe163)
              ("\\(~@\\)"                    #Xe164)
              ("\\(~-\\)"                    #Xe165)
              ("\\(~=\\)"                    #Xe166)
              ("\\(~>\\)"                    #Xe167)
              ("[^<]\\(~~\\)"                #Xe168)
              ("\\(~~>\\)"                   #Xe169)
              ("\\(%%\\)"                    #Xe16a)
              ;;("\\(x\\)"                     #Xe16b)
              ("[^:=]\\(:\\)[^:=]"           #Xe16c)
              ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d))))
              ;; ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

  (defun add-fira-code-symbol-keywords ()
    (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

  (add-hook 'org-mode-hook
            #'add-fira-code-symbol-keywords)
  (add-hook 'prog-mode-hook
            #'add-fira-code-symbol-keywords)
  ;; Ruler
  (require 'fill-column-indicator)
  (setq fci-rule-width 1
        fci-rule-color "sea green"
        fci-rule-use-dashes t
        fci-dash-pattern 0.5)

  ;; Python
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

  ;; Projectile respects .projectile files
  (setq projectile-indexing-method 'native)

  ;; Org mode
  (require 'org-habit)
  (setq org-bullets-bullet-list '("■" "○" "✸" "✿"))
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/notes.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("r" "Run" entry (file+headline "~/org/notes.org" "Running Workouts")
           "* DONE %?\nCLOSED: %T--%T\n  %i")
          ("w" "Workout" entry (file+headline "~/org/notes.org" "Strength Workouts")
           "* DONE %?\nCLOSED: %T\--%Tn  %i")
          ("m" "Mobility" entry (file+headline "~/org/notes.org" "Mobility Workouts")
           "* DONE %?\nCLOSED: %T\--%Tn  %i")))

  ;; fontify code in code blocks
  (setq org-src-fontify-natively t)

  ;; Use minted
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)

  ;; Add the shell-escape flag
  (setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                ;; "bibtex %b"
                                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; Sample minted options.
  (setq org-latex-minted-options '(("frame" "lines")
                                   ("fontsize" "\\scriptsize")
                                   ("xleftmargin" "\\parindent")
                                   ("linenos" "")))

  ;; bib stuff
  (require 'ox-bibtex)
  (setq org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "bibtex %b"
                                "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; BABEL
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (dot . t)
     (http . t)))

  ;; Remove y/n check on execute src blocks
  (setq org-confirm-babel-evaluate nil)

  ;; TEMPLATES
  (add-to-list 'org-structure-template-alist
               '("e" "#+begin_src emacs-lisp\n\n#+end_src"))  ;; elisp block
  (add-to-list 'org-structure-template-alist
               '("d" "#+begin_src dot :tangle no :exports results :file static/imgs/\n\n#+end_src"))  ;; graphviz block
  (add-to-list 'org-structure-template-alist
               '("p" "#+begin_src python\n\n#+end_src"))  ;; python-module

  (add-to-list 'org-structure-template-alist
               '("pp" "#+begin_src python :tangle no\n\n#+end_src"))  ;; python-proc
  (add-to-list 'org-structure-template-alist
               '("pd" "#+begin_src python :tangle no :dir \"src\" :results output :exports code\n\n#+end_src"))  ;; python-driver block
  (add-to-list 'org-structure-template-alist
               '("pdr" "#+begin_src python :tangle no :dir \"src\" :results output :exports results\n\n#+end_src"))  ;; python-driver block
  (add-to-list 'org-structure-template-alist
               '("pt" "#+begin_src python :tangle (tangle-test-f) :exports none\n\n#+end_src"))  ;; python-test block

  (add-to-list 'org-structure-template-alist
               '("n" "#+NAME: ?"))
  (add-to-list 'org-structure-template-alist
               '("h" "#+ATTR_HTML: :width 680px\n"))  ;; lock html output size to screen width in bigblow theme
  (add-to-list 'org-structure-template-alist
               '("c" " :PROPERTIES:\n :HTML_CONTAINER_CLASS: hsCollapsed\n :END:\n"))  ;; collapse by default in bigblow html theme

  (add-to-list 'org-structure-template-alist
               '("f" "# -*- org-use-tag-inheritance: nil org-babel-use-quick-and-dirty-noweb-expansion: t-*-\n#+BEGIN_QUOTE\n#+PROPERTY: header-args :eval never-export :noweb no-export\n#+PROPERTY: header-args:python :tangle (ek/file-path)\n#+END_QUOTE\n"))

  ;; Org truncate lines
  (setq toggle-truncate-lines nil)

  ;; solve html-export issues with fci-ruler
  (with-eval-after-load 'fill-column-indicator
    (defvar modi/htmlize-initial-fci-state nil
      "Variable to store the state of `fci-mode' when `htmlize-buffer' is called.")

    (defun modi/htmlize-before-hook-fci-disable ()
      (setq modi/htmlize-initial-fci-state fci-mode)
      (when fci-mode
        (fci-mode -1)))

    (defun modi/htmlize-after-hook-fci-enable-maybe ()
      (when modi/htmlize-initial-fci-state
        (fci-mode 1)))

    (add-hook 'htmlize-before-hook #'modi/htmlize-before-hook-fci-disable)
    (add-hook 'htmlize-after-hook #'modi/htmlize-after-hook-fci-enable-maybe))

  ;; autoload .venv - works but breaks anaconda mode on windows
  ;; (defun pyvenv-autoload ()
  ;;   (require 'projectile)
  ;;   (let* ((pdir (projectile-project-root)) (pfile (concat pdir ".venv")))
  ;;     (if (file-exists-p pfile)
  ;;         (pyvenv-workon (with-temp-buffer
  ;;                          (insert-file-contents pfile)
  ;;                          (nth 0 (split-string (buffer-string))))))))

  (defun pyvenv-autoload ()
    (when (string= buffer-file-name "c:/Dev/pop-synth/project.org")
      (pyvenv-workon "pop-synthvenv"))
    (when (string= buffer-file-name "c:/Dev/health/health.org")
      (pyvenv-workon "healthvenv")))

  (add-hook 'org-mode-hook 'pyvenv-autoload)

  ;; For virtualenvs in org-babel
  (require 'virtualenvwrapper)
  (pyvenv-mode 1)
  ;; (setq venv-location "c:/MyPrograms/Anaconda3/envs/")
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support

  ;; Tangle Org files when we save them
  (defun tangle-on-save-org-mode-file()
    (when (string= major-mode "org-mode")
      (org-babel-tangle)))
  (add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

  ;; fix temporary anaconda-mode reponse failures
  ;; (require 'anaconda-mode)
  ;; (remove-hook 'anaconda-mode-response-read-fail-hook
  ;;              'anaconda-mode-show-unreadable-response)
  ;; (setq company-idle-delay 0.5)

  ;; Hide all org blocks on file load
  (defvar org-blocks-hidden nil)
  (defun org-toggle-blocks ()
    (interactive)
    (if org-blocks-hidden
        (org-show-block-all)
      (org-hide-block-all))
    (setq-local org-blocks-hidden (not org-blocks-hidden)))

  (add-hook 'org-mode-hook 'org-toggle-blocks)
  (define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)

  ;; This is the default but can change for css lists for html export
  (setq org-html-htmlize-output-type 'inline-css)

  ;; priority colors
  (setq org-priority-faces '((65 :foreground "red")
                             (66 :foreground "yellow")
                             (67 :foreground "blue")))

  ;; Allows tagging headers with 'ignore' tag to remove header in export
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))

  ;; Lots of org src mode hacks here
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-window-setup 'current-window)

  (defun win-is-right ()
    "Figure out if the current window is to the left, right or middle"
    (let* ((win-edges (window-edges))
           (this-window-x-min (nth 0 win-edges))
           (this-window-x-max (nth 2 win-edges))
           (fr-width (frame-width)))
      (cond
       ((eq 0 this-window-x-min) nil)
       ((eq (+ fr-width 4) this-window-x-max) t)
       (t t))))

  (defun ek/org-edit-src-code ()
    (interactive)
    (if (win-is-right)
        (org-edit-src-code)
      (let ((org-src-window-setup 'other-window))
        (org-edit-src-code))))

  (defun ek/tangle-in-src-edit ()
    (interactive)
    (let ((pos (point))  ;; excursion wont work here
          (was-right (win-is-right))
          (current-prefix-arg '(4)))
      (org-edit-src-exit)
      (call-interactively 'org-babel-tangle)
      (if was-right
          (ek/org-edit-src-code)
        (org-edit-src-code))
      (goto-char pos)))

  (defun ek/test-in-src-edit ()
    (interactive)
    (let ((pos (point))  ;; excursion wont work here
          (was-right (win-is-right))
          (current-prefix-arg '(4)))
      (org-edit-src-exit)
      (call-interactively 'org-babel-tangle)
      (let ((cmd (format "py.test -k %s&" (ek/file-path))))
        (if was-right
            (ek/org-edit-src-code)
          (org-edit-src-code))
        (goto-char pos)
        (shell-command cmd))))


  ;; HACK - not restoring windows -> doesnt close other src edits
  ;; makes both ek/org-edit-src and ek/tangle work as desired
  (add-hook 'org-src-mode-hook (lambda () (setq org-src--saved-temp-window-config nil)))

  (define-key org-mode-map (kbd "C-c C-'") 'ek/org-edit-src-code)

  (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode (kbd "RET") 'ek/tangle-in-src-edit)
  (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode (kbd "t") 'ek/test-in-src-edit)

  ;; Required for asynchronous python execution
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; Set as a local variable to run emacs-lisp/dot blocks on file load
  (defun ek/exec-init ()
    (save-excursion 
      (org-element-map (org-element-parse-buffer 'element) 'src-block
        (lambda (src)
          (when (string= "emacs-lisp" (org-element-property :language src))
            (unless (string= "startup-proj" (org-element-property :name src))
              (goto-char (org-element-property :begin src))
              (org-babel-execute-src-block)))))))

  ;; This should eventually be done through locals rather than emacs settings
  (defun ek/startup-proj ()
    ;; Run init blocks for func defs
    (ek/exec-init)
    ;; Run init funcs
    (ek/setup-src))

  (setq ispell-program-name "aspell")
  ;;(setq ispell-personal-dictionary "C:/Users/ekasc/~/.aspell.LANG.pws")

  ;; (setq org-html-htmlize-output-type 'css)

  (setq tramp-default-method "plink")
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  ;; (require 'flycheck)

;;   (flycheck-def-args-var flycheck-python-mypy-args python-mypy)

;;   (flycheck-define-checker python-mypy
;;     "Mypy syntax checker. Requires mypy>=0.3.1.
;; Customize `flycheck-python-mypy-args` to add specific args to default
;; executable.
;; E.g. when processing Python2 files, add \"--py2\".
;; See URL `http://mypy-lang.org/'."

;;     :command ("mypy --ignore-missing-imports --fast-parser --python-version 3.6"
;;               (eval flycheck-python-mypy-args)
;;               source-original)
;;     :error-patterns
;;     ((error line-start (file-name) ":" line ": error:" (message) line-end))
;;     :modes python-mode)

;;   (add-to-list 'flycheck-checkers 'python-mypy t)




  (defun mypy-show-region ()
    "Show type of variable at point."
    (interactive)
    (let ((here (region-beginning))
          (there (region-end))
          (filename (buffer-file-name)))
      (let ((hereline (line-number-at-pos here))
            (herecol (save-excursion (goto-char here) (current-column)))
            (thereline (line-number-at-pos there))
            (therecol (save-excursion (goto-char there) (current-column))))
        (org-edit-src-exit)
        (shell-command
         (format "mypy --ignore-missing-imports --fast-parser --python-version 3.6 %s&" (ek/file-path)))
        (org-edit-src-code))))
         ;; (format "cd ~/src/mypy; python3 ./scripts/find_type.py %s %s %s %s %s python3 -m mypy -i mypy"
                 ;; filename hereline herecol thereline therecol)))))

  ;; (define-key python-mode-map (kbd "C-c m") 'mypy-show-region)

  ;; (provide 'flycheck-mypy)
  ;;;;;;;;
  )
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
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
 '(evil-want-Y-yank-to-eol nil)
 '(org-agenda-files (quote ("~/org/notes.org")))
 '(safe-local-variable-values
   (quote
    ((eval when
           (locate-library "rainbow-mode")
           (require
            (quote rainbow-mode))
           (rainbow-mode))
     (eval org-babel-execute-src-block)
     (eval
      (org-babel-goto-named-src-block "startup-proj")
      (org-babel-execute-maybe))
     (eval org-babel-goto-named-src-block "startup-proj")
     (eval org-babel-execute-maybe)
     (eval org-babel-goto-named-src-block
           (quote startup-proj))
     (eval ek/setup-src)
     (eval ek/exec-init)
     (eval ek/startup-proj)
     (org-babel-use-quick-and-dirty-noweb-expansion . t)
     (org-use-property-inheritance . t)
     (org-use-tag-inheritance)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
