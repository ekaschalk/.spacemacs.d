;; -*- mode: emacs-lisp -*-
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '((shell :variables shell-default-shell 'eshell)                      ; Shell
     better-defaults helm git org ranger syntax-checking version-control ; Core
     theming
     graphviz restclient                                                 ; Babel
     emacs-lisp html python                                              ; Langs
     )
   dotspacemacs-additional-packages '(outshine navi-mode virtualenvwrapper)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  (setq-default
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
   dotspacemacs-themes '(spacemacs-dark spacemacs-light zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Fira Code"
                               :size 12
                               :weight bold
                               :width condensed
                               :powerline-scale 1.1)
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
   dotspacemacs-fullscreen-at-startup t
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
   dotspacemacs-smartparens-strict-mode t  ; XXX Trying out
   dotspacemacs-smart-closing-parenthesis t  ; XXX Trying out
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  )

(defun dotspacemacs/user-config ()
;;; Spacemacs org -> outline highlighting
  (custom-theme-set-faces
   'spacemacs-dark
   '(outline-1 ((t (:inherit org-level-1))))
   '(outline-2 ((t (:inherit org-level-2))))
   '(outline-3 ((t (:inherit org-level-3))))
   '(outline-4 ((t (:inherit org-level-4)))))



  ;; https://github.com/akatov/pretty-mode/blob/master/pretty-mode.el

;;; Do later
  ;; (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  ;; (spacemacs/toggle-centered-point-globally-on)
  ;; Need to read up on
  ;; smart-parens: https://github.com/Fuco1/smartparens/wiki
  ;; Emacs-client and other investigations
  ;; http://psung.blogspot.com/2009/05/using-itsalltext-with-emacsemacsclient.html
  ;; https://github.com/docwhat/itsalltext
  ;; probably have to do the other fira code trick
  ;; (substitute-key-definition 'old-def 'new-def map)

;;; Outshine
  (require 'outshine)
  ;; (remove-hook 'navi-mode-hook 'evil-mode)

  (let ((map outline-mode-prefix-map))
    (define-key map "j" 'outline-forward-same-level)
    (define-key map "k" 'outline-backward-same-level)
    (spacemacs/set-leader-keys "o" map)
    )

  (setq outshine-use-speed-commands t)
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'prog-mode-hook 'outline-minor-mode)


;;; Evil
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-unordered-key-sequence "true")

;;; Toggles
  (spacemacs/toggle-highlight-long-lines-globally-on)
  (fringe-mode '(1 . 1))  ; Minimal left padding and ~ end newline markers
  (rainbow-delimiters-mode-enable)  ; Paren color based on depth
  (global-highlight-parentheses-mode 1)  ; Highlight containing parens
  (hungry-delete-mode 1)  ; in edit mode back gets all contiguous whitespace
  (add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))  ; SPC splits past 80
  (spacemacs/toggle-aggressive-indent-globally-on)  ; auto-indentation
  (spacemacs/toggle-mode-line-minor-modes-off)  ; no unicode symbs next to major
  (linum-relative-global-mode 1)  ; very useful for multi-line vim motions
  (global-prettify-symbols-mode 1)  ; eg. lambda

;;; Windows Frame Size Fix
  (add-to-list 'default-frame-alist '(font . "Fira Code"))
  (set-face-attribute 'default t :font "Fira Code")
  (defun ek/fix () (mapc (lambda (x) (zoom-frm-out)) '(1 2)))  ; 80 chars zoom

;;; Projectile
  (setq projectile-indexing-method 'native)  ; respect .projectile files

;;; Aspell
  (setq ispell-program-name "aspell")

;;; Python
  ;; Virtual Environments
  (require 'virtualenvwrapper)
  (pyvenv-mode 1)
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)

  (defun pyvenv-autoload ()
    (when (string= buffer-file-name "c:/~/dev/pop-synth/base.org")
      (pyvenv-workon "pop-synthvenv"))
    (when (string= buffer-file-name "c:/~/dev/health/base.org")
      (pyvenv-workon "healthvenv")))

  (add-hook 'org-mode-hook 'pyvenv-autoload)

  ;; Mypy
  (defun mypy-show-region ()
    (interactive)
    (org-edit-src-exit)
    (shell-command
     (format "mypy --ignore-missing-imports --fast-parser --python-version 3.6 %s&" (ek/file-path)))
    (org-edit-src-code))

  ;; (define-key python-mode-map (kbd "C-c m") 'mypy-show-region)

;;; Org
  ;; Core
  (require 'ox-extra)
  (setq org-bullets-bullet-list '("■" "○" "✸" "✿")
        org-priority-faces '((65 :foreground "red")
                             (66 :foreground "yellow")
                             (67 :foreground "blue")))
  (ox-extras-activate '(ignore-headlines))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 (dot . t)
                                 (http . t)))

  ;; Exporting
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
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; Templates
  (mapc (lambda (x) (add-to-list 'org-structure-template-alist x))
        (list
         ;; Common
         '("n" "#+NAME: ?")
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
                 " :tangle (ek/file-path)\n#+END_QUOTE\n"))))

  ;; Funcs Core
  (defvar org-blocks-hidden nil)
  (defun org-toggle-blocks ()
    (interactive)
    (if org-blocks-hidden
        (org-show-block-all)
      (org-hide-block-all))
    (setq-local org-blocks-hidden (not org-blocks-hidden)))

  (defun tangle-on-save-org-mode-file()
    (when (and (string= major-mode "org-mode")
               (string= buffer-file-name "c:/~/dev/pop-synth/base.org"))
      (org-babel-tangle)))

  ;; Python babel utilities
  (defun ek/tangle-in-src-edit ()
    (interactive)
    (let ((pos (point)) (vpos (window-start)))
      (org-edit-src-exit) (org-babel-tangle) (org-edit-src-code)
      (goto-char pos) (set-window-start (selected-window) vpos)))

  (defun ek/test-in-src-edit ()
    (interactive)
    (let ((cmd nil) (pos (point)) (current-prefix-arg '(4)))
      (org-edit-src-exit)

      (let ((base (buffer-base-buffer))
            (src-block (org-element-property :name (org-element-at-point))))
        (with-current-buffer (current-buffer)
          (save-excursion
            (when base
              (switch-to-buffer base)
              (org-babel-goto-named-src-block src-block))
            (call-interactively 'org-babel-tangle)
            (setq cmd (format "py.test -k %s&" (ek/file-path)))))
        (org-edit-src-code)
        (goto-char pos)
        (shell-command cmd))))

  ;; Project initation
  (defun ek/exec-init ()
    (save-excursion
      (org-element-map (org-element-parse-buffer 'element) 'src-block
        (lambda (src)
          (when (string= "emacs-lisp" (org-element-property :language src))
            (unless (string= "startup-proj" (org-element-property :name src))
              (goto-char (org-element-property :begin src))
              (org-babel-execute-src-block)))))))

  (defun ek/startup-proj ()
    (ek/exec-init)  ; Run proj-specific init blocks
    (ek/setup-src))  ; Run proj-specific setup-src

  ;; Hooks
  (add-hook 'org-mode-hook 'flyspell-mode)  ; Async python, spelling
  (add-hook 'org-mode-hook 'org-toggle-blocks)
  (add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

  ;; Keymappings
  (define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)
  (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode (kbd "RET") 'ek/tangle-in-src-edit)
  (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode (kbd "t") 'ek/test-in-src-edit)

;;; Font Ligatures
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

  (defconst fira-code-font-lock-keywords-alist
    (mapcar
     (lambda (regex-char-pair)
       `(,(car regex-char-pair)
         (0 (prog1 ()
              (compose-region
               (match-beginning 1)
               (match-end 1)
               ,(concat "	"
                        (list (decode-char 'ucs (cadr regex-char-pair)))))))))
     '(;; ("[^/]\\(\\*\\*\\)[^/]"        #Xe101) ("\\(\\*\\*\\*\\)"             #Xe102)
       ;; ("\\(\\*\\*/\\)"               #Xe103) ("\\(\\*>\\)"                  #Xe104)
       ;; ("[^*]\\(\\*/\\)"              #Xe105) ("\\(\\[\\]\\)"                #Xe109)
       ;; ("\\(/\\*\\)"                  #Xe12a) ("\\(/\\*\\*\\)"               #Xe12b)
       ;; ("\\(<\\*\\)"                  #Xe14b) ("\\(<\\*>\\)"                 #Xe14c)
       ;; ("\\(x\\)"                     #Xe16b)
       ("\\(www\\)"                   #Xe100) ("\\(\\\\\\\\\\)"              #Xe106)
       ("\\(\\\\\\\\\\\\\\)"          #Xe107) ("\\({-\\)"                    #Xe108)
       ("\\(::\\)"                    #Xe10a) ("\\(:::\\)"                   #Xe10b)
       ("[^=]\\(:=\\)"                #Xe10c) ("\\(!!\\)"                    #Xe10d)
       ("\\(!=\\)"                    #Xe10e) ("\\(!==\\)"                   #Xe10f)
       ("\\(-}\\)"                    #Xe110) ("\\(--\\)"                    #Xe111)
       ("\\(---\\)"                   #Xe112) ("\\(-->\\)"                   #Xe113)
       ("[^-]\\(->\\)"                #Xe114) ("\\(->>\\)"                   #Xe115)
       ("\\(-<\\)"                    #Xe116) ("\\(-<<\\)"                   #Xe117)
       ("\\(-~\\)"                    #Xe118) ("\\(#{\\)"                    #Xe119)
       ("\\(#\\[\\)"                  #Xe11a) ("\\(##\\)"                    #Xe11b)
       ("\\(###\\)"                   #Xe11c) ("\\(####\\)"                  #Xe11d)
       ("\\(#(\\)"                    #Xe11e) ("\\(#\\?\\)"                  #Xe11f)
       ("\\(#_\\)"                    #Xe120) ("\\(#_(\\)"                   #Xe121)
       ("\\(\\.-\\)"                  #Xe122) ("\\(\\.=\\)"                  #Xe123)
       ("\\(\\.\\.\\)"                #Xe124) ("\\(\\.\\.<\\)"               #Xe125)
       ("\\(\\.\\.\\.\\)"             #Xe126) ("\\(\\?=\\)"                  #Xe127)
       ("\\(\\?\\?\\)"                #Xe128) ;;("\\(;;\\)"                    #Xe129)
       ("\\(/=\\)"                    #Xe12c) ("\\(/==\\)"                   #Xe12d)
       ("\\(/>\\)"                    #Xe12e) ("\\(//\\)"                    #Xe12f)
       ("\\(///\\)"                   #Xe130) ("\\(&&\\)"                    #Xe131)
       ("\\(||\\)"                    #Xe132) ("\\(||=\\)"                   #Xe133)
       ("[^|]\\(|=\\)"                #Xe134) ("\\(|>\\)"                    #Xe135)
       ("\\(\\^=\\)"                  #Xe136) ("\\(\\$>\\)"                  #Xe137)
       ("\\(\\+\\+\\)"                #Xe138) ("\\(\\+\\+\\+\\)"             #Xe139)
       ("\\(\\+>\\)"                  #Xe13a) ("\\(=:=\\)"                   #Xe13b)
       ("[^!/]\\(==\\)[^>]"           #Xe13c) ("\\(===\\)"                   #Xe13d)
       ("\\(==>\\)"                   #Xe13e) ("[^=]\\(=>\\)"                #Xe13f)
       ("\\(=>>\\)"                   #Xe140) ("\\(<=\\)"                    #Xe141)
       ("\\(=<<\\)"                   #Xe142) ("\\(=/=\\)"                   #Xe143)
       ("\\(>-\\)"                    #Xe144) ("\\(>=\\)"                    #Xe145)
       ("\\(>=>\\)"                   #Xe146) ("[^-=]\\(>>\\)"               #Xe147)
       ("\\(>>-\\)"                   #Xe148) ("\\(>>=\\)"                   #Xe149)
       ("\\(>>>\\)"                   #Xe14a) ("\\(<|\\)"                    #Xe14d)
       ("\\(<|>\\)"                   #Xe14e) ("\\(<\\$\\)"                  #Xe14f)
       ("\\(<\\$>\\)"                 #Xe150) ("\\(<!--\\)"                  #Xe151)
       ("\\(<-\\)"                    #Xe152) ("\\(<--\\)"                   #Xe153)
       ("\\(<->\\)"                   #Xe154) ("\\(<\\+\\)"                  #Xe155)
       ("\\(<\\+>\\)"                 #Xe156) ("\\(<=\\)"                    #Xe157)
       ("\\(<==\\)"                   #Xe158) ("\\(<=>\\)"                   #Xe159)
       ("\\(<=<\\)"                   #Xe15a) ("\\(<>\\)"                    #Xe15b)
       ("[^-=]\\(<<\\)"               #Xe15c) ("\\(<<-\\)"                   #Xe15d)
       ("\\(<<=\\)"                   #Xe15e) ("\\(<<<\\)"                   #Xe15f)
       ("\\(<~\\)"                    #Xe160) ("\\(<~~\\)"                   #Xe161)
       ("\\(</\\)"                    #Xe162) ("\\(</>\\)"                   #Xe163)
       ("\\(~@\\)"                    #Xe164) ("\\(~-\\)"                    #Xe165)
       ("\\(~=\\)"                    #Xe166) ("\\(~>\\)"                    #Xe167)
       ("[^<]\\(~~\\)"                #Xe168) ("\\(~~>\\)"                   #Xe169)
       ("\\(%%\\)"                    #Xe16a) ("[^:=]\\(:\\)[^:=]"           #Xe16c)
       ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d))))

  (defun match-outline-levels (regex-char-pair)
    `(,(car regex-char-pair)
      (0 (prog1 ()
           (compose-region
            (match-beginning 1)
            (match-end 1)
            ,(concat "	"
                     (list (cadr regex-char-pair))))))))

  (defconst emacs-lisp-outline-levels
    (mapcar 'match-outline-levels
            '(("\\(^;;;\\)"                  ?■)
              ("\\(^;;;;\\)"                 ?○)
              ("\\(^;;;;;\\)"                ?✸)
              ("\\(^;;;;;;\\)"               ?✿))))

  ;; Fixes 'LATIN SUBSCRIPT SMALL LETTER J' unicode not picking up a font
  (set-fontset-font "fontset-default" '(#x2c7c . #x2c7c) "Courier New")

  (defconst python-outline-levels
    (mapcar 'match-outline-levels
            '(("\\(^# \\*\\)"                ?■)
              ("\\(^# \\*\\*\\)"             ?○)
              ("\\(^# \\*\\*\\*\\)"          ?✸)
              ("\\(^# \\*\\*\\*\\*\\)"       ?✿)
              ("\\(_0\\)[ \t\n]"             ?₀)
              ("\\(_1\\)[ \t\n]"             ?₁)
              ("\\(_2\\)[ \t\n]"             ?₂)
              ("\\(_3\\)[ \t\n]"             ?₃)
              ("\\(_4\\)[ \t\n]"             ?₄)
              ("\\(_i\\)[ \t\n]"             ?ᵢ)
              ("\\(_j\\)[ \t\n]"             ?ⱼ)
              ("\\(_k\\)[ \t\n]"             ?ₖ)
              ("\\(_m\\)[ \t\n]"             ?ₘ)
              ("\\(_n\\)[ \t\n]"             ?ₙ)
              ("\\(_x\\)[ \t\n]"             ?ₓ)
              ("\\(alpha\\)"            ?\u03B1) ; α
              ("\\(beta\\)"             ?\u03B2) ; β
              ("\\(gamma\\)"            ?\u03B3) ; γ
              ("\\(delta\\)"            ?\u03B4) ; δ
              ("\\(epsilon\\)"          ?\u03B5) ; ε
              ("\\(zeta\\)"             ?\u03B6) ; ζ
              ("\\(theta\\)"            ?\u03B8) ; θ
              ("\\(iota\\)"             ?\u03B9) ; ι
              ("\\(kappa\\)"            ?\u03BA) ; κ
              ("\\(mu\\)"               ?\u03BC) ; μ
              ("\\(nu\\)"               ?\u03BD) ; ν
              ("\\(xi\\)"               ?\u03BE) ; ξ
              ("\\(omicron\\)"          ?\u03BF) ; ο
              ("\\(pi\\)"               ?\u03C0) ; π
              ("\\(rho\\)"              ?\u03C1) ; ρ
              ("\\(sigma\\)"            ?\u03C3) ; σ
              ("\\(tau\\)"              ?\u03C4) ; τ
              ("\\(phi\\)"              ?\u03C6) ; φ
              ("\\(chi\\)"              ?\u03C7) ; χ
              ("\\(psi\\)"              ?\u03C8) ; ψ
              ("\\(omega\\)"            ?\u03C9) ; ω
              )))

  ;; Greeks not done through pretty symbols since that breaks subscripts
  ;; Symbola font is used for these unicode characters
  (add-hook 'python-mode-hook
            (lambda ()
              (mapc (lambda (pair) (push pair prettify-symbols-alist))
                    '(("for" . ?∀)
                      ("in" . ?∊)
                      ("not in" . ?∉)
                      ("not" . ?❗)

                      ("**2" . ?²)
                      ("int" . ?ℤ)
                      ("sum" . ?∑)
                      ("None" . ?∅)
                      ))))

  (defun add-fira-code-symbol-keywords ()
    (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))
  (defun emacs-lisp-outline-levels-keywords ()
    (font-lock-add-keywords nil emacs-lisp-outline-levels))
  (defun python-outline-levels-keywords ()
    (font-lock-add-keywords nil python-outline-levels))

  (add-hook 'org-mode-hook
            #'add-fira-code-symbol-keywords)
  (add-hook 'prog-mode-hook
            #'add-fira-code-symbol-keywords)

  (add-hook 'emacs-lisp-mode-hook
            #'emacs-lisp-outline-levels-keywords)
  (add-hook 'python-mode-hook
            #'python-outline-levels-keywords)

  )

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
   '(evil-want-Y-yank-to-eol t)
   '(package-selected-packages
     (quote
      (navi-mode outshine outorg window-purpose imenu-list zenburn-theme yapfify xterm-color web-mode virtualenvwrapper unfill tagedit smeargle slim-mode shell-pop scss-mode sass-mode restclient-helm ranger pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements orgit org-projectile org-present org-pomodoro alert log4e gntp org-download ob-restclient restclient ob-http mwim multi-term magit-gitflow live-py-mode less-css-mode hy-mode htmlize helm-pydoc helm-gitignore helm-css-scss haml-mode graphviz-dot-mode gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor eshell-z eshell-prompt-extras esh-help emmet-mode diff-hl cython-mode anaconda-mode pythonic ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
   '(safe-local-variable-values
     (quote
      ((eval ek/startup-proj)
       (org-babel-use-quick-and-dirty-noweb-expansion . t)
       (org-use-tag-inheritance)))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )
