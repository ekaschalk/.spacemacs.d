;; -*- mode: emacs-lisp -*-
;;; Working on
;;;; Improvements
;;;;;; TODO bind Q to universal buffer kill in navi mode
;;;;;; TODO M-j M-k for moving subtrees
;;;;;; TODO Narrow only jumps up heading if point not already on heading
;;;;;; TODO Improve M-n q window deletion handling
;;;;;; TODO Possible collapse outlined buffers by default
;;;; Todos
;;;;;; TODO GNUS
;;;;;; TODO enumerate important yasnippets
;;;;;; TODO Outshine/navi moved to layer
;;;;;; TODO font-lock stuff not sure if should move
;;;; Remember to Use
;;;;; Lisp state
;; LEARN:
;; a = absorb
;; b = forward barf, B = backwards barf

;; PROGRESS ON:
;; s = forward slurp, S = backwards slurp
;; (setq) var -> (setq var) after slurp

;; BASIC COMMANDS:
;; w=wrap, W=unwrap
;; dx=delete expr, dX=backwards delete expr
;; e=unwrap expr and kill after point
;; y=yank expr
;;; Spacemacs-Layers
;;;; Configuration
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '(".layers/")
   dotspacemacs-additional-packages '(outshine
                                      navi-mode
                                      virtualenvwrapper)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-but-keep-unused
;;;; Layers
   dotspacemacs-configuration-layers
   '(
;;;;; Core
     better-defaults
     helm
     git
     org
     ranger
     syntax-checking
     version-control
     (shell :variables
            shell-default-shell 'eshell)
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-snippets-in-popup t)
;;;;; Languages
     emacs-lisp
     html
     (python :variables
             python-sort-imports-on-save t
             python-test-runner 'pytest)
;;;;; Rarely Used
     markdown
     graphviz
     restclient
;;;;; Local
     org-python

     )))
;;; Spacemacs-Init
;;;; Configuration
(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-themes '(spacemacs-dark spacemacs-light zenburn)
   dotspacemacs-default-font '("Fira Code"
                               :size 12
                               :weight bold
                               :width condensed
                               :powerline-scale 1.1)
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
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-smart-closing-parenthesis t
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
  (dotspacemacs/user-config/display/windows-frame-size-fix)

  ;; Group 2
  (dotspacemacs/user-config/display/fira-code-ligatures)
  (dotspacemacs/user-config/display/my-ligatures)

  ;; Rest
  (dotspacemacs/user-config/display/prettify-symbols)
  (dotspacemacs/user-config/display/select-ligatures)
  (dotspacemacs/user-config/display/theme-updates)
  )

;;;;; Windows-frame-size-fix
(defun dotspacemacs/user-config/display/windows-frame-size-fix ()
  (add-to-list 'default-frame-alist '(font . "Fira Code"))
  (set-face-attribute 'default t :font "Fira Code")
  (defun ek/fix ()
    (interactive)
    (mapc (lambda (x) (zoom-frm-out)) '(1 2)))
  (global-set-key (kbd "<f2>") 'ek/fix))

;;;;; Theme-updates
(defun dotspacemacs/user-config/display/theme-updates ()
  (custom-theme-set-faces
   'spacemacs-dark
   '(outline-1 ((t (:inherit org-level-1 :underline t))))
   '(outline-2 ((t (:inherit org-level-2 :underline t))))
   '(outline-3 ((t (:inherit org-level-3 :underline t))))
   '(outline-4 ((t (:inherit org-level-4 :underline t))))))

;;;;; Fira-code-ligatures
(defun dotspacemacs/user-config/display/fira-code-ligatures ()
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

  (defun add-fira-code-symbol-keywords ()
    (font-lock-add-keywords nil fira-code-font-lock-keywords-alist)))

;;;;; My-ligatures
(defun dotspacemacs/user-config/display/my-ligatures ()
  (defun match-outline-levels (regex-char-pair)
    `(,(car regex-char-pair)
      (0 (prog1 ()
           (compose-region
            (match-beginning 1)
            (match-end 1)
            ,(concat "	"
                     (list (cadr regex-char-pair))))))))

  (defconst emacs-lisp-prettify-pairs
    (mapcar 'match-outline-levels
            '(("\\(^;;;\\)"                   ?■)
              ("\\(^;;;;\\)"                  ?○)
              ("\\(^;;;;;\\)"                 ?✸)
              ("\\(^;;;;;;\\)"                ?✿))))

  (defconst python-prettify-pairs
    (mapcar 'match-outline-levels
            '(("\\(^# \\*\\)[ \t\n]"          ?■)
              ("\\(^# \\*\\*\\)[ \t\n]"       ?○)
              ("\\(^# \\*\\*\\*\\)[ \t\n]"    ?✸)
              ("\\(^# \\*\\*\\*\\*\\)[^\\*]"  ?✿)
              ("\\(_0\\)[: \t\n]"             ?₀)
              ("\\(_1\\)[: \t\n]"             ?₁)
              ("\\(_2\\)[: \t\n]"             ?₂)
              ("\\(_3\\)[: \t\n]"             ?₃)
              ("\\(_4\\)[: \t\n]"             ?₄)
              ("\\(_i\\)[: \t\n]"             ?ᵢ)
              ("\\(_j\\)[: \t\n]"             ?ⱼ)
              ("\\(_k\\)[: \t\n]"             ?ₖ)
              ("\\(_m\\)[: \t\n]"             ?ₘ)
              ("\\(_n\\)[: \t\n]"             ?ₙ)
              ("\\(_x\\)[: \t\n]"             ?ₓ)
              ("\\(alpha\\)"            ?\u03B1) ; α
              ("\\(beta\\)"             ?\u03B2) ; β
              ("\\(gamma\\)"            ?\u03B3) ; γ
              ("\\(delta\\)"            ?\u03B4) ; δ
              ("\\(epsilon\\)"          ?\u03B5) ; ε
              ("\\(zeta\\)"             ?\u03B6) ; ζ
              ("\\(theta\\)"            ?\u03B8) ; θ
              ("\\(iota\\)"             ?\u03B9) ; ι
              ("\\(kappa\\)"            ?\u03BA) ; κ
              ;; ("\\(mu\\)"               ?\u03BC) ; μ breaks accumulate
              ;; ("\\(xi\\)"               ?\u03BE) ; ξ breaks axis
              ("\\(omicron\\)"          ?\u03BF) ; ο
              ;; ("\\(pi\\)"               ?\u03C0) ; π breaks eg capitalize
              ("\\(rho\\)"              ?\u03C1) ; ρ
              ("\\(sigma\\)"            ?\u03C3) ; σ
              ("\\(tau\\)"              ?\u03C4) ; τ
              ("\\(phi\\)"              ?\u03C6) ; φ
              ("\\(chi\\)"              ?\u03C7) ; χ
              ("\\(omega\\)"            ?\u03C9) ; ω
              )))

  (defun emacs-lisp-prettify-keywords ()
    (font-lock-add-keywords nil emacs-lisp-prettify-pairs))
  (defun python-prettify-keywords ()
    (font-lock-add-keywords nil python-prettify-pairs)))

;;;;; Select-ligatures
(defun dotspacemacs/user-config/display/select-ligatures ()
  (add-hook 'org-mode-hook
            #'add-fira-code-symbol-keywords)
  (add-hook 'prog-mode-hook
            #'add-fira-code-symbol-keywords)

  (add-hook 'emacs-lisp-mode-hook
            #'emacs-lisp-prettify-keywords)
  (add-hook 'python-mode-hook
            #'python-prettify-keywords))

;;;;; Prettify-symbols
  ;; Greeks not done through pretty symbols since that breaks subscripts
  ;; Fixes for unicode not picking up a default font on some chars
  ;; https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
(defun dotspacemacs/user-config/display/prettify-symbols ()
  (set-fontset-font "fontset-default" '(#x2c7c . #x2c7c) "Courier New")
  (set-fontset-font "fontset-default" '(#x1d518 . #x1d518) "Symbola")
  (set-fontset-font "fontset-default" '(#x1d4d0 . #x1d4e2) "Symbola")
  (set-fontset-font "fontset-default" '(#x1d4d0 . #x1d54a) "Symbola")
  (set-fontset-font "fontset-default" '(#x1d54a . #x1d572) "Symbola")

  (add-hook 'python-mode-hook
            (lambda ()
              (mapc (lambda (pair) (push pair prettify-symbols-alist))
                    '(;; Syntax
                      ("not" .      ?❗) ; ¬
                      ("for" .      ?∀)
                      ("in" .       ?∊)
                      ("not in" .   ?∉)
                      ("return" .  ?⟼)
                      ("yield" .   ?⟻)
                      ;; Base Types
                      ("None" .     ?∅)
                      ("int" .      ?ℤ)
                      ("float" .    ?ℝ)
                      ("str" .      ?𝕊)
                      ("True" .     ?𝕋)
                      ("False" .    ?𝔽)
                      ;; Mypy Containers
                      ("Dict" .     ?𝔇)
                      ("List" .     ?ℒ)
                      ("Callable" . ?ℱ)
                      ("Iterable" . ?𝔊)
                      ("Set" .      ?Ω)
                      ;; Mypy Compositions
                      ("Any" .      ?❔) ; ？ ❓
                      ("Tuple" .    ?⨂)
                      ("Union" .    ?⋃)
                      ;; Other
                      ("**2" .      ?²)
                      ("sum" .      ?∑))))))

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
  (spacemacs/toggle-mode-line-minor-modes-off)  ; no unicode symbs next to major
  (global-prettify-symbols-mode 1))  ; eg. lambda, python lots of config

;;;; Navigation
(defun dotspacemacs/user-config/navigation ()
  (dotspacemacs/user-config/navigation/avy))

;;;;; Avy
(defun dotspacemacs/user-config/navigation/avy ()
  (global-set-key (kbd "C-h") 'avy-pop-mark)
  (global-set-key (kbd "C-j") 'evil-avy-goto-char-2)
  (global-set-key (kbd "C-k") 'evil-avy-goto-word-or-subword-1)
  (global-set-key (kbd "C-l") 'evil-avy-goto-line))

;;;; Misc
(defun dotspacemacs/user-config/misc ()
  (dotspacemacs/user-config/misc/aspell)
  (dotspacemacs/user-config/misc/auto-completion)
  (dotspacemacs/user-config/misc/projectile))

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

;;;; To - delete:
(defun dotspacemacs/user-config ()
  ;; Group 1
  (dotspacemacs/user-config/display)

  ;; Rest
  (dotspacemacs/user-config/configuration)
  (dotspacemacs/user-config/navigation)
  (dotspacemacs/user-config/misc)

;;;; MOVE-TO-LAYER Outshine-mode
  ;; TODO Add promote/demote outline heading, not outline subtree
  (require 'outshine)
  (require 'navi-mode)

  (setq outshine-use-speed-commands t)
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'prog-mode-hook 'outline-minor-mode)

;;;;; Navi bindings
  (add-to-list 'navi-key-mappings
               '("python" .
                 ((:FUN . "f")
                  ;; (:VAR . "v")
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
    (define-key map (kbd "M-n")
      (lambda () (interactive) (navi-goto-occurrence-other-window)
        (recenter 3)))  ; Also binded to "o", this one is for consistency

    (evil-define-key '(normal visual motion) map
      "f" (lambda () (interactive) (navi-generic-command ?f current-prefix-arg)) ;Fun
      "v" (lambda () (interactive) (navi-generic-command ?v current-prefix-arg)) ;Var
      "x" (lambda () (interactive) (navi-generic-command ?x current-prefix-arg)) ;Obj
      "a" (lambda () (interactive) (navi-generic-command ?a current-prefix-arg)) ;All
      "1" (lambda () (interactive) (navi-generic-command ?1 current-prefix-arg))
      "2" (lambda () (interactive) (navi-generic-command ?2 current-prefix-arg))
      "3" (lambda () (interactive) (navi-generic-command ?3 current-prefix-arg))
      "4" (lambda () (interactive) (navi-generic-command ?4 current-prefix-arg))

      "u" 'navi-undo
      "n" 'navi-narrow-to-thing-at-point
      "w" 'navi-widen

      "d" (lambda () (interactive) (occur-mode-display-occurrence)
            (other-window 1) (recenter 3) (other-window 1))
      "o" (lambda () (interactive) (navi-goto-occurrence-other-window)
            (recenter 3))  ; 3 lines from top is good spacing
      "q" (lambda () (interactive) (navi-quit-and-switch)
            (delete-other-windows) (recenter 3)))

    (setq navi-mode-map map))

;;;;; Outshine bindings
  (defun my-outshine-navi ()
    (interactive)
    (let ((line nil))
      (widen)  ; Broken on narrowed buffers
      (save-excursion
        (outline-previous-visible-heading 1)
        (setq line
              (replace-regexp-in-string "\n$" ""
                                        (thing-at-point 'line t))))
      (outshine-navi)
      (navi-generic-command ?2 nil)  ; default to 2 heading levels
      (search-forward-regexp line)))

  ;; Org doesnt use outline minor mode but can utilize navi
  ;; TODO remove tags from string in org mode
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
                          (outline-previous-visible-heading 1)
                          (outshine-narrow-to-subtree)))
      (kbd "SPC n j") 'outline-move-subtree-down
      (kbd "SPC n k") 'outline-move-subtree-up))


;;;; MOVE-TO-LAYER Python
;;;;; Virtual Environments
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

;;;;; Mypy
  ;;   (setq flycheck-python-mypy-args
  ;;         '("--ignore-missing-imports" "--fast-parser" "--python-version 3.6"))

  ;;   (flycheck-def-args-var flycheck-python-mypy-args python-mypy)

  ;;   (flycheck-define-checker python-mypy
  ;;     "Mypy syntax checker. Requires mypy>=0.3.1.
  ;; Customize `flycheck-python-mypy-args` to add specific args to default
  ;; executable.
  ;; E.g. when processing Python2 files, add \"--py2\".
  ;; See URL `http://mypy-lang.org/'."

  ;;     :command ("mypy"
  ;;               (eval flycheck-python-mypy-args)
  ;;               source-original)
  ;;     :error-patterns
  ;;     ((error line-start (file-name) ":" line ": error:" (message) line-end))
  ;;     :modes python-mode)

  ;;   ;; (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  ;;   (add-to-list 'flycheck-checkers 'python-mypy t)

  (defun mypy-show-region ()
    (interactive)
    (shell-command
     (format "mypy --ignore-missing-imports --fast-parser --python-version 3.6 %s&" (buffer-file-name))))

  ;; (define-key python-mode-map (kbd "C-c m") 'mypy-show-region)

;;;; ?? Org
;;;;; Core
  (require 'ox-extra)
  (setq org-bullets-bullet-list '("■" "○" "✸" "✿")
        org-priority-faces '((65 :foreground "red")
                             (66 :foreground "yellow")
                             (67 :foreground "blue")))
  (ox-extras-activate '(ignore-headlines))

  (setq org-refile-targets (quote ((nil :regexp . "Week of"))))

;;;;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window)
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (dot . t)
                               (http . t)))
;;;;; Exporting
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

;;;;; Templates
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

;;;;; Toggle blocks
  (defvar org-blocks-hidden nil)
  (defun org-toggle-blocks ()
    (interactive)
    (if org-blocks-hidden
        (org-show-block-all)
      (org-hide-block-all))
    (setq-local org-blocks-hidden (not org-blocks-hidden)))

;;;;; Hooks and Keymappings
  (add-hook 'org-mode-hook 'flyspell-mode)  ; Async python, spelling
  (add-hook 'org-mode-hook 'org-toggle-blocks)

  (define-key org-mode-map
    (kbd "C-c t") 'org-toggle-blocks))

;;;; Compose Config


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
 '(evil-want-Y-yank-to-eol t)
 '(package-selected-packages
   (quote
    (mmm-mode markdown-toc markdown-mode gh-md multiple-cursors helm-company helm-c-yasnippet company-web web-completion-data company-statistics company-restclient know-your-http-well company-anaconda company auto-yasnippet yasnippet ac-ispell auto-complete navi-mode outshine outorg window-purpose imenu-list zenburn-theme yapfify xterm-color web-mode virtualenvwrapper unfill tagedit smeargle slim-mode shell-pop scss-mode sass-mode restclient-helm ranger pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements orgit org-projectile org-present org-pomodoro alert log4e gntp org-download ob-restclient restclient ob-http mwim multi-term magit-gitflow live-py-mode less-css-mode hy-mode htmlize helm-pydoc helm-gitignore helm-css-scss haml-mode graphviz-dot-mode gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor eshell-z eshell-prompt-extras esh-help emmet-mode diff-hl cython-mode anaconda-mode pythonic ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
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
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
