;;; Setup

;; -- Eric Kaschalk's Spacemacs Configuration --
;; -- Contact: ekaschalk@gmail.com --
;; -- MIT License --
;; -- Emacs 26.1 ~ Spacemacs Dev Branch 0.300.0.x ~ pkgs updated: 10/09/18 --
;; -- http://modernemacs.com --
;;
;; Personal layers host most of my configuration - see README.
;; Ligatures and icons require installation - see README.
;;
;; `init.el' sets-up spacemacs, defining required `dotspacemacs/...' funcs & vars.
;; `outline-minor-mode' and extension `outshine-mode' will help with navigation.

(defvar eric?    (string= "Eric Kaschalk" (user-full-name)) "Am I me?")
(defvar linux?   (eq system-type 'gnu/linux)     "Are we on a linux machine?")
(defvar mac?     (eq system-type 'darwin)        "Are we on a macOS machine?")
(defvar windows? (not (or linux? mac?))          "Are we on a windows machine?")
(defvar desktop? (= 1440 (display-pixel-height)) "Am I on my desktop?")

(defvar dotspacemacs/font
  (if (x-list-fonts "Operator Mono")
      "operator mono medium"  ; Personally used but a paid font
    "Source Code Pro")

  "Font name to use, defaulting to Source Code Pro.")

;;; Layer Declarations
;;;; Local

(defvar dotspacemacs/layers/local
  '((macros   :location local)   ; All local layers depend on this layer
    (config   :location local)   ; Org, Avy, Evil, Misc... config
    (display  :location local)   ; Pretty-eshell/code/outlines... pkgs
    (personal :location local))  ; Personal pkgs

  "Local layers housed in `~/.spacemacs.d/layers'.")

;;;; Core

(defvar dotspacemacs/layers/core
  '(better-defaults
    git
    syntax-checking

    (auto-completion :variables
                     auto-completion-return-key-behavior 'complete
                     auto-completion-tab-key-behavior 'complete
                     auto-completion-enable-snippets-in-popup t)
    (ivy :variables
         ivy-extra-directories nil)
    (org :variables
         org-want-todo-bindings t)
    (shell :variables
           shell-default-shell 'eshell)
    (version-control :variables
                     version-control-global-margin t
                     version-control-diff-tool 'git-gutter+))

  "Layers I consider core to Spacemacs.")

;;;; Langs

(defvar dotspacemacs/layers/langs
  '(;; Markups
    csv
    html
    markdown
    yaml

    ;; Languages
    c-c++
    clojure
    emacs-lisp
    hy
    javascript

    (haskell :variables
             haskell-completion-backend 'intero)
    (python :variables
            python-test-runner 'pytest
            python-spacemacs-indent-guess nil))

  "Programming and markup language layers.")

;;;; Extra

(defvar dotspacemacs/layers/extra
  '(gnus
    graphviz
    ranger

    (ibuffer :variables
             ibuffer-group-buffers-by 'projects))

  "Miscellaneous layers.")

;;; Spacemacs/
;;;; Spacemacs/init

(defun dotspacemacs/init ()
  "Instantiate Spacemacs core settings.

All `dotspacemacs-' variables with values set different than their defaults.

They are all defined in `~/.emacs.d/core/core-dotspacemacs.el'.
Check `dotspacemacs/get-variable-string-list' for all vars you can configure."
  (setq-default
   ;; Display
   dotspacemacs-default-font `(,dotspacemacs/font
                               :size ,(cond (mac? 18) (desktop? 20) (t 34))
                               :powerline-scale 1.5)
   dotspacemacs-themes       '(solarized-light
                               zenburn)

   ;; Editing settings
   dotspacemacs-editing-style '(vim :variables
                                    vim-style-visual-feedback t
                                    vim-style-remap-Y-to-y$ t)

   ;; Elpa
   dotspacemacs-elpa-https        nil
   dotspacemacs-elpa-subdirectory nil

   ;; General
   dotspacemacs-auto-generate-layout-names  t
   dotspacemacs-fullscreen-at-startup       t
   dotspacemacs-large-file-size             5
   dotspacemacs-pretty-docs                 t
   dotspacemacs-search-tools                '("ag" "rg" "pt" "ack" "grep")
   dotspacemacs-scratch-mode                'org-mode
   dotspacemacs-startup-lists               nil
   dotspacemacs-whitespace-cleanup          'trailing
   ))

;;;; Spacemacs/layers

(defun dotspacemacs/layers ()
  "Instantiate Spacemacs layers declarations and package configurations."
  (setq-default
   ;; Layers
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-configuration-layers     (append dotspacemacs/layers/local
                                                 dotspacemacs/layers/core
                                                 dotspacemacs/layers/langs
                                                 dotspacemacs/layers/extra)

   ;; Packages
   dotspacemacs-additional-packages '(solarized-theme
                                      faceup)
   dotspacemacs-excluded-packages   '(;; Must Exclude
                                      fringe      ; For styling reasons
                                      importmagic ; Python pkg was broken for me
                                      scss-mode   ; We overwrite this mode

                                      ;; Packages I don't use
                                      anzu
                                      centered-cursor-mode
                                      column-enforce-mode
                                      company-statistics
                                      doom-modeline
                                      eshell-prompt-extras
                                      evil-anzu
                                      evil-mc
                                      evil-tutor
                                      fancy-battery
                                      fill-column-indicator
                                      gnuplot
                                      golden-ratio
                                      indent-guide
                                      live-py-mode
                                      multiple-cursors
                                      mwim
                                      neotree)
   dotspacemacs-frozen-packages     '()
   ))

;;;; Spacemacs/user-init

(defun dotspacemacs/user-init ()
  "Package independent settings to run before `dotspacemacs/user-config'."
  (setq custom-file "./elisp/.custom-settings.el"))

;;;; Spacemacs/user-config
;;;;; Core

(defun dotspacemacs/user-config ()
  "Configuration that cannot be delegated to layers."
  (dotspacemacs/user-config/toggles)
  (dotspacemacs/user-config/eric-only)
  (when eric? (dotspacemacs/user-config/experiments)))

;;;;; Toggles

(defun dotspacemacs/user-config/toggles ()
  "Spacemacs toggles not intended to be put into layers."
  (spacemacs/toggle-mode-line-minor-modes-off)
  (global-highlight-parentheses-mode 1)
  (rainbow-delimiters-mode-enable)
  (fringe-mode '(0 . 8)))

;;;;; Personal

(defun dotspacemacs/user-config/eric-only ()
  "Personal configuration updates and experiments."
  ;; (setq find-function-C-source-directory "~/dev/emacs-dev/src")
  ;; (load-file "~/dev/hy-mode/hy-mode.el")
  ;; (load-file "~/dev/hy-mode/hy-personal.el")
  ;; (require 'hy-mode)
  ;; (require 'hy-personal)
  )

;;;;; Experiments

(defun dotspacemacs/user-config/experiments ()
  "Space for trying out configuration updates."
  )
