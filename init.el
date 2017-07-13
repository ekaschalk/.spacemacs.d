;;; Intro

;; -- Eric Kaschalk's Spacemacs Configuration --
;; -- Contact: ekaschalk@gmail.com --
;; -- MIT License --
;; -- Emacs 25.2.1 - Dev Branch - Release 0.200.9.x - pulled/pkgs updated: 5/29 --
;; -- See README for details and VERSION for updates --
;;
;; All configuration is housed in personal layers

(setq is-linuxp (eq system-type 'gnu/linux))
(defun os-path (x) (if is-linuxp x (expand-file-name x "c:")))

;;; Layers
;;;; Local

(defvar dotspacemacs/layers/local
  '((macros :location local)    ; All local layers inherit these macros

    (config :location local)    ; Org, Avy, Evil, Misc... config
    (display :location local)   ; Pretty-eshell/code/outlines... pkgs
    (langs :location local)     ; Language config
    (personal :location local)  ; Personal pkgs
    )
  "Local layers housed in '~/.spacemacs.d/layers'.")

;;;; Core

(defvar dotspacemacs/layers/core
  '(better-defaults
    git
    org
    syntax-checking
    (auto-completion :variables
                     auto-completion-return-key-behavior 'complete
                     auto-completion-tab-key-behavior 'complete
                     auto-completion-enable-snippets-in-popup t)
    (ivy :variables
         ivy-extra-directories nil)
    (shell :variables
           shell-default-shell 'eshell)
    (version-control :variables
                     version-control-global-margin t
                     version-control-diff-tool 'git-gutter+)
    )
  "Layers I consider core to Spacemacs")

;;;; Langs

(defvar dotspacemacs/layers/langs
  '(emacs-lisp
    html
    javascript
    markdown
    rust
    (clojure :variables
             clojure-enable-fancify-symbols t)
    (haskell :variables
             haskell-completion-backend 'intero)
    (python :variables
            python-sort-imports-on-save t
            python-test-runner 'pytest)
    )
  "Programming and markup language layers")

;;;; Extra

(defvar dotspacemacs/layers/extra
  '(gnus
    graphviz
    ranger
    (evil-snipe :variables
                evil-snipe-enable-alternate-f-and-t-behaviors t)
    (ibuffer :variables
             ibuffer-group-buffers-by 'projects)
    )
  "Miscellaneous layers")

;;;; Additional Packages

(setq dotspacemacs/additional/packages
      '(solarized-theme  ; Extra themes won't work when loaded from layer
        ))

;;;; Dotspacemacs

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path `(,(os-path "~/.spacemacs.d/layers/"))
   dotspacemacs-additional-packages dotspacemacs/additional/packages
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-but-keep-unused
   dotspacemacs-configuration-layers
   (append dotspacemacs/layers/core
           dotspacemacs/layers/langs
           dotspacemacs/layers/extra
           dotspacemacs/layers/local)))

;;; Spacemacs-Init
;;;; Configuration

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-themes '(solarized-dark solarized-light)
   dotspacemacs-default-font `("operator mono medium"
                               :size ,(if is-linuxp 18 12)
                               :powerline-scale 1.5)
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
   dotspacemacs-fullscreen-at-startup (if is-linuxp nil t)
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
  (setq custom-file "./elisp/.custom-settings.el"))

;;; Spacemacs-User-config

(defun dotspacemacs/user-config ()
  "Spacemacs toggles not intended to be put into layers."
  (spacemacs/toggle-highlight-long-lines-globally-on)
  (spacemacs/toggle-mode-line-minor-modes-off)
  (spacemacs/toggle-aggressive-indent-globally-on)
  (global-highlight-parentheses-mode 1)
  (rainbow-delimiters-mode-enable)
  (fringe-mode '(0 . 4)))
