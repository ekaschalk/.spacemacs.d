;;; Setup
;;;; Commentary

;; -- Eric Kaschalk's Spacemacs Configuration --
;; -- Contact: ekaschalk@gmail.com --
;; -- MIT License --
;; -- Emacs 26.1 ~ Spacemacs Dev Branch 0.300.0.x ~ pkgs updated: 10/09/18 --
;; -- http://modernemacs.com --
;;
;; Personal layers host most of my configuration - see README.
;; Ligatures and icons require installation - see README.
;;
;; Layers are declared in `layers/config/layers.el'.
;;
;; Configure `server?'        to true if you - use emacs as a daemon.
;; Configure `redo-bindings?' to true if you - want my aggressive rebindings.
;;
;; `init.el' sets spacemacs up, defining required `dotspacemacs/..' funcs & vars.
;; `outline-minor-mode' and extension `outshine-mode' will help with navigation.

;;;; Constants

(defvar eric?    (string= "Eric Kaschalk" (user-full-name)) "Am I me?")
(defvar linux?   (eq system-type 'gnu/linux)     "Are we on a linux machine?")
(defvar mac?     (eq system-type 'darwin)        "Are we on a macOS machine?")
(defvar windows? (not (or linux? mac?))          "Are we on a windows machine?")

;;;; Configuration

(defvar server? (if eric? t nil)
  "Alias `dotspacemacs-enable-server'. Defaults to nil for non-eric users.")

(defvar redo-bindings? (if eric? t nil)
  "Redo spacemacs bindings? Defaults to nil for non-eric users.

I aggressively re-bind and un-bind spacemacs defaults.

This indicator:
1. Removes prefixes/bindings contained within `redo-spacemacs-prefixes-list'.
2. Removes bindings in `redo-spacemacs-undo-bindings-alist'.
3. Adds bindings in `redo-spacemacs-new-bindings-alist'.

It is highly recommend to look through the above 3 variables before enabling,
defined at end of `layers/config/packages.el' in `config/init-redo-spacemacs'.")

;;; Spacemacs/
;;;; Spacemacs/init

(defun dotspacemacs/init ()
  "Instantiate Spacemacs core settings.

All `dotspacemacs-' variables with values set different than their defaults.

They are all defined in `~/.emacs.d/core/core-dotspacemacs.el'.
Check `dotspacemacs/get-variable-string-list' for all vars you can configure."
  (setq-default
   ;; Display
   dotspacemacs-default-font `(,(if (x-list-fonts "Operator Mono")
                                    "operator mono medium"
                                  "Source Code Pro")
                               :size ,(if (= 1440 (display-pixel-height)) 20 18))
   dotspacemacs-themes       '(solarized-light
                               zenburn)

   ;; General
   dotspacemacs-auto-generate-layout-names t
   dotspacemacs-editing-style              '(vim :variables
                                                 vim-style-visual-feedback t
                                                 vim-style-remap-Y-to-y$ t)
   dotspacemacs-elpa-https                 nil
   dotspacemacs-elpa-subdirectory          nil
   dotspacemacs-enable-server              server?
   dotspacemacs-fullscreen-at-startup      t
   dotspacemacs-large-file-size            5
   dotspacemacs-persistent-server          server?
   dotspacemacs-pretty-docs                t
   dotspacemacs-search-tools               '("ag" "rg" "pt" "ack" "grep")
   dotspacemacs-scratch-mode               'org-mode
   dotspacemacs-startup-lists              nil
   dotspacemacs-whitespace-cleanup         'trailing

   ;; The following are unchanged but are still required for reloading via
   ;; 'SPC f e R' `dotspacemacs/sync-configuration-layers' to not throw warnings
   dotspacemacs-emacs-leader-key  "M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-leader-key        "SPC"
   dotspacemacs-mode-line-theme   'all-the-icons))

;;;; Spacemacs/layers

(defun dotspacemacs/layers ()
  "Instantiate Spacemacs layers declarations and package configurations."
  (setq-default
   dotspacemacs-configuration-layers     '((macros   :location local)
                                           (config   :location local)
                                           (display  :location local)
                                           (personal :location local))
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-additional-packages      '(solarized-theme)
   dotspacemacs-frozen-packages          '()
   dotspacemacs-excluded-packages
   '(;; Must Exclude (for styling and functionality reasons)
     fringe importmagic scss-mode vi-tilde-fringe

     ;; Packages I don't use
     anzu centered-cursor-mode column-enforce-mode company-statistics
     doom-modeline eshell-prompt-extras evil-anzu evil-mc evil-tutor
     fancy-battery fill-column-indicator gnuplot golden-ratio indent-guide
     live-py-mode multi-term multiple-cursors mwim neotree paradox py-isort
     yapfify)))

;;;; Spacemacs/user-init

(defun dotspacemacs/user-init ()
  "Package independent settings to run before `dotspacemacs/user-config'."
  (setq custom-file "~/.spacemacs.d/.custom-settings.el"))

;;;; Spacemacs/user-config
;;;;; Core

(defun dotspacemacs/user-config ()
  "Configuration that cannot be delegated to layers."
  (dotspacemacs/user-config/toggles)
  (dotspacemacs/user-config/post-layer-load-config)
  (dotspacemacs/user-config/eric-only)
  (dotspacemacs/user-config/experiments))

;;;;; Toggles

(defun dotspacemacs/user-config/toggles ()
  "Spacemacs toggles should not be put into layers, per Spacemacs docs."
  (spacemacs/toggle-mode-line-minor-modes-off)
  (global-highlight-parentheses-mode 1)
  (rainbow-delimiters-mode-enable)
  (fringe-mode '(0 . 0)))

;;;;; Post Layer Load

(defun dotspacemacs/user-config/post-layer-load-config ()
  "Configuration that must take place *after all* layers/pkgs are instantiated."
  (when (configuration-layer/package-used-p 'redo-spacemacs)
    (redo-spacemacs-bindings))

  (when server?
    ;; The set-fontset has to be done later in the process than the
    ;; layer loading or Emacs will break. So right now dumping this here.
    ;; This just enables pretty-fonts to effect the initial frame.
    ;; All future frames are handled via `after-make-frame-functions'.
    (when (configuration-layer/package-used-p 'pretty-fonts)
      (display/init-pretty-fonts/kwds     'noframe)
      (display/init-pretty-fonts/fontsets 'noframe))

    ;; While toggling with `toggle-frame-fullscreen' works, I could not get
    ;; it to work as a hook attached to the frame-make or window-setup.
    ;; Depending on your OS, you may need a different/not-at-all need this.
    (when mac?
      (add-to-list 'default-frame-alist '(fullscreen . fullboth)))))

;;;;; Personal

(defun dotspacemacs/user-config/eric-only ()
  "Personal configuration updates and experiments."
  (when eric?
    ;; Emacs-anywhere defaults to org-mode rather than markdown-mode
    (add-hook 'ea-popup-hook (lambda (&rest args) (org-mode)))

    ;; Hy-mode development
    ;; (load-file "~/dev/hy-mode/hy-mode.el")
    ;; (load-file "~/dev/hy-mode/hy-personal.el")
    ;; (require 'hy-mode)
    ;; (require 'hy-personal)

    ;; Emacs-core development
    ;; (setq find-function-C-source-directory "~/dev/emacs-dev/src")
    ))

;;;;; Experiments

(defun dotspacemacs/user-config/experiments ()
  "Space for trying out configuration updates."
  )
