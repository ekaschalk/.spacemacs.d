;;; Display Layer

(setq display-packages
      '(
        ;; Owned Display Packages
        all-the-icons
        all-the-icons-ivy
        all-the-icons-dired
        pretty-mode
        spaceline-all-the-icons
        (prettify-utils :location (recipe :fetcher github
                                          :repo "Ilazki/prettify-utils.el"))

        ;; Owned Local Display Packages
        (pretty-code :location local)
        (pretty-eshell :location local)
        (pretty-fonts :location local)
        (pretty-magit :location local)
        (pretty-outlines :location local)
        (windows-frame-size-fix :location local)
        ))

;;; Locals
;;;; Pretty-code

(defun display/init-pretty-code ()
  (use-package pretty-code
    :after prettify-utils macros
    :config
    (progn
      (global-prettify-symbols-mode 1)

      (setq hy-pretty-pairs
            (pretty-code-get-pairs
             '(;; Functional
               :lambda
               "fn"
               :def "defn" :composition "comp"

               ;; Types
               :null "None"
               :true "True" :false "False"

               ;; Flow
               :not "not"
               :in "in" :not-in "not-in"
               :and "and" :or "or"
               :some "some"

               ;; Other
               :tuple "#t"  ; Tag macro for tuple casting
               )))

      (setq python-pretty-pairs
            (pretty-code-get-pairs
             '(;; Functional
               :lambda
               "lambda"
               :def "def"

               ;; Types
               :null "None"
               :true "True" :false "False"
               :int "int" :float "float"
               :str "str" :bool "bool"

               ;; Flow
               :not "not"
               :in "in" :not-in "not in"
               :and "and" :or "or"
               :for "for"
               :return "return" :yield "yield"

               ;; Other
               :tuple "Tuple" :pipe "tz-pipe"
               )))

      (pretty-code-set-pairs `((hy-mode-hook     ,hy-pretty-pairs)
                               (python-mode-hook ,python-pretty-pairs))))))

;;;; Pretty-eshell

(defun display/init-pretty-eshell ()
  (use-package pretty-eshell
    :after macros
    :config
    (progn
      ;; Directory
      (pretty-eshell-section
       esh-dir
       "\xf07c"  ; 
       (abbreviate-file-name (eshell/pwd))
       '(:foreground "gold" :bold ultra-bold :underline t))

      ;; Git Branch
      (pretty-eshell-section
       esh-git
       "\xe907"  ; 
       (magit-get-current-branch)
       '(:foreground "pink"))

      ;; Python Virtual Environment
      (pretty-eshell-section
       esh-python
       "\xe928"  ; 
       pyvenv-virtual-env-name)

      ;; Time
      (pretty-eshell-section
       esh-clock
       "\xf017"  ; 
       (format-time-string "%H:%M" (current-time))
       '(:foreground "forest green"))

      ;; Prompy Number
      (pretty-eshell-section
       esh-num
       "\xf0c9"  ; 
       (number-to-string pretty-eshell-prompt-num)
       '(:foreground "brown"))

      (setq pretty-eshell-funcs
            (list esh-dir esh-git esh-python esh-clock esh-num)))))

;;;; Pretty-fonts

(defun display/init-pretty-fonts ()
  (use-package pretty-fonts
    :init
    (defconst pretty-fonts-hy-mode
      '(("\\(self\\)"   ?⊙)))

    :config
    (progn
      (pretty-fonts-set-kwds
       '(;; Fira Code Ligatures
         (pretty-fonts-fira-font prog-mode-hook org-mode-hook)
         ;; Custom replacements not possible with `pretty-code' package
         (pretty-fonts-hy-mode hy-mode-hook)))

      (pretty-fonts-set-fontsets
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
          #xe871 #xe918 #xe3e7
          ;;
          #xe3d0 #xe3d1 #xe3d2 #xe3d4)

         ("Symbola"
          ;; 𝕊    ⨂      ∅      ⟻    ⟼     ⊙      𝕋       𝔽
          #x1d54a #x2a02 #x2205 #x27fb #x27fc #x2299 #x1d54b #x1d53d
          ;; 𝔹    𝔇       𝔗
          #x1d539 #x1d507 #x1d517))))))

;;;; Pretty-magit

(defun display/init-pretty-magit ()
  (use-package pretty-magit
    :after ivy magit macros
    :config
    (progn
      (pretty-magit-add-leader
       "Feature"
       ?
       (:foreground "slate gray" :height 1.2))

      (pretty-magit-add-leader
       "Add"
       ?
       (:foreground "#375E97" :height 1.2))

      (pretty-magit-add-leader
       "Fix"
       ?
       (:foreground "#FB6542" :height 1.2))

      (pretty-magit-add-leader
       "Clean"
       ?
       (:foreground "#FFBB00" :height 1.2))

      (pretty-magit-add-leader
       "Docs"
       ?
       (:foreground "#3F681C" :height 1.2))

      (pretty-magit-add-leader
       "master"
       ?
       (:box t :height 1.2)
       'no-prompt)

      (pretty-magit-add-leader
       "origin"
       ?
       (:box t :height 1.2)
       'no-prompt))))

;;;; Pretty-outlines

(defun display/init-pretty-outlines ()
  (use-package pretty-outlines
    :after outshine macros
    :config
    (progn
      (setq pretty-outlines-bullets-bullet-list
            '("" "" "" ""))
      (setq pretty-outlines-ellipsis
            "")

      (spacemacs/add-to-hooks 'pretty-outlines-set-display-table
                              '(outline-mode-hook
                                outline-minor-mode-hook))

      (spacemacs/add-to-hooks 'pretty-outlines-add-bullets
                              '(emacs-lisp-mode-hook
                                hy-mode-hook
                                python-mode-hook)))))

;;;; Windows-frame-size-fix

(defun display/init-windows-frame-size-fix ()
  (use-package windows-frame-size-fix
    :if (not linux?)))

;;; Core Packages
;;;; All-the-icons

(defun display/init-all-the-icons ()
  (use-package all-the-icons
    :config
    (progn
      (defconst all-the-icons-icon-hy
        '("\\.hy$"
          all-the-icons-fileicon "lisp" :face all-the-icons-orange))
      (defconst all-the-icons-mode-icon-hy
        '(hy-mode
          all-the-icons-fileicon "lisp" :face all-the-icons-orange))

      (defconst all-the-icons-icon-graphviz
        '("\\.dot$"
          all-the-icons-fileicon "graphviz" :face all-the-icons-pink))
      (defconst all-the-icons-mode-icon-graphviz
        '(graphviz-dot-mode
          all-the-icons-fileicon "graphviz" :face all-the-icons-pink))

      (add-to-list 'all-the-icons-icon-alist
                   all-the-icons-icon-hy)
      (add-to-list 'all-the-icons-icon-alist
                   all-the-icons-icon-graphviz)
      (add-to-list 'all-the-icons-mode-icon-alist
                   all-the-icons-mode-icon-hy)
      (add-to-list 'all-the-icons-mode-icon-alist
                   all-the-icons-mode-icon-graphviz))))

;;;; All-the-icons-ivy

(defun display/init-all-the-icons-ivy ()
  (use-package all-the-icons-ivy
    :after all-the-icons
    :config
    (progn
      (all-the-icons-ivy-setup)
      (advice-add 'all-the-icons-ivy-file-transformer :override
                  'ivy-file-transformer-fixed-for-files))))

;;;; All-the-icons-dired

(defun display/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :config
    (add-hook 'dired-mode-hook
              'all-the-icons-dired-mode)))

;;;; Pretty-mode

(defun display/init-pretty-mode ()
  (use-package pretty-mode
    :config
    (progn
      (global-pretty-mode t)

      (pretty-deactivate-groups
       '(:equality :ordering :ordering-double :ordering-triple
                   :arrows :arrows-twoheaded :punctuation
                   :logic :sets))
      (pretty-activate-groups
       '(:greek :arithmetic-nary)))))

;;;; Prettify-utils

(defun display/init-prettify-utils ()
  (use-package prettify-utils))

;;;; Spaceline-all-the-icons

(defun display/post-init-spaceline-all-the-icons ()
  (spaceline-all-the-icons-theme)

  (setq spaceline-highlight-face-func
        'spaceline-highlight-face-default)

  (setq spaceline-all-the-icons-icon-set-modified
        'chain)

  (setq spaceline-all-the-icons-icon-set-window-numbering
        'square)

  (setq spaceline-all-the-icons-separator-type
        'none)

  (setq spaceline-all-the-icons-primary-separator
        "")

  ;; Buffer Segments
  (spaceline-toggle-all-the-icons-buffer-size-off)
  (spaceline-toggle-all-the-icons-buffer-position-off)

  ;; Git Segments
  (spaceline-toggle-all-the-icons-git-status-off)
  (spaceline-toggle-all-the-icons-vc-icon-off)
  (spaceline-toggle-all-the-icons-vc-status-off)

  ;; Misc Segments
  (spaceline-toggle-all-the-icons-eyebrowse-workspace-off)
  (spaceline-toggle-all-the-icons-flycheck-status-off)
  (spaceline-toggle-all-the-icons-time-off))
