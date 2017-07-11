;;; Display Layer

(setq display-packages
      '(
        ;; Language specific updates
        hy-mode

        ;; Core Display Packages
        all-the-icons
        all-the-icons-ivy
        spaceline-all-the-icons
        (prettify-utils :location (recipe :fetcher github
                                          :repo "Ilazki/prettify-utils.el"))

        ;; Local packages
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
    ;; :after hy-mode python
    :config
    (progn
      (global-prettify-symbols-mode 1)

      (setq hy-pretty-pairs
            (pretty-code-get-pairs
             '(:lambda "fn" :def "defn"
                       :composition "comp"
                       :null "None" :true "True" :false "False"
                       :in "in" :not "not"
                       :and "and" :or "or"
                       :some "some"
                       :tuple "#t"
                       :pipe "ap-pipe"
                       )))

      (setq python-pretty-pairs
            (pretty-code-get-pairs
             '(:lambda "lambda" :def "def"
                       :null "None" :true "True" :false "False"
                       :int "int" :float "float" :str "str" :bool "bool"
                       :not "not" :for "for" :in "in" :not-in "not in"
                       :return "return" :yield "yield"
                       :and "and" :or "or"
                       :tuple "Tuple"
                       :pipe "tz-pipe"
                       )))

      (pretty-code-set-pairs `((hy-mode-hook     ,hy-pretty-pairs)
                               (python-mode-hook ,python-pretty-pairs))))))

;;;; Pretty-eshell

(defun display/init-pretty-eshell ()
  (use-package pretty-eshell
    :config
    (progn
      (esh-section esh-dir
                   "\xf07c"  ; 
                   (abbreviate-file-name (eshell/pwd))
                   '(:foreground "gold" :bold ultra-bold :underline t))
      (esh-section esh-git
                   "\xe907"  ; 
                   (magit-get-current-branch)
                   '(:foreground "pink"))
      (esh-section esh-python
                   "\xe928"  ; 
                   pyvenv-virtual-env-name)
      (esh-section esh-clock
                   "\xf017"  ; 
                   (format-time-string "%H:%M" (current-time))
                   '(:foreground "forest green"))
      (esh-section esh-num
                   "\xf0c9"  ; 
                   (number-to-string esh-prompt-num)
                   '(:foreground "brown"))
      (setq eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num)))))

;;;; Pretty-fonts

(defun display/init-pretty-fonts ()
  (use-package pretty-fonts
    :init
    (progn
      (defconst pretty-fonts-hy-mode
        '(("\\(self\\)"   ?⊙))))

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
    :config
    (progn
      (pretty-magit "Feature" ? (:foreground "slate gray" :height 1.2))
      (pretty-magit "Add"     ? (:foreground "#375E97" :height 1.2))
      (pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.2))
      (pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.2))
      (pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.2))
      (pretty-magit "master"  ? (:box t :height 1.2) t)
      (pretty-magit "origin"  ? (:box t :height 1.2) t))))

;;;; Pretty-outlines

(defun display/init-pretty-outlines ()
  (use-package pretty-outlines
    :after outshine
    :config
    (progn
      ;; Ellipsis
      (add-hook 'outline-mode-hook 'pretty-outline-set-display-table)
      (add-hook 'outline-minor-mode-hook 'pretty-outline-set-display-table)

      ;; Outlines
      (add-hook 'emacs-lisp-mode-hook 'pretty-outline-add-bullets)
      ;;   (add-hook 'hy-mode-hook 'pretty-outline-add-bullets)
      ;;   (add-hook 'python-mode-hook 'pretty-outline-add-bullets)
      )))

;;;; Windows-frame-size-fix

(defun display/init-windows-frame-size-fix ()
  (use-package windows-frame-size-fix
    :if (not is-linuxp)))

;;; Core Packages
;;;; All-the-icons

(defun display/init-all-the-icons ()
  (use-package all-the-icons
    :config
    (progn
      (add-to-list
       'all-the-icons-icon-alist
       '("\\.hy$" all-the-icons-fileicon "lisp" :face all-the-icons-orange))
      (add-to-list
       'all-the-icons-mode-icon-alist
       '(hy-mode all-the-icons-fileicon "lisp" :face all-the-icons-orange)))))

;;;; All-the-icons-ivy

(defun display/init-all-the-icons-ivy ()
  (use-package all-the-icons-ivy
    :after all-the-icons
    :config
    (progn
      (all-the-icons-ivy-setup)
      (advice-add 'all-the-icons-ivy-file-transformer
                  :override 'ivy-file-transformer-fixed-for-files))))

;;;; Hy-mode

(defun display/post-init-hy-mode ()
  (defun hy-extra-syntax ()
    (font-lock-add-keywords
     nil '(;; self is not defined by hy-mode as a keyword
           ("\\<\\(self\\)" . 'font-lock-constant-face)

           ;; Highlight entire line for decorators
           ("\\(#@.*$\\)" . 'font-lock-function-name-face)

           ;; Syntax highlighting for reader-macros
           ("\\(#.\\)" . 'font-lock-function-name-face)

           ;; Highlight with macros
           ("\\(with[^ ]*\\)" . 'font-lock-keyword-face)

           ;; Highlight functions after specific macros
           ("\-fixture \\([^ ]*\\)" 1 'font-lock-function-name-face)
           ("\-fixtures \\([^ ]*\\)" 1 'font-lock-function-name-face)

           ;; Fixture macros
           ("\\(deffixture\\)" . 'font-lock-keyword-face)
           ("deffixture \\([^ ]*\\)" 1 'font-lock-function-name-face)

           ;; Asserts
           ("(\\(assert[^ ]*\\)" 1 font-lock-keyword-face)
           )))

  (add-hook 'hy-mode-hook 'hy-extra-syntax))

;;;; Prettify-utils

(defun display/init-prettify-utils ()
  (use-package prettify-utils))

;;;; Spaceline-all-the-icons

(defun display/init-spaceline-all-the-icons ()
  (use-package spaceline-all-the-icons
    :after spaceline
    :config
    (progn
      (spaceline-all-the-icons-theme)
      (setq spaceline-highlight-face-func 'spaceline-highlight-face-default
            spaceline-all-the-icons-icon-set-modified 'chain
            spaceline-all-the-icons-icon-set-window-numbering 'circle
            spaceline-all-the-icons-separator-type 'none
            spaceline-all-the-icons-primary-separator "")
      (spaceline-toggle-all-the-icons-buffer-size-off)
      (spaceline-toggle-all-the-icons-buffer-position-off)
      (spaceline-toggle-all-the-icons-vc-icon-off)
      (spaceline-toggle-all-the-icons-vc-status-off)
      (spaceline-toggle-all-the-icons-git-status-off)
      (spaceline-toggle-all-the-icons-flycheck-status-off)
      (spaceline-toggle-all-the-icons-time-off)
      (spaceline-toggle-all-the-icons-battery-status-off)
      (spaceline-toggle-hud-off))))
