;;; Display Layer -*- lexical-binding: t; -*-

(setq display-packages
      '(;; Owned packages
        all-the-icons
        all-the-icons-ivy
        all-the-icons-dired
        pretty-mode
        solarized-theme
        (prettify-utils :location (recipe :fetcher github
                                          :repo "Ilazki/prettify-utils.el"))

        ;; Elsehwere-owned packages
        spaceline-all-the-icons
        which-key

        ;; Personal display-related packages
        (pretty-code     :location local)
        (pretty-eshell   :location local)
        (pretty-fonts    :location local)
        (pretty-magit    :location local)
        (pretty-outlines :location local)))

;;; Owned Packages
;;;; All-the-icons

(defun display/init-all-the-icons ()
  (use-package all-the-icons
    :config
    (let ((hy-face '(:face all-the-icons-orange))
          (dt-face '(:face all-the-icons-pink)))
      (mapcar*
       #'add-to-list
       '(all-the-icons-icon-alist
         all-the-icons-icon-alist
         all-the-icons-mode-icon-alist
         all-the-icons-mode-icon-alist)
       `(("\\.hy$"          all-the-icons-fileicon "hy"       ,@hy-face)
         ("\\.dot$"         all-the-icons-fileicon "graphviz" ,@dt-face)
         (hy-mode           all-the-icons-fileicon "hy"       ,@hy-face)
         (graphviz-dot-mode all-the-icons-fileicon "graphviz" ,@dt-face))))))

;;;; All-the-icons-ivy

(defun display/init-all-the-icons-ivy ()
  (defun all-the-icons-ivy-file-transformer-stdized (s)
    "Fix `all-the-icons-ivy-file-transformer' to have stdized height/offset."
    (format "%s\t%s"
            (propertize "\t" 'display
                        (all-the-icons-icon-for-file s :height 0.9 :v-adjust 0))
            s))

  (use-package all-the-icons-ivy
    :config
    (progn
      ;; I have no idea why the default behavior for this pkg
      ;; doesn't standardize the vertical offset and height
      (advice-add 'all-the-icons-ivy-file-transformer :override
                  'all-the-icons-ivy-file-transformer-stdized)

      ;; Counsel defines a particular file transformer for just
      ;; projectile (works on virtual files). Lets tack on the
      ;; all-the-icons-ivy transformer for projectile icons once-again.
      (advice-add 'counsel-projectile-find-file-transformer :filter-return
                  'all-the-icons-ivy-file-transformer-stdized)
      (advice-add 'counsel-projectile-transformer :filter-return
                  'all-the-icons-ivy-file-transformer-stdized)

      (all-the-icons-ivy-setup))))

;;;; All-the-icons-dired

(defun display/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

;;;; Pretty-mode

(defun display/init-pretty-mode ()
  ;; I *only* use greek letter replacements at the moment.
  ;; However, I go back and forht on whether to use nil-like <-> emptyset.
  ;; I currently have it *enabled*. Uncomment the deactivation to remove it.

  (use-package pretty-mode
    :config
    (progn
      (global-pretty-mode t)

      (pretty-deactivate-groups
       '(:equality :ordering :ordering-double :ordering-triple
                   :arrows :arrows-twoheaded :punctuation
                   :logic :sets
                   ;; :nil
                   ))
      (pretty-activate-groups
       '(:greek)))))

;;;; Prettify-utils

(defun display/init-prettify-utils ()
  (use-package prettify-utils))

;;;; Solarized-theme

(defun display/init-solarized-theme ()
  (use-package solarized-theme))

;;; Unowned Packages
;;;; Which-key

(defun display/post-init-which-key ()
  (when (configuration-layer/package-used-p 'pretty-fonts)
    (setq which-key-separator " ")
    (setq which-key-prefix-prefix " ")))

;;;; Spaceline-all-the-icons

(defun display/post-init-spaceline-all-the-icons ()
  (spaceline-all-the-icons-theme)

  (setq spaceline-highlight-face-func 'spaceline-highlight-face-default)

  (setq spaceline-all-the-icons-icon-set-modified         'chain)
  (setq spaceline-all-the-icons-icon-set-window-numbering 'square)
  (setq spaceline-all-the-icons-separator-type            'none)
  (setq spaceline-all-the-icons-primary-separator         "")

  ;; Mode Segments
  (spaceline-toggle-all-the-icons-minor-modes-off)

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

;;; Pretty Packages
;;;; Pretty-code

(defun display/init-pretty-code ()
  ;; Note: I'm not using many of the symbols I've defined
  ;; in the past like ones for "in", "for", and so on.
  ;; Full commentary on why in `pretty-code.el'.

  (use-package pretty-code
    :config
    (progn
      (global-prettify-symbols-mode 1)

      ;; note: refactor pretty-code set/get someday
      (pretty-code-set-pairs
       `((hy-mode-hook
          ,(pretty-code-get-pairs
            '(:lambda "fn" :def "defn")))
         (python-mode-hook
          ,(pretty-code-get-pairs
            '(:lambda "lambda" :def "def")))
         (emacs-lisp-mode-hook
          ,(pretty-code-get-pairs
            '(:def "defun"))))))))

;;;; Pretty-eshell

(defun display/init-pretty-eshell ()
  (use-package pretty-eshell
    :init
    (progn
      ;; Change default banner message
      (setq eshell-banner-message (s-concat (s-repeat 20 "---") "\n\n"))

      ;; More prompt styling
      (setq pretty-eshell-header "\n︳")
      (setq pretty-eshell-prompt-string " "))

    :config
    (progn
      ;; Directory
      (pretty-eshell-section
       esh-dir
       "\xf07c"  ; 
       (abbreviate-file-name (eshell/pwd))
       '(:foreground "#268bd2" :bold bold :underline t))

      ;; Git Branch
      (pretty-eshell-section
       esh-git
       "\xe907"  ; 
       (magit-get-current-branch)
       '(:foreground "#8D6B94"))

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

      ;; Prompt Number
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
    :config
    ;; !! This is required to avoid segfault when using emacs as daemon !!
    (spacemacs|do-after-display-system-init
     (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
     (pretty-fonts-set-kwds '((pretty-fonts-fira-font
                               prog-mode-hook
                               org-mode-hook)))

     (pretty-fonts-set-fontsets
      '(;; All-the-icons fontsets
        ("fontawesome"
         ;;                         
         #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

        ("all-the-icons"
         ;;    
         #xe907 #xe928)

        ("github-octicons"
         ;;                               
         #xf091 #xf059 #xf076 #xf075 #xe192  #xf016 #xf071)

        ("material icons"
         ;;              
         #xe871 #xe918 #xe3e7  #xe5da
         ;;              
         #xe3d0 #xe3d1 #xe3d2 #xe3d4))))))

;;;; Pretty-magit

(defun display/init-pretty-magit ()
  (use-package pretty-magit
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
       (:foreground "#3F681C" :height 1.2)))))

;;;; Pretty-outlines

(defun display/init-pretty-outlines ()
  (use-package pretty-outlines
    :init
    (progn
      (setq pretty-outlines-ellipsis            "")
      (setq pretty-outlines-bullets-bullet-list '("" "" "" ""))

      (spacemacs/add-to-hooks 'pretty-outlines-set-display-table
                              '(outline-mode-hook
                                outline-minor-mode-hook))

      (spacemacs/add-to-hooks 'pretty-outlines-add-bullets
                              '(emacs-lisp-mode-hook
                                hy-mode-hook
                                python-mode-hook)))))
