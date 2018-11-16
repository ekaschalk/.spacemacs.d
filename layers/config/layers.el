;;; -*- lexical-binding: t; -*-

(configuration-layer/declare-layers
 '(;; Core
   (auto-completion :variables
                    auto-completion-return-key-behavior 'complete
                    auto-completion-tab-key-behavior 'complete
                    auto-completion-enable-snippets-in-popup t)
   better-defaults
   git
   (ivy :variables
        ivy-extra-directories nil)
   (org :variables
        org-want-todo-bindings t)
   (shell :variables
          shell-default-shell 'eshell)
   syntax-checking
   (version-control :variables
                    version-control-global-margin t
                    version-control-diff-tool 'git-gutter+)

   ;; Misc
   graphviz
   ranger
   (ibuffer :variables
            ibuffer-group-buffers-by 'projects)

   ;; Markups
   csv
   html
   markdown
   yaml

   ;; Languages
   c-c++
   clojure
   emacs-lisp
   haskell
   hy  ; I wrote this mode/layer, yay
   (python :variables
           python-test-runner 'pytest
           python-spacemacs-indent-guess nil)



   ;; Experimental/in-flux
   (mu4e :variables
         mu4e-installation-path       "/usr/local/share/emacs/site-lisp"
         mu4e-use-maildirs-extension  t
         ;; mu4e-enable-async-operations t
         ;; mu4e-spacemacs-layout-name "@Mu4e"
         ;; mu4e-spacemacs-layout-binding "m"
         ;; mu4e-spacemacs-kill-layout-on-exit t
         )
   ))
