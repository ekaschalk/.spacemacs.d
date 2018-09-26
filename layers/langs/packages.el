;;; Langs Packages

(setq langs-packages
      '(
        clojure
        python
        virtualenvwrapper
        ))

;;; Clojure

(defun langs/post-init-clojure ()
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
    "," 'lisp-state-toggle-lisp-state)
  (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode
    "," 'lisp-state-toggle-lisp-state)
  )

;;; Virtualenvwrapper

(defun langs/init-virtualenvwrapper ()
  (use-package virtualenvwrapper
    :after python
    :config
    (progn
      (pyvenv-mode 1)
      (venv-initialize-interactive-shells)
      (venv-initialize-eshell)

      ;; Fixes hy-mode environment when pyvenv is activated
      ;; TODO Check if this is necessary/add to spacemacs-hy
      (add-hook 'pyvenv-post-activate-hooks 'python/init-hy-mode))))

;;; Python

(defun langs/post-init-python ()
  ;; Executable find ipython isn't working on my machine so I have to
  ;; either modify executable-find (bad) or override the setup
  (advice-add 'spacemacs//python-setup-shell
              :override
              (lambda (&rest args)
                (setq python-shell-interpreter "ipython")
                (setq python-shell-interpreter-args "--simple-prompt -i")))

  ;; Enables python shell to print unicode
  (setenv "PYTHONIOENCODING" "utf-8")
  (setenv "LANG" "en_US.UTF-8")

  ;; Remove flyspell
  (add-hook 'python-mode-hook (lambda () (flyspell-mode -1)))

  ;; Disable printing logs within pytest
  (setq pytest-cmd-flags "-x --no-print-logs -s"))
