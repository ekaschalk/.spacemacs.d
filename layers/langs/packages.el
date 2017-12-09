;;; Langs Packages

(setq langs-packages
      '(
        python
        virtualenvwrapper

        ;; (mypy-flycheck :location local)  ; Currently not using mypy
        (windows-pytest :location local)
        ))

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
  ;; Sometimes ipython shells trigger a bad error to popup
  ;; TODO Check if this is still necessary
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_")))

  ;; Enables python shell to print unicode
  (setenv "PYTHONIOENCODING" "utf-8")
  (setenv "LANG" "en_US.UTF-8")

  ;; Remove flyspell
  (add-hook 'python-mode-hook (lambda () (flyspell-mode -1)))

  ;; Disable printing logs within pytest
  ;; TODO Check if this is still necessary
  ;; (setq pytest-cmd-flags "-x --no-print-logs")
  )

;;; Windows-pytest

(defun langs/init-windows-pytest ()
  (use-package windows-pytest
    :after python
    :if (not linux?)))
