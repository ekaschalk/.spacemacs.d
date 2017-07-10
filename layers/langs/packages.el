;;; Langs Packages

(setq langs-packages
      '(
        hy-mode
        python
        virtualenvwrapper

        (mypy-flycheck :location local)
        (windows-pytest :location local)
        ))

;;; Python

(defun langs/post-init-python ()
  ;; Sometimes ipython shells trigger a bad error to popup
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_")))

  ;; Remove flyspell
  (add-hook 'python-mode-hook (lambda () (flyspell-mode -1)))

  ;; Whether to print logs in pytest
  ;; (setq pytest-cmd-flags "-x --no-print-logs")
  ;; (setq pytest-cmd-flags "-x -s")

  ;; Enables python shell to print unicode
  (setenv "PYTHONIOENCODING" "utf-8")
  (setenv "LANG" "en_US.UTF-8")
  )

;;; Venvs

(defun langs/init-virtualenvwrapper ()
  (use-package virtualenvwrapper
    :after python
    :config
    (progn
      (pyvenv-mode 1)
      (venv-initialize-interactive-shells)
      (venv-initialize-eshell)

      ;; Fixes hy-mode environment when pyvenv is activated
      (add-hook 'pyvenv-post-activate-hooks 'python/init-hy-mode))))

;;; Hy

(defun langs/post-init-hy-mode ()
  (defun hy-insert-pdb ()
    (interactive)
    (insert "(do (import pdb) (pdb.set-trace))"))

  (defun hy-insert-thread-pdb ()
    (interactive)
    (insert "((tz.do (do (import pdb) (pdb.set-trace))))"))

  (spacemacs/set-leader-keys-for-major-mode
    'hy-mode (kbd "dd") 'hy-insert-pdb)
  (spacemacs/set-leader-keys-for-major-mode
    'hy-mode (kbd "dt") 'hy-insert-thread-pdb))

;;; Windows-pytest

(defun langs/init-windows-pytest ()
  (use-package windows-pytest
    :after python
    :if (not is-linuxp)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode
        'python-mode (kbd "t m") 'ek-pytest-module)
      (spacemacs/set-leader-keys-for-major-mode
        'python-mode (kbd "t t") 'ek-pytest-one))))
