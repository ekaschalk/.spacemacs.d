;;; Langs Packages

(setq langs-packages
      '(
        lispy
        virtualenvwrapper

        (python :location built-in)

        ;; (mypy-flycheck :location local)
        (windows-pytest :location local)
        ))

;;; Lispy

(defun langs/init-lispy ()
  (use-package lispy
    :config (require 'le-hy)))

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
      (add-hook 'pyvenv-post-activate-hooks 'python/init-hy-mode))))

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

  ;; Enables python shell to print unicode
  (setenv "PYTHONIOENCODING" "utf-8")
  (setenv "LANG" "en_US.UTF-8")

  ;; Cython numpy symlink
  (setenv "CFLAGS" "-I /root/.virtualenvs/pop-synthvenv/lib/python3.6/site-packages/numpy/core/include"))

;;; Windows-pytest

(defun langs/init-windows-pytest ()
  (use-package windows-pytest
    :after python
    :if (not is-linuxp)
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "tm" 'ek-pytest-module
      "tt" 'ek-pytest-one)))
