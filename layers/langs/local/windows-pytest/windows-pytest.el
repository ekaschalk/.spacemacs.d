(require 'python)

(provide 'windows-pytest)

(defun ek-pytest-module ()
  (interactive)
  (shell-command (format "py.test -x -s %s&" buffer-file-name)))

(defun ek-pytest-one ()
  (interactive)
  (save-excursion
    (let ((test-name
           (progn
             (re-search-backward "^[ ]*def \\(test_[a-zA-Z0-9_]*\\)")
             (match-string 1))))
      (shell-command
       (format "py.test -x -s %s::%s&" buffer-file-name test-name)))))
