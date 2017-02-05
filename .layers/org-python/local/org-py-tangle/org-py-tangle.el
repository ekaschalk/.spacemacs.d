(require 'org)

;;; Tangling
  (defun tangle-on-save-org-mode-file()
    (when (and (string= major-mode "org-mode")
               (string= buffer-file-name "c:/~/dev/pop-synth/base.org"))
      (org-babel-tangle)))

  (defun ek/tangle-in-src-edit ()
    (interactive)
    (let ((pos (point)) (vpos (window-start)))
      (org-edit-src-exit) (org-babel-tangle) (org-edit-src-code)
      (goto-char pos) (set-window-start (selected-window) vpos)))

  (defun ek/test-in-src-edit ()
    (interactive)
    (let ((cmd nil) (pos (point)) (current-prefix-arg '(4)))
      (org-edit-src-exit)

      (let ((base (buffer-base-buffer))
            (src-block (org-element-property :name (org-element-at-point))))
        (with-current-buffer (current-buffer)
          (save-excursion
            (when base
              (switch-to-buffer base)
              (org-babel-goto-named-src-block src-block))
            (call-interactively 'org-babel-tangle)
            (setq cmd (format "py.test -k %s&" (ek/file-path)))))
        (org-edit-src-code)
        (goto-char pos)
        (shell-command cmd))))

;;; Projects
  (defun ek/exec-init ()
    (save-excursion
      (org-element-map (org-element-parse-buffer 'element) 'src-block
        (lambda (src)
          (when (string= "emacs-lisp" (org-element-property :language src))
            (unless (string= "startup-proj" (org-element-property :name src))
              (goto-char (org-element-property :begin src))
              (org-babel-execute-src-block)))))))

  (defun ek/startup-proj ()
    (ek/exec-init)  ; Run proj-specific init blocks
    (ek/setup-src))  ; Run proj-specific setup-src

(provide 'org-py-tangle)
