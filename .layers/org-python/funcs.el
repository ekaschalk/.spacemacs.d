;; ;;;; Include Org Integration
;; (defun org-hide-init ()
;;   (when (derived-mode-p 'org-mode)
;;     (save-window-excursion
;;       (save-excursion
;;         (goto-char (point-min))
;;         (when (search-forward-regexp "^* Init" nil 'noerror)
;;           (org-cycle))))))

;; (defun file-contents (filename)
;;   "Return the contents of FILENAME."
;;   (with-temp-buffer
;;     (insert-file-contents filename)
;;     (buffer-string)))

;; ;; #+INCLUDE: "src/file.py" :src python :func "statements"
;; ;; or
;; ;; #+INCLUDE: "src/file.py" :src python :func "fields" :lines "36-40"
;; (defun update-python-includes ()
;;   "Format is #+INCLUDE: \"file\" :src python :func \"func_def\""
;;   (interactive)
;;   (when (derived-mode-p 'org-mode)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (search-forward-regexp
;;               "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:func"
;;               nil 'noerror)
;;         (let* ((file (expand-file-name (match-string-no-properties 1))))
;;           (when (looking-at ".*\"\\([a-zA-Z_]+\\)\"")
;;             (setq py-incl-func (match-string-no-properties 1)))

;;           (setq py-incl-module (concat py-incl-func "\n" (file-contents file)))

;;           (save-excursion
;;             (org-babel-goto-named-src-block "extract-python-func-lines")
;;             (org-babel-execute-src-block)
;;             (org-babel-goto-named-result "extract-python-func-lines")
;;             (setq lines
;;                   (s-chomp (org-element-property :value (org-element-at-point)))))

;;           (if (looking-at ".*:lines *\\(\"[-0-9]+\"\\)")
;;               (replace-match lines :fixedcase :literal nil 1)
;;             (goto-char (line-end-position))
;;             (insert " :lines " lines))))))
;;   (org-hide-init))

;; ;; #+INCLUDE-DOCS: "src/file.py"
;; (defun python-include-docstrings ()
;;   "Format is #+INCLUDE: \"file\""
;;   (interactive)
;;   (when (derived-mode-p 'org-mode)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (search-forward-regexp
;;               "^\\s-*#\\+INCLUDE-DOCS: *\"\\([^\"]+\\)\""
;;               nil 'noerror)
;;         (let* ((file (expand-file-name (match-string-no-properties 1))))
;;           (setq py-incl-module (file-contents file))

;;           (save-excursion
;;             (org-babel-goto-named-src-block "extract-python-docstrings")
;;             (org-babel-execute-src-block)
;;             (org-babel-goto-named-result "extract-python-docstrings")
;;             (setq py-docstrings
;;                   (org-element-property :value (org-element-at-point)))
;;             )

;;           (forward-line)
;;           (when (looking-at ".*begin_src python")
;;             (let* ((src (org-element-at-point))
;;                    (start (org-element-property :begin src))
;;                    (end (org-element-property :end src)))
;;               (delete-region start (- end 1))))  ; was deleting extra \n
;;           (save-excursion
;;             (insert "#+begin_src python\n" py-docstrings "\n#+end_src\n"))
;;           (org-cycle) ; Dont expand the imputed python source
;;           ))))
;;   (org-hide-init))

;; (add-hook 'before-save-hook #'update-python-includes)
;; (add-hook 'before-save-hook #'python-include-docstrings)
