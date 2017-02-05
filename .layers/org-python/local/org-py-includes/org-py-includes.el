(require 'org)
(require 'flyspell)  ; async
(require 'python)  ; async

;;; Include Python Source
(defun org-hide-init ()
  (when (derived-mode-p 'org-mode)
    (save-window-excursion
      (save-excursion
        (goto-char (point-min))
        (when (search-forward-regexp "^* Init" nil 'noerror)
          (org-cycle))))))

(defun file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

;; #+INCLUDE: "src/file.py" :src python :func "statements"
;; or
;; #+INCLUDE: "src/file.py" :src python :func "fields" :lines "36-40"
(defun update-python-includes ()
  "Format is #+INCLUDE: \"file\" :src python :func \"func_def\""
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:func"
              nil 'noerror)
        (let* ((file (expand-file-name (match-string-no-properties 1))))
          (when (looking-at ".*\"\\([a-zA-Z_]+\\)\"")
            (setq py-incl-func (match-string-no-properties 1)))

          (setq py-incl-module (concat py-incl-func "\n" (file-contents file)))

          (save-excursion
            (org-babel-goto-named-src-block "extract-python-func-lines")
            (org-babel-execute-src-block)
            (org-babel-goto-named-result "extract-python-func-lines")
            (setq lines
                  (s-chomp (org-element-property :value (org-element-at-point)))))

          (if (looking-at ".*:lines *\\(\"[-0-9]+\"\\)")
              (replace-match lines :fixedcase :literal nil 1)
            (goto-char (line-end-position))
            (insert " :lines " lines))))))
  (org-hide-init))

;; #+INCLUDE-DOCS: "src/file.py"
(defun python-include-docstrings ()
  "Format is #+INCLUDE: \"file\""
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+INCLUDE-DOCS: *\"\\([^\"]+\\)\""
              nil 'noerror)
        (let* ((file (expand-file-name (match-string-no-properties 1))))
          (setq py-incl-module (file-contents file))

          (save-excursion
            (org-babel-goto-named-src-block "extract-python-docstrings")
            (org-babel-execute-src-block)
            (org-babel-goto-named-result "extract-python-docstrings")
            (setq py-docstrings
                  (org-element-property :value (org-element-at-point)))
            )

          (forward-line)
          (when (looking-at ".*begin_src python")
            (let* ((src (org-element-at-point))
                   (start (org-element-property :begin src))
                   (end (org-element-property :end src)))
              (delete-region start (- end 1))))  ; was deleting extra \n
          (save-excursion
            (insert "#+begin_src python\n" py-docstrings "\n#+end_src\n"))
          (org-cycle) ; Dont expand the imputed python source
          ))))
  (org-hide-init))

;;; Asynchronous Python Source Blocks
;; * Numbered lines in code blocks
(defvar number-line-overlays '()
  "List of overlays for line numbers.")

(make-variable-buffer-local 'number-line-overlays)

(defun number-line-clear ()
  "Clear the numbered lines in a code block."
  (mapc 'delete-overlay number-line-overlays)
  (setq number-line-overlays '())
  (remove-hook 'post-command-hook 'number-line-clear))


(defun number-line-src-block ()
  "Add line numbers to an org src-block."
  (interactive)

  (save-excursion
    (let* ((src-block (org-element-context))
           (nlines (- (length
                       (s-split
                        "\n"
                        (org-element-property :value src-block)))
                      1)))
      (goto-char (org-element-property :begin src-block))
      (re-search-forward (regexp-quote (org-element-property :value src-block)))
      (goto-char (match-beginning 0))

      (loop for i from 1 to nlines
            do
            (beginning-of-line)
            (let (ov)
              (setq ov (make-overlay (point) (point)))
              (overlay-put
               ov
               'before-string (propertize
                               (format "%03s:" (number-to-string i))
                               'font-lock-face '(:foreground "black" :background "gray80")))
              (add-to-list 'number-line-overlays ov))
            (next-line))))

  (add-hook 'post-command-hook 'number-line-clear))

(defvar org-babel-async-python-show-results nil
  "Determines if the async buffer is shown while the src block runs.")

(defvar org-babel-async-python-show-line-numbers t
  "Determines if line numbers are shown after an exception.")

(defun org-babel-async-execute:python (&optional arg)
  "Execute the python src-block at point asynchronously.
:var headers are supported.
:results output is all that is supported for output.
The variable `org-babel-async-python-show-results' determines if
a new window will pop up showing you the output as it appears,
and the output in that window will be put in the RESULTS section
of the code block. If there is an exception, the cursor will jump
back to the line it occurred on in the code block, and the files
in the traceback are clickable.
Use a prefix arg to force it to run if it is already running, or
there is an async marker present.
Note that if there are side effects from the code block these do
not get undone when you kill the process, e.g. if you modify
files.
To make C-c C-c use this, try this.
 (add-to-list 'org-ctrl-c-ctrl-c-hook 'org-babel-async-execute:python)"
  (interactive "P")
  (when (and (org-in-src-block-p)
             (string= "python" (nth 0 (org-babel-get-src-block-info))))
    (let* ((current-file (buffer-file-name))
           (cb (current-buffer))
           (code (org-element-property :value (org-element-context)))
           (varcmds (org-babel-variable-assignments:python
                     (nth 2 (org-babel-get-src-block-info))))
           (params (nth 2 (org-babel-get-src-block-info)))
           (wc (current-window-configuration))
           (file-line-regexp "File \"\\(.*\\)\",? line \\([0-9]*\\)")
           py-file
           md5-hash
           pbuffer
           process)

      ;; First, check if something is running
      (let ((location (org-babel-where-is-src-block-result))
            results)
        (when location
          (save-excursion
            (goto-char location)
            (when (looking-at (concat org-babel-result-regexp ".*$"))
              (setq results (buffer-substring-no-properties
                             location
                             (save-excursion
                               (forward-line 1) (org-babel-result-end)))))))
        (with-temp-buffer (insert (or results ""))
                          (goto-char (point-min))
                          (when (re-search-forward "<async:\\(.*\\)>" nil t)
                            (setq md5-hash (match-string 1))))
        (when md5-hash
          (if (and (get-process md5-hash) (not arg))
              (error "%s is running. Use prefix arg to kill it."
                     (match-string 0 results))
            ;; we want to kill stuff, delete results and continue. we either
            ;; asked for it to be killed, or the process is dead/stale
            (if (get-process md5-hash)
                (interrupt-process (format "*py-%s*" md5-hash))
              (when (get-buffer (format "*py-%s*" md5-hash))
                (kill-buffer (format "*py-%s*" md5-hash)))))))

      ;; Get the md5 for the current block
      (with-temp-buffer
        (dolist (cmd varcmds)
          (insert cmd)
          (insert "\n"))
        (insert code)
        (setq md5-hash (md5 (buffer-string))
              pbuffer (format "*py-%s*" md5-hash)
              py-file (expand-file-name (format "pymd5-%s.py" md5-hash))))

      ;; create the file to run
      (with-temp-file py-file
        (dolist (cmd varcmds)
          (insert cmd)
          (insert "\n"))
        (insert code))

      ;; get rid of old results, and put a place-holder for the new results to
      ;; come. The place holder is clickable, and kills the process.
      (org-babel-remove-result)
      (org-babel-insert-result
       (format "<Open results> <async:%s> click to kill" md5-hash)
       (cdr (assoc :result-params
                   (nth 2 (org-babel-get-src-block-info)))))

      ;; make the placeholder clickable
      (save-excursion
        (re-search-forward (format "<async:%s> click to kill" md5-hash))
        (flyspell-delete-region-overlays (match-beginning 0) (match-end 0))
        (let ((map (make-sparse-keymap)))
          (define-key map [mouse-1]
            (lambda ()
              (interactive)
              (org-babel-previous-src-block)
              (org-babel-kill-async)))
          (set-text-properties
           (match-beginning 0) (match-end 0)
           `(font-lock-face (:foreground "red")
                            local-map ,map
                            mouse-face highlight
                            help-echo "Click to kill async process"))))
      (save-excursion
        (re-search-forward "<Open results>")
        (flyspell-delete-region-overlays (match-beginning 0) (match-end 0))
        (let ((map (make-sparse-keymap)))
          (define-key map [mouse-1]
            `(lambda ()
               (interactive)
               (pop-to-buffer ,pbuffer)
               (use-local-map (copy-keymap org-mode-map))
               (setq header-line-format "Press q to quit. Press k to abort.")
               (local-set-key "q"
                              #'(lambda ()
                                  (interactive)
                                  (delete-window)))
               (local-set-key "k"
                              #'(lambda ()
                                  (interactive)
                                  (quit-window t)
                                  (switch-to-buffer ,cb)
                                  (goto-char (point-min))
                                  (re-search-forward ,md5-hash)
                                  (org-babel-previous-src-block)
                                  (org-babel-kill-async)))))

          (set-text-properties
           (match-beginning 0) (match-end 0)
           `(font-lock-face (:foreground "green4")
                            local-map ,map
                            mouse-face highlight
                            help-echo "Click to open results buffer"))))

      (setq font-lock-extra-managed-props (delq 'local-map font-lock-extra-managed-props))

      ;; open the results buffer to see the results in when we want it.
      (when org-babel-async-python-show-results
        (switch-to-buffer-other-window pbuffer))

      ;; run the code
      (setq process (start-process
                     md5-hash
                     pbuffer
                     "python"
                     py-file))


      ;; when the process is done, run this code to put the results in the
      ;; org-mode buffer.
      (set-process-sentinel
       process
       `(lambda (process event)
          (delete-file ,py-file)
          (let* ((line-number))
            (unless (string= "finished\n" event)
              ;; Probably got an exception. Let's parse it and move
              ;; point to where it belongs in the code block.
              (with-current-buffer ,pbuffer
                (setq results (buffer-string))
                (goto-char (point-min))
                ;; get the last line that matches the code block
                (while (re-search-forward
                        (format "\"\\(%s\\)\", line \\([0-9]+\\)" ,py-file) nil t)
                  (replace-match "Org SRC" nil nil nil 1)
                  (setq line-number (string-to-number (match-string 2))))))

            ;; Now get the results and insert them
            (save-window-excursion
              (save-excursion
                (save-restriction
                  ;; Make sure we end up deleting the temp file and buffer
                  (unwind-protect
                      (with-current-buffer ,cb
                        (widen)
                        (goto-char (point-min))
                        (when (re-search-forward
                               (format "<async:%s>" ,md5-hash)
                               nil t)
                          (org-babel-previous-src-block)
                          (org-babel-remove-result)
                          (org-babel-insert-result
                           (with-current-buffer ,pbuffer
                             (buffer-string))
                           (cdr (assoc :result-params
                                       (nth 2 (org-babel-get-src-block-info)))))))
                    ;; delete the results buffer then delete the tempfile.
                    ;; finally, delete the process.
                    (when (get-buffer ,pbuffer)
                      (kill-buffer ,pbuffer))
                    (when process
                      (delete-process process))))))

            ;; restore window configuration
            (set-window-configuration ,wc)
            (org-redisplay-inline-images)

            ;; Finally, if we got a line number, add click properties to file
            ;; lines, move point and shine beacon
            (when line-number
              (save-excursion
                (while (re-search-forward ,file-line-regexp nil t)
                  (let ((map (make-sparse-keymap))
                        (start (match-beginning 1))
                        (end (match-end 1))
                        (fname (match-string 1))
                        (ln (string-to-number (match-string 2))))
                    (define-key map [mouse-1]
                      `(lambda ()
                         (interactive)
                         (if (string-match "Org SRC" ,fname)
                             (progn
                               (org-babel-previous-src-block)
                               (goto-char (org-element-property :begin (org-element-context)))
                               (forward-line ,ln)
                               ;; For some reason clicking on these links
                               ;; sometimes folds the results drawer. This makes
                               ;; sure it is unfolded.
                               (when (-contains? (cdr
                                                  (assoc
                                                   :result-params
                                                   (nth 2 (org-babel-get-src-block-info))))
                                                 "drawer")
                                 (save-excursion
                                   (search-forward ":RESULTS:")
                                   (org-flag-drawer nil))))
                           ;; regular file
                           (find-file ,fname)
                           (goto-line ,ln))))
                    (flyspell-delete-region-overlays start end)
                    (set-text-properties
                     start
                     end
                     `(font-lock-face org-link
                                      mouse-face highlight
                                      local-map ,map
                                      help-echo "Click to open")))))

              (goto-char (org-element-property :begin (org-element-context)))
              (forward-line (- line-number (length (org-babel-variable-assignments:python
                                                    (nth 2 (org-babel-get-src-block-info))))))
              (message "%s" results)
              (when org-babel-async-python-show-line-numbers
                (number-line-src-block)))))))))


(defun org-babel-kill-async ()
  "Kill the current async process.
Run this in the code block that is running."
  (interactive)
  (let ((location (org-babel-where-is-src-block-result))
        results)
    (when location
      (save-excursion
        (goto-char location)
        (when (looking-at (concat org-babel-result-regexp ".*$"))
          (setq results (buffer-substring-no-properties
                         location
                         (save-excursion
                           (forward-line 1) (org-babel-result-end)))))))
    (when (and results (string-match "<async:\\(.*\\)>" results))
      (interrupt-process (match-string 1 results)))))

(provide 'org-py-includes)
