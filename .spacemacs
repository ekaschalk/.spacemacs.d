;; -*- mode: emacs-lisp -*-
;;; Spacemacs-Layers
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '((shell :variables
            shell-default-shell 'eshell)
     (auto-completion :variables
                      auto-completion-return-key-behavior 'cycle
                      auto-completion-tab-key-behavior 'complete
                      ;; auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t)
     better-defaults helm git org ranger syntax-checking version-control
     graphviz restclient
     emacs-lisp html python
     )
   dotspacemacs-additional-packages '(outshine navi-mode virtualenvwrapper)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-but-keep-unused))
;;; Spacemacs-Init
(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '()
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-themes '(spacemacs-dark spacemacs-light zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Fira Code"
                               :size 12
                               :weight bold
                               :width condensed
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ())

;;; Spacemacs-Config
(defun dotspacemacs/user-config ()

;;;; TODOS

;;;; Auto-completion
  ;; (global-company-mode) ; Non-major modes get completion
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

;;;; Windows Frame Size Fix
  ;; This needs to occur early in config, not ideal solution
  (add-to-list 'default-frame-alist '(font . "Fira Code"))
  (set-face-attribute 'default t :font "Fira Code")
  (defun ek/fix () (mapc (lambda (x) (zoom-frm-out)) '(1 2)))  ; 80 chars zoom

;;;; Outshine-mode
  ;; TODO Add promote/demote outline heading, not outline subtree
  (require 'outshine)
  (require 'navi-mode)

  (setq outshine-use-speed-commands t)
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'prog-mode-hook 'outline-minor-mode)

;;;;; Navi bindings
  (add-to-list 'navi-key-mappings
               '("python" .
                 ((:FUN . "f")
                  ;; (:VAR . "v")
                  (:OBJ . "x"))))

  (add-to-list 'navi-keywords
               '("python" .
                 ((:FUN . "\\(^[ ]*def[a-zA-Z0-9_ ]*\\|^[ ]*class[a-zA-Z0-9_ ]*\\)")
                  (:OBJ . "^[ ]*\\(class[a-zA-Z0-9_ ]*\\)"))))

  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'navi-cycle-subtree)
    (define-key map (kbd "<backtab>") 'navi-cycle-buffer)

    (define-key map (kbd "M-h") 'navi-promote-subtree)
    (define-key map (kbd "M-j") 'navi-move-down-subtree)
    (define-key map (kbd "M-k") 'navi-move-up-subtree)
    (define-key map (kbd "M-l") 'navi-demote-subtree)
    (define-key map (kbd "M-n")
      (lambda () (interactive) (navi-goto-occurrence-other-window)
        (recenter 3)))  ; Also binded to "o", this one is for consistency

    (evil-define-key '(normal visual motion) map
      "f" (lambda () (interactive) (navi-generic-command ?f current-prefix-arg)) ;Fun
      "v" (lambda () (interactive) (navi-generic-command ?v current-prefix-arg)) ;Var
      "x" (lambda () (interactive) (navi-generic-command ?x current-prefix-arg)) ;Obj
      "a" (lambda () (interactive) (navi-generic-command ?a current-prefix-arg)) ;All
      "1" (lambda () (interactive) (navi-generic-command ?1 current-prefix-arg))
      "2" (lambda () (interactive) (navi-generic-command ?2 current-prefix-arg))
      "3" (lambda () (interactive) (navi-generic-command ?3 current-prefix-arg))
      "4" (lambda () (interactive) (navi-generic-command ?4 current-prefix-arg))

      "u" 'navi-undo
      "n" 'navi-narrow-to-thing-at-point
      "w" 'navi-widen

      "d" (lambda () (interactive) (occur-mode-display-occurrence)
            (other-window 1) (recenter 3) (other-window 1))
      "o" (lambda () (interactive) (navi-goto-occurrence-other-window)
            (recenter 3))  ; 3 lines from top is good spacing
      "q" (lambda () (interactive) (navi-quit-and-switch)
            (delete-other-windows) (recenter 3)))

    (setq navi-mode-map map))

;;;;; Outshine bindings
  (defun my-outshine-navi ()
    (interactive)
    (let ((line nil))
      (widen)  ; Broken on narrowed buffers
      (save-excursion
        (outline-previous-visible-heading 1)
        (setq line
              (replace-regexp-in-string "\n$" ""
                                        (thing-at-point 'line t))))
      (outshine-navi)
      (navi-generic-command ?2 nil)  ; default to 2 heading levels
      (search-forward-regexp line)))

  ;; Org doesnt use outline minor mode but can utilize navi
  ;; TODO remove tags from string in org mode
  (define-key org-mode-map (kbd "M-n") 'my-outshine-navi)

  ;; Outline minor mode vim keybindings
  (let ((map outline-minor-mode-map))
    (define-key map (kbd "M-n") 'my-outshine-navi)

    (define-key map (kbd "C-M-<return>")  ; insert-subheading
      (lambda ()
        (interactive)
        (let ((line nil) (str nil))
          (save-excursion
            (outline-previous-visible-heading 1)
            (setq level (outshine-calc-outline-level))
            (setq str (outshine-calc-outline-string-at-level (+ 1 level))))
          (evil-unimpaired/insert-space-below 1)
          (evil-next-line 1)
          (insert str))))

    (define-key map (kbd "M-RET") 'outshine-insert-heading)
    (define-key map (kbd "<backtab>") 'outshine-cycle-buffer)
    (define-key map (kbd "M-h") 'outline-promote)
    (define-key map (kbd "M-l") 'outline-demote)

    (evil-define-key '(normal visual motion) map
      "gh" 'outline-up-heading
      "gj" 'outline-forward-same-level
      "gk" 'outline-backward-same-level
      "gl" 'outline-next-visible-heading
      "gu" 'outline-previous-visible-heading

      (kbd "SPC n n") (lambda ()
                        (interactive)
                        (save-excursion
                          (outline-previous-visible-heading 1)
                          (outshine-narrow-to-subtree)))
      (kbd "SPC n j") 'outline-move-subtree-down
      (kbd "SPC n k") 'outline-move-subtree-up))

;;;; Evil
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-unordered-key-sequence "true")

;;;; Toggles
  (spacemacs/toggle-highlight-long-lines-globally-on)
  (fringe-mode '(1 . 1))  ; Minimal left padding and ~ end newline markers
  (rainbow-delimiters-mode-enable)  ; Paren color based on depth
  (global-highlight-parentheses-mode 1)  ; Highlight containing parens
  (hungry-delete-mode 1)  ; in edit mode back gets all contiguous whitespace
  (add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))  ; SPC splits past 80
  (spacemacs/toggle-aggressive-indent-globally-on)  ; auto-indentation
  (spacemacs/toggle-mode-line-minor-modes-off)  ; no unicode symbs next to major
  (linum-relative-global-mode 1)  ; very useful for multi-line vim motions
  (global-prettify-symbols-mode 1)  ; eg. lambda, python lots of config

;;;; Python
;;;;; Virtual Environments
  (require 'virtualenvwrapper)
  (pyvenv-mode 1)
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)

  (defun pyvenv-autoload ()
    (when (string= buffer-file-name "c:/~/dev/pop-synth/base.org")
      (pyvenv-workon "pop-synthvenv"))
    (when (string= buffer-file-name "c:/~/dev/health/base.org")
      (pyvenv-workon "healthvenv")))

  (add-hook 'org-mode-hook 'pyvenv-autoload)

;;;;; Mypy
  (defun mypy-show-region ()
    (interactive)
    (org-edit-src-exit)
    (shell-command
     (format "mypy --ignore-missing-imports --fast-parser --python-version 3.6 %s&" (ek/file-path)))
    (org-edit-src-code))

  ;; (define-key python-mode-map (kbd "C-c m") 'mypy-show-region)

;;;;; Include Org Integration
  (defun org-hide-init ()
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward-regexp "^* Init" nil 'noerror)
          (org-cycle)))))

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
    (org-hide-init)
    )

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
    (org-hide-init)
    )

  (add-hook 'before-save-hook #'update-python-includes)
  (add-hook 'before-save-hook #'python-include-docstrings)


;;;;; Asynchronous Python Source execution
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

  (add-to-list 'org-ctrl-c-ctrl-c-hook 'org-babel-async-execute:python)

;;;; Org

  (require 'ox-extra)
  (setq org-bullets-bullet-list '("■" "○" "✸" "✿")
        org-priority-faces '((65 :foreground "red")
                             (66 :foreground "yellow")
                             (67 :foreground "blue")))
  (ox-extras-activate '(ignore-headlines))

;;;;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window)
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (dot . t)
                               (http . t)))
;;;;; Exporting
  (require 'ox-bibtex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-html-htmlize-output-type 'inline-css
        org-latex-listings 'minted
        org-latex-minted-options
        '(("frame" "lines")
          ("fontsize" "\\scriptsize")
          ("xleftmargin" "\\parindent")
          ("linenos" ""))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;;;; Templates
  (mapc (lambda (x) (add-to-list 'org-structure-template-alist x))
        (list
         ;; Common
         '("n" "#+NAME: ?")
         ;; Emacs-Lisp
         '("e" "#+begin_src emacs-lisp\n\n#+end_src")
         ;; Python
         '("p" "#+begin_src python\n\n#+end_src")
         '("pd" "#+begin_src python :tangle no :results output\n\n#+end_src")
         '("pt" "#+begin_src python :results silent :exports none\n\n#+end_src")
         ;; Misc
         '("c" " :PROPERTIES:\n :HTML_CONTAINER_CLASS: hsCollapsed\n :END:\n")
         `("d" ,(concat
                 "#+begin_src dot :tangle no :exports results :file static/imgs/"
                 "\n\n#+end_src"))
         ;; Project File header
         `("f" ,(concat
                 "# -*- org-use-tag-inheritance: nil"
                 " org-babel-use-quick-and-dirty-noweb-expansion: t-*-\n"
                 "#+BEGIN_QUOTE\n#+PROPERTY: header-args :eval never-export"
                 " :noweb no-export\n#+PROPERTY: header-args:python"
                 " :tangle (ek/file-path)\n#+END_QUOTE\n"))))

;;;;; Funcs
;;;;;; Toggle blocks
  (defvar org-blocks-hidden nil)
  (defun org-toggle-blocks ()
    (interactive)
    (if org-blocks-hidden
        (org-show-block-all)
      (org-hide-block-all))
    (setq-local org-blocks-hidden (not org-blocks-hidden)))

;;;;;; Tangling
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

;;;;;; Projects
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

;;;;; Hooks and Keymappings
  (add-hook 'org-mode-hook 'flyspell-mode)  ; Async python, spelling
  (add-hook 'org-mode-hook 'org-toggle-blocks)
  (add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

  (define-key org-mode-map
    (kbd "C-c t") 'org-toggle-blocks)
  (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
    (kbd "RET") 'ek/tangle-in-src-edit)
  (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
    (kbd "t") 'ek/test-in-src-edit)

;;;; Display
;;;;; Themes
  (custom-theme-set-faces
   'spacemacs-dark
   '(outline-1 ((t (:inherit org-level-1 :underline t))))
   '(outline-2 ((t (:inherit org-level-2 :underline t))))
   '(outline-3 ((t (:inherit org-level-3 :underline t))))
   '(outline-4 ((t (:inherit org-level-4 :underline t)))))

;;;;; Font Ligatures
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

  (defconst fira-code-font-lock-keywords-alist
    (mapcar
     (lambda (regex-char-pair)
       `(,(car regex-char-pair)
         (0 (prog1 ()
              (compose-region
               (match-beginning 1)
               (match-end 1)
               ,(concat "	"
                        (list (decode-char 'ucs (cadr regex-char-pair)))))))))
     '(;; ("[^/]\\(\\*\\*\\)[^/]"        #Xe101) ("\\(\\*\\*\\*\\)"             #Xe102)
       ;; ("\\(\\*\\*/\\)"               #Xe103) ("\\(\\*>\\)"                  #Xe104)
       ;; ("[^*]\\(\\*/\\)"              #Xe105) ("\\(\\[\\]\\)"                #Xe109)
       ;; ("\\(/\\*\\)"                  #Xe12a) ("\\(/\\*\\*\\)"               #Xe12b)
       ;; ("\\(<\\*\\)"                  #Xe14b) ("\\(<\\*>\\)"                 #Xe14c)
       ;; ("\\(x\\)"                     #Xe16b)
       ("\\(www\\)"                   #Xe100) ("\\(\\\\\\\\\\)"              #Xe106)
       ("\\(\\\\\\\\\\\\\\)"          #Xe107) ("\\({-\\)"                    #Xe108)
       ("\\(::\\)"                    #Xe10a) ("\\(:::\\)"                   #Xe10b)
       ("[^=]\\(:=\\)"                #Xe10c) ("\\(!!\\)"                    #Xe10d)
       ("\\(!=\\)"                    #Xe10e) ("\\(!==\\)"                   #Xe10f)
       ("\\(-}\\)"                    #Xe110) ("\\(--\\)"                    #Xe111)
       ("\\(---\\)"                   #Xe112) ("\\(-->\\)"                   #Xe113)
       ("[^-]\\(->\\)"                #Xe114) ("\\(->>\\)"                   #Xe115)
       ("\\(-<\\)"                    #Xe116) ("\\(-<<\\)"                   #Xe117)
       ("\\(-~\\)"                    #Xe118) ("\\(#{\\)"                    #Xe119)
       ("\\(#\\[\\)"                  #Xe11a) ("\\(##\\)"                    #Xe11b)
       ("\\(###\\)"                   #Xe11c) ("\\(####\\)"                  #Xe11d)
       ("\\(#(\\)"                    #Xe11e) ("\\(#\\?\\)"                  #Xe11f)
       ("\\(#_\\)"                    #Xe120) ("\\(#_(\\)"                   #Xe121)
       ("\\(\\.-\\)"                  #Xe122) ("\\(\\.=\\)"                  #Xe123)
       ("\\(\\.\\.\\)"                #Xe124) ("\\(\\.\\.<\\)"               #Xe125)
       ("\\(\\.\\.\\.\\)"             #Xe126) ("\\(\\?=\\)"                  #Xe127)
       ("\\(\\?\\?\\)"                #Xe128) ;;("\\(;;\\)"                    #Xe129)
       ("\\(/=\\)"                    #Xe12c) ("\\(/==\\)"                   #Xe12d)
       ("\\(/>\\)"                    #Xe12e) ("\\(//\\)"                    #Xe12f)
       ("\\(///\\)"                   #Xe130) ("\\(&&\\)"                    #Xe131)
       ("\\(||\\)"                    #Xe132) ("\\(||=\\)"                   #Xe133)
       ("[^|]\\(|=\\)"                #Xe134) ("\\(|>\\)"                    #Xe135)
       ("\\(\\^=\\)"                  #Xe136) ("\\(\\$>\\)"                  #Xe137)
       ("\\(\\+\\+\\)"                #Xe138) ("\\(\\+\\+\\+\\)"             #Xe139)
       ("\\(\\+>\\)"                  #Xe13a) ("\\(=:=\\)"                   #Xe13b)
       ("[^!/]\\(==\\)[^>]"           #Xe13c) ("\\(===\\)"                   #Xe13d)
       ("\\(==>\\)"                   #Xe13e) ("[^=]\\(=>\\)"                #Xe13f)
       ("\\(=>>\\)"                   #Xe140) ("\\(<=\\)"                    #Xe141)
       ("\\(=<<\\)"                   #Xe142) ("\\(=/=\\)"                   #Xe143)
       ("\\(>-\\)"                    #Xe144) ("\\(>=\\)"                    #Xe145)
       ("\\(>=>\\)"                   #Xe146) ("[^-=]\\(>>\\)"               #Xe147)
       ("\\(>>-\\)"                   #Xe148) ("\\(>>=\\)"                   #Xe149)
       ("\\(>>>\\)"                   #Xe14a) ("\\(<|\\)"                    #Xe14d)
       ("\\(<|>\\)"                   #Xe14e) ("\\(<\\$\\)"                  #Xe14f)
       ("\\(<\\$>\\)"                 #Xe150) ("\\(<!--\\)"                  #Xe151)
       ("\\(<-\\)"                    #Xe152) ("\\(<--\\)"                   #Xe153)
       ("\\(<->\\)"                   #Xe154) ("\\(<\\+\\)"                  #Xe155)
       ("\\(<\\+>\\)"                 #Xe156) ("\\(<=\\)"                    #Xe157)
       ("\\(<==\\)"                   #Xe158) ("\\(<=>\\)"                   #Xe159)
       ("\\(<=<\\)"                   #Xe15a) ("\\(<>\\)"                    #Xe15b)
       ("[^-=]\\(<<\\)"               #Xe15c) ("\\(<<-\\)"                   #Xe15d)
       ("\\(<<=\\)"                   #Xe15e) ("\\(<<<\\)"                   #Xe15f)
       ("\\(<~\\)"                    #Xe160) ("\\(<~~\\)"                   #Xe161)
       ("\\(</\\)"                    #Xe162) ("\\(</>\\)"                   #Xe163)
       ("\\(~@\\)"                    #Xe164) ("\\(~-\\)"                    #Xe165)
       ("\\(~=\\)"                    #Xe166) ("\\(~>\\)"                    #Xe167)
       ("[^<]\\(~~\\)"                #Xe168) ("\\(~~>\\)"                   #Xe169)
       ("\\(%%\\)"                    #Xe16a) ("[^:=]\\(:\\)[^:=]"           #Xe16c)
       ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d))))

  (defun match-outline-levels (regex-char-pair)
    `(,(car regex-char-pair)
      (0 (prog1 ()
           (compose-region
            (match-beginning 1)
            (match-end 1)
            ,(concat "	"
                     (list (cadr regex-char-pair))))))))


;;;;; Custom Ligatures
  (defconst emacs-lisp-prettify-pairs
    (mapcar 'match-outline-levels
            '(("\\(^;;;\\)"                   ?■)
              ("\\(^;;;;\\)"                  ?○)
              ("\\(^;;;;;\\)"                 ?✸)
              ("\\(^;;;;;;\\)"                ?✿))))

  (defconst python-prettify-pairs
    (mapcar 'match-outline-levels
            '(("\\(^# \\*\\)[ \t\n]"          ?■)
              ("\\(^# \\*\\*\\)[ \t\n]"       ?○)
              ("\\(^# \\*\\*\\*\\)[ \t\n]"    ?✸)
              ("\\(^# \\*\\*\\*\\*\\)[^\\*]"  ?✿)
              ("\\(_0\\)[: \t\n]"             ?₀)
              ("\\(_1\\)[: \t\n]"             ?₁)
              ("\\(_2\\)[: \t\n]"             ?₂)
              ("\\(_3\\)[: \t\n]"             ?₃)
              ("\\(_4\\)[: \t\n]"             ?₄)
              ("\\(_i\\)[: \t\n]"             ?ᵢ)
              ("\\(_j\\)[: \t\n]"             ?ⱼ)
              ("\\(_k\\)[: \t\n]"             ?ₖ)
              ("\\(_m\\)[: \t\n]"             ?ₘ)
              ("\\(_n\\)[: \t\n]"             ?ₙ)
              ("\\(_x\\)[: \t\n]"             ?ₓ)
              ("\\(alpha\\)"            ?\u03B1) ; α
              ("\\(beta\\)"             ?\u03B2) ; β
              ("\\(gamma\\)"            ?\u03B3) ; γ
              ("\\(delta\\)"            ?\u03B4) ; δ
              ("\\(epsilon\\)"          ?\u03B5) ; ε
              ("\\(zeta\\)"             ?\u03B6) ; ζ
              ("\\(theta\\)"            ?\u03B8) ; θ
              ("\\(iota\\)"             ?\u03B9) ; ι
              ("\\(kappa\\)"            ?\u03BA) ; κ
              ;; ("\\(mu\\)"               ?\u03BC) ; μ breaks accumulate
              ;; ("\\(xi\\)"               ?\u03BE) ; ξ breaks axis
              ("\\(omicron\\)"          ?\u03BF) ; ο
              ;; ("\\(pi\\)"               ?\u03C0) ; π breaks eg capitalize
              ("\\(rho\\)"              ?\u03C1) ; ρ
              ("\\(sigma\\)"            ?\u03C3) ; σ
              ("\\(tau\\)"              ?\u03C4) ; τ
              ("\\(phi\\)"              ?\u03C6) ; φ
              ("\\(chi\\)"              ?\u03C7) ; χ
              ("\\(omega\\)"            ?\u03C9) ; ω
              )))

  (defun add-fira-code-symbol-keywords ()
    (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))
  (defun emacs-lisp-prettify-keywords ()
    (font-lock-add-keywords nil emacs-lisp-prettify-pairs))
  (defun python-prettify-keywords ()
    (font-lock-add-keywords nil python-prettify-pairs))

  (add-hook 'org-mode-hook
            #'add-fira-code-symbol-keywords)
  (add-hook 'prog-mode-hook
            #'add-fira-code-symbol-keywords)

  (add-hook 'emacs-lisp-mode-hook
            #'emacs-lisp-prettify-keywords)
  (add-hook 'python-mode-hook
            #'python-prettify-keywords)

;;;;; Prettify Symbols
  ;; Greeks not done through pretty symbols since that breaks subscripts
  ;; Fixes for unicode not picking up a default font on some chars
  ;; https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
  (set-fontset-font "fontset-default" '(#x2c7c . #x2c7c) "Courier New")
  (set-fontset-font "fontset-default" '(#x1d518 . #x1d518) "Symbola")
  (set-fontset-font "fontset-default" '(#x1d4d0 . #x1d4e2) "Symbola")
  (set-fontset-font "fontset-default" '(#x1d4d0 . #x1d54a) "Symbola")
  (set-fontset-font "fontset-default" '(#x1d54a . #x1d572) "Symbola")

  (add-hook 'python-mode-hook
            (lambda ()
              (mapc (lambda (pair) (push pair prettify-symbols-alist))
                    '(;; Syntax
                      ("not" .      ?❗) ; ¬
                      ("for" .      ?∀)
                      ("in" .       ?∊)
                      ("not in" .   ?∉)
                      ("return" .  ?⟼)
                      ("yield" .   ?⟻)
                      ;; Base Types
                      ("None" .     ?∅)
                      ("int" .      ?ℤ)
                      ("float" .    ?ℝ)
                      ("str" .      ?𝕊)
                      ("True" .     ?𝕋)
                      ("False" .    ?𝔽)
                      ;; Mypy Containers
                      ("Dict" .     ?𝔇)
                      ("List" .     ?ℒ)
                      ("Callable" . ?ℱ)
                      ("Iterable" . ?𝔊)
                      ("Set" .      ?Ω)
                      ;; Mypy Compositions
                      ("Any" .      ?❔) ; ？ ❓
                      ("Tuple" .    ?⨂)
                      ("Union" .    ?⋃)
                      ;; Other
                      ("**2" .      ?²)
                      ("sum" .      ?∑)))))

  )

;;;; Misc
;;;;; Projectile
(setq projectile-indexing-method 'native)  ; respect .projectile files

;;;;; Aspell
(setq ispell-program-name "aspell")

;;;; Future
;;;;; Considering
;; (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; smart-parens: https://github.com/Fuco1/smartparens/wiki
;; (substitute-key-definition 'old-def 'new-def map)

;;;;; Emacs-client
;; http://psung.blogspot.com/2009/05/using-itsalltext-with-emacsemacsclient.html
;; https://github.com/docwhat/itsalltext

;;; Spacemacs-Autogen
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(evil-want-Y-yank-to-eol t)
 '(package-selected-packages
   (quote
    (helm-company helm-c-yasnippet company-web web-completion-data company-statistics company-restclient know-your-http-well company-anaconda company auto-yasnippet yasnippet ac-ispell auto-complete navi-mode outshine outorg window-purpose imenu-list zenburn-theme yapfify xterm-color web-mode virtualenvwrapper unfill tagedit smeargle slim-mode shell-pop scss-mode sass-mode restclient-helm ranger pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements orgit org-projectile org-present org-pomodoro alert log4e gntp org-download ob-restclient restclient ob-http mwim multi-term magit-gitflow live-py-mode less-css-mode hy-mode htmlize helm-pydoc helm-gitignore helm-css-scss haml-mode graphviz-dot-mode gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor eshell-z eshell-prompt-extras esh-help emmet-mode diff-hl cython-mode anaconda-mode pythonic ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
 '(safe-local-variable-values
   (quote
    ((eval ek/startup-proj)
     (org-babel-use-quick-and-dirty-noweb-expansion . t)
     (org-use-tag-inheritance)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
