;; (require 'evil)  ; Remove evil-insert at end if not using evil
(require 'ivy)  ; for leader-prompts only
(require 'magit)
(require 'macros)

(provide 'pretty-magit)

;;; Pretty-magit

(defvar pretty-magit-alist nil
  "An alist of regexes, an icon, and face properties to apply to icon.")

(defvar pretty-magit-prompt nil
  "A list of commit leader prompt candidates.")

;;;###autoload
(defmacro pretty-magit-add-leader (word icon props &optional no-prompt?)
  "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
  `(progn
     (add-to-list 'pretty-magit-alist
                  (list (rx bow
                            (group ,word (eval (if ,no-prompt? "" ":"))))
                        ,icon
                        ',props))
     (unless ,no-prompt?
       (add-to-list 'pretty-magit-prompt
                    (concat ,word ": ")))))

;;;###autoload
(defun pretty-magit-add-magit-faces ()
  "Add face properties and compose symbols for buffer from pretty-magit."
  (interactive)
  (with-silent-modifications
    (--each pretty-magit-alist
      (-let [(rgx icon props) it]
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp rgx nil t)
            (let ((start (match-beginning 1))
                  (end (match-end 1)))
              (compose-region start end icon)
              (when props
                (add-face-text-property start end props)))))))))

;;; Leader Prompts

(defvar pretty-magit--use-magit-commit-prompt? nil
  "Do we need to use the magit commit prompt?")

(defun pretty-magit-use-magit-commit-prompt (&rest args)
  (setq pretty-magit--use-magit-commit-prompt? t))

;;;###autoload
(defun magit-commit-prompt ()
  "Magit prompt and insert commit header with faces."
  (interactive)
  (when pretty-magit--use-magit-commit-prompt?
    (setq pretty-magit--use-magit-commit-prompt?
          nil)
    (insert (ivy-read "Commit Type "
                      pretty-magit-prompt
                      :require-match t
                      :sort t
                      :preselect "Add: "))
    (pretty-magit-add-magit-faces)
    (evil-insert 1)))

;;; Hooks

(remove-hook 'git-commit-setup-hook
             'with-editor-usage-message)
(add-hook 'git-commit-setup-hook
          'magit-commit-prompt)

(advice-add 'magit-status :after
            'pretty-magit-add-magit-faces)
(advice-add 'magit-refresh-buffer :after
            'pretty-magit-add-magit-faces)
(advice-add 'magit-commit :after
            'pretty-magit-use-magit-commit-prompt)
