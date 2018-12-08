;;; pretty-magit.el --- Magit commit leader completion and styling -*- lexical-binding: t; -*-

;;; Commentary:

;; Commit leader support, compose leaders with any character of your choice
;; and prompt from them when committing with magit.

;;; Code:
;;;; Requires

(require 'dash)
(require 'dash-functional)
(require 'magit)

(require 'ivy)  ; If using commit prompts

;;;; Configuration

(defvar pretty-magit--alist nil
  "An alist of regexes, an icon, and face properties to apply to icon.")

(defvar pretty-magit--prompt nil
  "A list of commit leader prompt candidates.")

(defvar pretty-magit--use-commit-prompt? nil
  "Do we need to use the magit commit prompt?")

;;;; Compositions

(defun pretty-magit--add-magit-faces ()
  "Add face properties and compose symbols for buffer from pretty-magit."
  (interactive)
  (with-silent-modifications
    (-each pretty-magit--alist
      (-lambda ((rgx char face-props))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward rgx nil t)
            (-let [(start end) (match-data 1)]
              (compose-region start end char)
              (when face-props
                (add-face-text-property start end face-props)))))))))

(defun pretty-magit-add-leader (word char face-props)
  "Replace sanitized WORD with CHAR having FACE-PROPS and add to prompts."
  (add-to-list 'pretty-magit--alist
               (list (rx-to-string `(: bow
                                       (group ,word ":")))
                     char face-props))
  (add-to-list 'pretty-magit--prompt
               (concat word ": ")))

(defun pretty-magit-add-leaders (leaders)
  "Map `pretty-magit-add-leader' over LEADERS."
  (-each leaders
    (-applify #'pretty-magit-add-leader)))

;;;; Prompts

(defun pretty-magit--use-commit-prompt (&rest args)
  (setq pretty-magit--use-commit-prompt? t))

(defun pretty-magit-commit-prompt ()
  "Magit prompt and insert commit header with faces."
  (interactive)
  (when (and pretty-magit--use-commit-prompt?
             pretty-magit--prompt)
    (setq pretty-magit--use-commit-prompt? nil)
    (insert (ivy-read "Commit Type " pretty-magit--prompt
                      :require-match t
                      :sort t
                      :preselect "Add: "))
    (pretty-magit--add-magit-faces)
    (evil-insert 1)))

;;;; Enable

;;;###autoload
(defun pretty-magit-setup (&optional no-commit-prompts?)
  "Advise the appropriate magit funcs to add pretty-magit faces."
  (advice-add 'magit-status         :after 'pretty-magit--add-magit-faces)
  (advice-add 'magit-refresh-buffer :after 'pretty-magit--add-magit-faces)

  (unless no-commit-prompts?
    (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
    (add-hook    'git-commit-setup-hook 'pretty-magit-commit-prompt)

    (advice-add 'magit-commit-create :after 'pretty-magit--use-commit-prompt)))


(provide 'pretty-magit)
