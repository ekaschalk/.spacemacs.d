;;; pretty-magit.el --- Look up python documents (reference) in Emacs

;; Copyright (C) 2017-2017 Eric Kaschalk

;; Author: Eric Kaschalk <ekaschalk@gmail.com>
;; Maintainer: Eric Kaschalk <ekaschalk@gmail.com>
;; Created: July 7th 2017
;; Keywords: magit,version-control,display

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; Add faces to Magit manually for things like commit headers eg. (Add: ...).

;; Adding faces to Magit is non-trivial since any use of font-lock will break
;; fontification of the buffer. This is due to Magit doing all styling with
;; `propertize' and black magic. So we apply the faces the manual way.

;; Adds Ivy integration so a prompt of (Add, Docs, ...) appears when commiting.
;; Eexplore icons by evaluating eg.: (all-the-icons-insert-icons-for 'material)

(setq pretty-magit-alist nil)
(setq pretty-magit-prompt nil)

(defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
  "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
  `(progn
     (add-to-list 'pretty-magit-alist
                  (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
                        ,ICON ',PROPS))
     (unless ,NO-PROMPT?
       (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

;;;###autoload
(defun add-magit-faces ()
  "Add face properties and compose symbols for buffer from pretty-magit."
  (interactive)
  (with-silent-modifications
    (--each pretty-magit-alist
      (-let (((rgx icon props) it))
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp rgx nil t)
            (compose-region
             (match-beginning 1) (match-end 1) icon)
            (when props
              (add-face-text-property
               (match-beginning 1) (match-end 1) props))))))))

;;; Leader Prompts

(setq use-magit-commit-prompt-p nil)
(defun use-magit-commit-prompt (&rest args)
  (setq use-magit-commit-prompt-p t))

;;;###autoload
(defun magit-commit-prompt ()
  "Magit prompt and insert commit header with faces."

  (interactive)
  (when use-magit-commit-prompt-p
    (setq use-magit-commit-prompt-p nil)
    (insert (ivy-read "Commit Type " pretty-magit-prompt
                      :require-match t :sort t :preselect "Add: "))
    (add-magit-faces)
    (evil-insert 1)))


(provide 'pretty-magit)
;;; pretty-magit.el ends here
