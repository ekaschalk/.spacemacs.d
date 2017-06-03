;;; eshell-git-prompt.el --- Some Eshell prompt for Git users  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; URL: https://github.com/xuchunyang/eshell-git-prompt
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5") (dash "2.11.0"))
;; Keywords: eshell git
;; Version: 0.1.1
;; Created: 09/11/2015

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides some themes of Emacs Shell (Eshell) prompt.
;;
;; Usage:
;; In Eshell, type ~use-theme~ to list and preview available themes, then
;; type ~use-theme name~ to choose a theme.
;;
;; You can also choose a theme in your init file by using
;; ~eshell-git-prompt-use-theme~, then Eshell will use theme at the
;; startup. For example, put the following in you init file
;;
;; TODO
;; - [ ] For `eshell-prompt-regexp' hack, replace '$' with '' ('\x06')
;; - [ ] Make it easier to make new theme (that is, improve API)
;; - [ ] Test with recent version Emacs and in text-base Emacs
;;
;; Note
;; 1 You must kill all Eshell buffers and re-enter Eshell to make your new
;;   prompt take effect.
;; 2 You must set `eshell-prompt-regexp' to FULLY match your Eshell prompt,
;;   sometimes it is impossible, especially when you don't want use any special
;;   characters (e.g., '$') to state the end of your Eshell prompt
;; 3 If you set `eshell-prompt-function' or `eshell-prompt-regexp' incorrectly,
;;   Eshell may crashes, if it happens, you can M-x `eshell-git-prompt-use-theme'
;;   to revert to the default Eshell prompt, then read Note #1.

;;; Code:

(require 'cl-lib)
(require 'dash)

(declare-function eshell/pwd "em-dirs" (&rest args))
(declare-function eshell-printn "esh-io" (object))

(eval-when-compile
  (defvar eshell-last-command-status)
  (defvar eshell-prompt-function)
  (defvar eshell-prompt-regexp))


;;; * Customization
(defcustom eshell-git-prompt-themes
  '((robbyrussell
     eshell-git-prompt-robbyrussell
     eshell-git-prompt-robbyrussell-regexp)
    (git-radar
     eshell-git-prompt-git-radar
     eshell-git-prompt-git-radar-regexp)
    (powerline
     eshell-git-prompt-powerline
     eshell-git-prompt-powerline-regexp)
    ;; Only a single $
    (simple
     eshell-git-prompt-simple
     eshell-git-prompt-simple-regexp)
    (default
      eshell-git-prompt-default-func
      eshell-git-prompt-default-regexp))
  "A list of all available themes.
You can add your own theme to this list, then run
`eshell-git-prompt-use-theme' to use it."
  :group 'eshell-prompt
  :type '(repeat (list symbol symbol symbol)))

(defface eshell-git-prompt-powerline-dir-face
  '((t :background "steel blue"))
  "Face for directory name in eshell git prompt theme `powerline`"
  :group 'eshell-faces)

(defface eshell-git-prompt-powerline-clean-face
  '((t :background "forest green"))
  "Face for git branch (clean) in eshell git prompt theme `powerline`"
  :group 'eshell-faces)

(defface eshell-git-prompt-powerline-not-clean-face
  '((t :background "indian red"))
  "Face for git branch (not clean) in eshell git prompt theme `powerline`"
  :group 'eshell-faces)


;;; * Internal

(defmacro with-face (str &rest properties)
  "Add face PROPERTIES to STR."
  (declare (indent 1))
  `(propertize ,str 'face (list ,@properties)))

(defmacro eshell-git-prompt-str-append (str suffix)
  "Append STR with SUFFIX and set the value of symbol STR to the result."
  (if (symbolp str)
      (list 'setq str (list 'concat str suffix))
    (list 'cl-callf 'concat str suffix)))

(cl-defun eshell-git-prompt--git-root-dir
    (&optional (directory default-directory))
  "Return Git root directory name if exist, otherwise, return nil."
  (let ((root (locate-dominating-file directory ".git")))
    (and root (file-name-as-directory root))))

(cl-defun eshell-git-prompt--shorten-directory-name
    (&optional (directory default-directory))
  "Return only current directory name (without ending slash).
DIRECTORY must end with slash.

For example:

  \"~/foo/bar\" => \"bar\"
  \"~\" => \"~\"
  \"/\" => \"/\""
  (let ((dir (abbreviate-file-name directory)))
    (if (> (length dir) 1)
        (file-name-nondirectory (substring dir 0 -1))
      dir)))

(defun eshell-git-prompt--slash-str (str)
  "Make sure STR is ended with one slash, return it."
  (if (string-suffix-p "/" str)
      str
    (concat str "/")))

(defun eshell-git-prompt-last-command-status ()
  "Return Eshell last command execution status.
When Eshell just launches, `eshell-last-command-status' is not defined yet,
return 0 (i.e., success)."
  (if (not (boundp 'eshell-last-command-status))
      0
    eshell-last-command-status))

(defun eshell-git-prompt-exit-success-p ()
  "Replacement of `eshell-exit-success-p'.
Should return nil only if a external command fails, otherwise return non-nil.

It is unable to distinguish if a elisp command fails in practice, for example,
`eshell/cd' return nil whether it successes or not."
  (= (eshell-git-prompt-last-command-status) 0))

(defconst eshell-git-prompt---git-global-arguments
  '("--no-pager" "--literal-pathspecs" "-c" "core.preloadindex=true")
  "Global git arguments.")

(defun eshell-git-prompt--process-git-arguments (args)
  "Prepare ARGS for a function that invokes Git."
  (setq args (-flatten args))
  (append eshell-git-prompt---git-global-arguments args))

(defun eshell-git-prompt--git-insert (&rest args)
  "Execute Git with ARGS, inserting its output at point."
  (setq args (eshell-git-prompt--process-git-arguments args))
  (apply #'process-file "git" nil (list t nil) nil args))

(defun eshell-git-prompt--git-string (&rest args)
  "Execute Git with ARGS, returning the first line of its output.
If there is no output return nil.  If the output begins with a
newline return an empty string."
  (with-temp-buffer
    (apply #'eshell-git-prompt--git-insert args)
    (unless (bobp)
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun eshell-git-prompt--git-lines (&rest args)
  "Execute Git with ARGS, returning its output as a list of lines.
Empty lines anywhere in the output are omitted."
  (with-temp-buffer
    (apply #'eshell-git-prompt--git-insert args)
    (split-string (buffer-string) "\n" t)))

(defun eshell-git-prompt--collect-status ()
  "Return working directory status as a plist.
If working directory is clean, return nil."
  (let ((untracked 0)                   ; next git-add, then git-commit
        (modified 0)                    ; next git-commit
        (modified-updated 0)            ; next git-commit
        (new-added 0)                   ; next git-commit
        (deleted 0)                     ; next git-rm, then git-commit
        (deleted-updated 0)             ; next git-commit
        (renamed-updated 0)             ; next git-commit
        )
    (-when-let (status-items (eshell-git-prompt--git-lines "status" "--porcelain"))
      (--each status-items
        (pcase (substring it 0 2)
          ("??" (cl-incf untracked))
          ("MM" (progn (cl-incf modified)
                       (cl-incf modified-updated)))
          (" M" (cl-incf modified))
          ("M " (cl-incf modified-updated))
          ("A " (cl-incf new-added))
          (" D" (cl-incf deleted))
          ("D " (cl-incf deleted-updated))
          ("R " (cl-incf renamed-updated))))
      (list :untracked untracked
            :modified modified
            :modified-updated  modified-updated
            :new-added new-added
            :deleted deleted
            :deleted-updated deleted-updated
            :renamed-updated renamed-updated))))

(defun eshell-git-prompt--branch-name ()
  "Return current branch name."
  (eshell-git-prompt--git-string "symbolic-ref" "HEAD" "--short"))

(defvar eshell-git-prompt-branch-name nil)

(defun eshell-git-prompt--commit-short-sha ()
  (eshell-git-prompt--git-string "rev-parse" "--short" "HEAD"))

(defun  eshell-git-prompt--readable-branch-name ()
  (-if-let (branch-name eshell-git-prompt-branch-name)
      branch-name
    (concat "detached@" (eshell-git-prompt--commit-short-sha))))

(defun eshell-git-prompt--remote-branch-name ()
  (eshell-git-prompt--git-string "for-each-ref" "--format=%(upstream:short)"
                                 (format "refs/heads/%s"
                                         eshell-git-prompt-branch-name)))

(defvar eshell-git-prompt-remote-branch-name nil)

(defun eshell-git-prompt--commits-ahead-of-remote ()
  (-if-let (remote-branch-name eshell-git-prompt-remote-branch-name)
      (string-to-number
       (eshell-git-prompt--git-string "rev-list" "--right-only" "--count"
                                      (format "%s...HEAD" remote-branch-name)))
    0))

(defun eshell-git-prompt--commits-behind-of-remote ()
  (-if-let (remote-branch-name eshell-git-prompt-remote-branch-name)
      (string-to-number
       (eshell-git-prompt--git-string "rev-list" "--left-only" "--count"
                                      (format "%s...HEAD" remote-branch-name)))
    0))


;;; * Themes

(defun eshell-git-prompt-simple ()
  (if (= (user-uid) 0) "# " "$ "))

(defconst eshell-git-prompt-simple-regexp "^[#$] ")

;; the Eshell default prompt
(defun eshell-git-prompt-default-func ()
  (concat (abbreviate-file-name (eshell/pwd))
          (if (= (user-uid) 0) " # " " $ ")))

(defconst eshell-git-prompt-default-regexp "^[^#$\n]* [#$] ")

;; oh-my-zsh's robbyrussell theme
;; see https://github.com/robbyrussell/oh-my-zsh/wiki/Themes#robbyrussell
(defun eshell-git-prompt-robbyrussell ()
  "Eshell Git prompt with oh-my-zsh's robbyrussell theme.

It looks like:

➜ eshell-git-prompt git:(master) ✗ git status "
  ;; Prompt components
  (let (beg dir git-branch git-dirty end)
    ;; Beg: start symbol
    (setq beg
          (with-face "➜"
            :foreground (if (eshell-git-prompt-exit-success-p)
                            "green" "red")))

    ;; Dir: current working directory
    (setq dir (with-face (eshell-git-prompt--shorten-directory-name)
                :foreground "cyan"))

    ;; Git: branch/detached head, dirty status
    (when (eshell-git-prompt--git-root-dir)
      (setq eshell-git-prompt-branch-name (eshell-git-prompt--branch-name))

      (setq git-branch
            (concat
             (with-face "git:(" :foreground "blue")
             (with-face (eshell-git-prompt--readable-branch-name) :foreground "red")
             (with-face ")" :foreground "blue")))

      (setq git-dirty
            (when (eshell-git-prompt--collect-status)
              (with-face "✗" :foreground "yellow"))))

    ;; End: To make it possible to let `eshell-prompt-regexp' to match the full prompt
    (setq end (propertize "$" 'invisible t))

    ;; Build prompt
    (concat (mapconcat #'identity (-non-nil (list beg dir git-branch git-dirty)) " ")
            end
            " ")))

(defconst eshell-git-prompt-robbyrussell-regexp "^[^$\n]*\\\$ ")

;; git-radar
;; see https://github.com/michaeldfallen/git-radar

(defun eshell-git-prompt-git-radar ()
  "Eshell Git prompt inspired by git-radar."
  (concat
   (with-face "➜"
     :foreground (if (eshell-git-prompt-exit-success-p)
                     "green" "red"))
   " "
   (with-face (eshell-git-prompt--shorten-directory-name)
     :foreground "cyan")
   ;; Yo, we are in a Git repo, display some information about it
   (when (eshell-git-prompt--git-root-dir)
     (setq eshell-git-prompt-branch-name
           (eshell-git-prompt--branch-name)
           eshell-git-prompt-remote-branch-name
           (eshell-git-prompt--remote-branch-name))
     (concat
      " "
      (with-face "git:(" :foreground "dark gray")

      ;; Branch name
      (with-face (eshell-git-prompt--readable-branch-name)
        :foreground "gray")

      ;; Local commits
      (when eshell-git-prompt-remote-branch-name
        (let ((local-behind (eshell-git-prompt--commits-behind-of-remote))
              (local-ahead (eshell-git-prompt--commits-ahead-of-remote)))
          (cond ((and (> local-ahead 0) (> local-behind 0))
                 (concat " "
                         (number-to-string local-behind)
                         (with-face "⇵" :foreground "yellow")
                         (number-to-string local-ahead)))
                ((> local-behind 0)
                 (concat " "
                         (number-to-string local-behind)
                         (with-face "↓" :foreground "red")))
                ((> local-ahead 0)
                 (concat " "
                         (number-to-string local-ahead)
                         (with-face "↑" :foreground "LimeGreen"))))))

      (with-face ")" :foreground "dark gray")

      ;; File status
      (-when-let (git-status (eshell-git-prompt--collect-status))
        (-let [(&plist :untracked untracked
                       :new-added new-added
                       :modified-updated modified-updated
                       :modified modified)
               git-status]
          (concat
           ;; Updated to index changes
           (let (group1)
             (setq group1
                   (concat
                    (when (> new-added 0)
                      (concat
                       (number-to-string new-added)
                       (with-face "A" :foreground "green")))
                    (when (> modified-updated 0)
                      (concat
                       (number-to-string modified-updated)
                       (with-face "M" :foreground "green")))))
             (when (> (length group1) 0)
               (concat " " group1)))
           ;; Modified but not updated
           (when (> modified 0)
             (concat " " (number-to-string modified)
                     (with-face "M" :foreground "red")))
           ;; Untracked file
           (when (> untracked 0)
             (concat " " (number-to-string untracked)
                     (with-face "A" :foreground "white"))))))))
   ;; To make it possible to let `eshell-prompt-regexp' to match the full prompt
   (propertize "$" 'invisible t) " "))

(defconst eshell-git-prompt-git-radar-regexp "^[^$\n]*\\\$ ")


;; Powerline

(defun eshell-git-prompt-powerline ()
  (let ((segment-separator "\xe0b0")
        (plusminus         "\x00b1")
        (branch            "\xe0a0")
        (detached          "\x27a6")
        (cross             "\x2718")
        dir git git-bg)
    (setq dir
          (propertize
           (concat
            " "
            (unless (eshell-git-prompt-exit-success-p)
              (concat cross " "))
            (abbreviate-file-name (eshell/pwd))
            " ")
           'face 'eshell-git-prompt-powerline-dir-face))
    (setq git
          (when (eshell-git-prompt--git-root-dir)
            (setq git-face
                  (if (eshell-git-prompt--collect-status)
                      'eshell-git-prompt-powerline-not-clean-face
                    'eshell-git-prompt-powerline-clean-face))
            (setq eshell-git-prompt-branch-name (eshell-git-prompt--branch-name))
            (propertize
                (concat " "
                        (-if-let (branch-name eshell-git-prompt-branch-name)
                            (concat branch " " branch-name)
                          (concat detached " "(eshell-git-prompt--commit-short-sha)))
                        " ")
              'face git-face)))
    (concat
     (if git
         (concat dir
                 (with-face segment-separator
                   :foreground (face-background 'eshell-git-prompt-powerline-dir-face)
                   :background (face-background git-face))
                 git
                 (with-face segment-separator
                   :foreground (face-background git-face)))
       (concat dir
               (with-face segment-separator
                 :foreground (face-background 'eshell-git-prompt-powerline-dir-face))))
     (propertize "$" 'invisible t) " ")))

(defconst eshell-git-prompt-powerline-regexp "^[^$\n]*\\\$ ")

(defvar eshell-git-prompt-current-theme nil)

;;;###autoload
(defun eshell-git-prompt-use-theme (theme)
  "Pick up a Eshell prompt theme from `eshell-git-prompt-themes' to use."
  (interactive
   (let ((theme
          (completing-read "Use theme: "
                           (--map (symbol-name (car it))
                                  eshell-git-prompt-themes)
                           nil t)))
     (list (intern theme))))
  (when (stringp theme)
    (setq theme (intern theme)))
  (-if-let (func-regexp (assoc-default theme eshell-git-prompt-themes))
      (progn
        (setq eshell-prompt-function (symbol-function (car func-regexp))
              eshell-prompt-regexp (symbol-value (cadr func-regexp)))
        (setq eshell-git-prompt-current-theme theme)
        (when (called-interactively-p 'interactive)
          (message
           "Now kill all Eshell buffers and re-enter Eshell to use %s theme"
           (symbol-name theme))))
    (error "Theme \"%s\" is not available" theme)))

;; TODO: Support the --help option
;; TODO: Support completing (via pcomplete)
;;;###autoload
(defun eshell/use-theme (&optional theme)
  "List all available themes and pick one from Eshell."
  (if (null theme)
      (progn
        (eshell-printn "")
        (eshell-printn (make-string 60 ?-))
        (eshell-printn (format "%-20s%s" "Name" "Preview"))
        (eshell-printn (make-string 60 ?-))
        (eshell-printn
         (mapconcat (lambda (theme)
                      (format "%-20s%s"
                              (symbol-name (car theme))
                              (funcall (cadr theme))))
                    eshell-git-prompt-themes "\n"))
        (eshell-printn (make-string 60 ?-))
        (eshell-printn "")
        (eshell-printn "Type 'use-theme theme-name' to use a theme."))
    (when (numberp theme)
      (setq theme (number-to-string theme)))
    (setq theme (intern theme))
    (-if-let (func-regexp (assoc-default theme eshell-git-prompt-themes))
        (progn
          (setq eshell-prompt-function (symbol-function (car func-regexp))
                eshell-prompt-regexp (symbol-value (cadr func-regexp)))
          (setq eshell-git-prompt-current-theme theme)
          (eshell-printn ""))
      (error
       "Theme \"%s\" is not available.
Run this command again without argument to view all available themes.

usage: use-theme: (&optional theme)"
       (symbol-name theme))))
  nil)

(provide 'eshell-git-prompt)

;; Local Variables:
;; coding: utf-8
;; End:
;;; eshell-git-prompt.el ends here
