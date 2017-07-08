;;; pretty-eshell.el --- Eshell customization utilities

;; Copyright (C) 2017-2017 Eric Kaschalk

;; Author: Eric Kaschalk <ekaschalk@gmail.com>
;; Maintainer: Eric Kaschalk <ekaschalk@gmail.com>
;; Created: July 7th 2017
;; Keywords: shell,display

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

(defvar eshell-funcs nil
  "List of `esh-section' to enable.")

(defvar esh-sep "  "
  "String delimits each `esh-section'")

(defvar esh-section-delim " "
  "String delimits icons and their text.")

(defvar esh-header "\n "
  "Initial string composing the eshell prompt.")

(defvar esh-prompt-string " "
  "Prompt string, must match builtin `eshell-prompt-regexp'")

;;; Core

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME
         (lambda () (when ,FORM
                 (-> ,ICON
                    (concat esh-section-delim ,FORM)
                    (with-face ,@PROPS))))))

(defun esh--acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (concat acc esh-sep it))
    acc))

(defun esh-prompt-func ()
  "Replacement for `eshell-prompt-function'"
  (concat esh-header
          (-reduce-from 'esh--acc "" eshell-funcs)
          "\n"
          esh-prompt-string))

(setq eshell-prompt-function 'esh-prompt-func)

;;; Sections

(setq esh-prompt-num 0)
(add-hook 'eshell-exit-hook
          (lambda () (setq esh-prompt-num 0)))
(advice-add 'eshell-send-input :before
            (lambda (&rest args) (setq esh-prompt-num (+ 1 esh--prompt-num))))


(provide 'pretty-eshell)
;;; pretty-eshell.el ends here
