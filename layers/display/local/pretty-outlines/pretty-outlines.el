;;; pretty-outlines.el --- Outline mode display updates

;; Copyright (C) 2017-2017 Eric Kaschalk

;; Author: Eric Kaschalk <ekaschalk@gmail.com>
;; Maintainer: Eric Kaschalk <ekaschalk@gmail.com>
;; Created: July 7th 2017
;; Keywords: outlines,display,org-mode

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

(require 'outshine)

(defvar pretty-outline-bullets-bullet-list '("" "" "" "")
  "An implemention of `org-bullets-bullet-list' for outlines")

(defvar pretty-outline-ellipsis ?
  "An implementation of `org-ellipsis' for outlines")

;;; Outline-ellipsis

(setq pretty-outline-display-table (make-display-table))
(set-display-table-slot pretty-outline-display-table 'selective-display
                        (vector (make-glyph-code pretty-outline-ellipsis
                                                 'escape-glyph)))
(defun pretty-outline-set-display-table ()
  (setf buffer-display-table pretty-outline-display-table))

;;; Outline-bullets

(defun pretty-outline--add-font-locks (FONT-LOCK-ALIST)
  "Put text property for FONT-LOCK-ALIST for var-width replacements."
  (font-lock-add-keywords
   nil (--map (-let (((rgx uni-point) it))
             `(,rgx (0 (progn
                         (put-text-property
                          (match-beginning 1) (match-end 1)
                          'display
                          ,uni-point)
                         nil))))
           FONT-LOCK-ALIST)))

(defun pretty-outline--bullets-rgx-at-level (LEVEL)
  "Calculate regex or outline-bullets at LEVEL."
  (concat "\\(^"
          (-> LEVEL
             outshine-calc-outline-string-at-level
             s-trim-right)
          "\\) "))

(defun pretty-outline--propertize-bullet (LEVEL BULLET)
  "Add LEVEL-dependent face to BULLET."
  (with-face BULLET
             (pcase LEVEL
               (0 '(:inherit outline-1 :underline nil))
               (1 '(:inherit outline-2 :underline nil))
               (2 '(:inherit outline-3 :underline nil))
               (3 '(:inherit outline-4 :underline nil))
               (_ nil))))

(defun pretty-outline-add-bullets ()
  "Use with `add-hook' to enable pretty-outline-bullets-bullet-list for mode."
  (pretty-outline--add-font-locks
   (--map-indexed
    (list
     (pretty-outline--bullets-rgx-at-level (+ 1 it-index))
     (concat
      (s-repeat it-index " ")
      (pretty-outline--propertize-bullet it-index it)))
    (-take 8 (-cycle pretty-outline-bullets-bullet-list)))))


(provide 'pretty-outlines)
;;; pretty-outlines.el ends here
