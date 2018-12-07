;;; pretty-outlines.el --- Org-bullets styling for outline-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Similar to `org-bullets-bullet-list' and `org-ellipsis' for
;; `outline-minor-mode', generalized to work for all major-modes that define an
;; outline syntax when the hook `pretty-outlines-add-bullets' is added.

;;; Code:
;;;; Requires

(require 'dash)
(require 'dash-functional)
(require 's)

(require 'outshine)
(require 'prettify-utils)

;;;; Config

(defvar pretty-outlines-bullets-bullet-list '(#x25c9 #x25cb #x2738 #x273f)
  "An implemention of `org-bullets-bullet-list' for outlines.

The headers wrap around for further levels than elements in this
list, just like for org-bullets.

Defaults to org-bullet's defaults. I use all-the-icons
border-boxed numbers, as used in eg. all-the-icons version of
spaceline.")

(defvar pretty-outlines-ellipsis "~"
  "An implementation of `org-ellipsis' for outlines.")

(defvar pretty-outlines--max-outline-depth 8
  "The max outline depth to apply `pretty-outlines' over.")

(defvar pretty-outlines--common-face-props '(:underline nil)
  "Common face properties/overwrites to apply to each bullet.

These properties are applied to only the *bullet* part of the
outline. The bullet will inherit the outline-face appropriately
and rarely would you modify this. I add :underline nil so my
underlining starts at title text.")

;;;; Outline-ellipsis

;;;###autoload
(defun pretty-outlines-set-display-table ()
  (-let [display-table (if buffer-display-table
                           buffer-display-table
                         (make-display-table))]
    (unless buffer-display-table
      (setq buffer-display-table display-table))

    (->> pretty-outlines-ellipsis
       (--map (make-glyph-code it 'font-lock-keyword-face))
       vconcat
       (set-display-table-slot display-table 4))))

;;;; Outline-bullets

;;;###autoload
(defun pretty-outlines--bullets-rgx-at-level (level)
  "Modify outline regex with group matches at LEVEL."
  (let* ((outline-str
          (->> level outshine-calc-outline-string-at-level s-trim-right))
         (outline-rx
          `(: (group bol
                     ,outline-str)
              " ")))
    (rx-to-string outline-rx 'no-group)))

;;;###autoload
(defun pretty-outlines--level->face-props (level)
  "Get face properties for LEVEL."
  `(:inherit ,(intern (s-concat "outline-"
                                (number-to-string level)))
             ,@pretty-outlines--common-face-props))

;;;###autoload
(defun pretty-outlines--propertize-bullet (level bullet)
  "Add LEVEL-dependent face to BULLET."
  (->> level
     pretty-outlines--level->face-props
     (propertize bullet 'face)))

;;;###autoload
(defun pretty-outlines--glue-bullet (level bullet)
  "Impute composition rules gluing leading spaces LEVEL times to BULLET."
  (-> level 1-
     (s-repeat " ")
     (s-concat (char-to-string bullet))
     prettify-utils-string))

;;;###autoload
(defun pretty-outlines--build-keyword (rgx composition face-props)
  "Build font-lock-keyword to compose RGX into COMPOSITION with FACE-PROPS."
  `(,rgx (0 (prog1 nil
              (compose-region (match-beginning 1) (match-end 1)
                              ',composition)
              (put-text-property (match-beginning 1) (match-end 1) 'face
                                 ',face-props)))))

;;;###autoload
(defun pretty-outlines--build-rgx-composition-face-alist (codepoints)
  "Given CODEPOINTS construct keyword alist."
  (-map-indexed (-lambda (level bullet)
                  (incf level)  ; Outlines enumerated 1..n
                  (list (pretty-outlines--bullets-rgx-at-level level)
                        (pretty-outlines--glue-bullet          level bullet)
                        (pretty-outlines--level->face-props    level)))
                codepoints))

;;;###autoload
(defun pretty-outlines-add-bullets ()
  "Add as hook to any major mode with outline syntax to enable pretty-outlines."
  (->> pretty-outlines-bullets-bullet-list
     -cycle
     (-take pretty-outlines--max-outline-depth)
     pretty-outlines--build-rgx-composition-face-alist
     (-map (-applify #'pretty-outlines--build-keyword))
     (font-lock-add-keywords nil)))

(provide 'pretty-outlines)
