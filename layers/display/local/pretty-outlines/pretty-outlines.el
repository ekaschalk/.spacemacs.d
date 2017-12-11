(require 'outshine)
(require 'macros)

(provide 'pretty-outlines)

;;; Config

(defvar pretty-outlines-bullets-bullet-list '("1" "2" "3" "4")
  "An implemention of `org-bullets-bullet-list' for outlines.")

(defvar pretty-outlines-ellipsis "~"
  "An implementation of `org-ellipsis' for outlines.")

;;; Outline-ellipsis

;;;###autoload
(defun pretty-outlines-set-display-table ()
  (-let [display-table
         (if buffer-display-table buffer-display-table (make-display-table))]
    (unless buffer-display-table
      (setq buffer-display-table display-table))

    (->> pretty-outlines-ellipsis
       (-map (lambda (c) (make-glyph-code c 'font-lock-keyword-face)))
       vconcat
       (set-display-table-slot display-table 4))))

;;; Outline-bullets

;;;###autoload
(defun pretty-outlines--add-font-locks (font-lock-alist)
  "Put text property for FONT-LOCK-ALIST for var-width replacements."
  (font-lock-add-keywords
   nil
   (-map
    (-lambda ((rgx uni-point))
      `(,rgx (0 (prog1
                    nil
                  (let ((start (match-beginning 1))
                        (end (match-end 1)))
                    (put-text-property start end 'display ,uni-point))))))
    font-lock-alist)))

;;;###autoload
(defun pretty-outlines--bullets-rgx-at-level (level)
  "Calculate regex or outline-bullets at LEVEL."
  (concat "\\(^"
          (->> level
             outshine-calc-outline-string-at-level
             s-trim-right
             (s-replace "*" "\\*"))
          "\\) "))

;;;###autoload
(defun pretty-outlines--propertize-bullet (LEVEL BULLET)
  "Add LEVEL-dependent face to BULLET."
  (with-face BULLET
             (pcase LEVEL
               (0 '(:inherit outline-1 :underline nil))
               (1 '(:inherit outline-2 :underline nil))
               (2 '(:inherit outline-3 :underline nil))
               (3 '(:inherit outline-4 :underline nil))
               (_ nil))))

;;;###autoload
(defun pretty-outlines-add-bullets ()
  "Use with `add-hook' to enable pretty-outlines-bullets-bullet-list for mode."
  (pretty-outlines--add-font-locks
   (--map-indexed
    (list
     (pretty-outlines--bullets-rgx-at-level (+ 1 it-index))
     (concat
      (s-repeat it-index " ")
      (pretty-outlines--propertize-bullet it-index it)))
    (-take 8 (-cycle pretty-outlines-bullets-bullet-list)))))
