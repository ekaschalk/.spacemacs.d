(require 'dash)
(require 'outshine)
(require 's)

(provide 'pretty-outlines)

;;; Config

(defvar pretty-outline-bullets-bullet-list '("" "" "" "")
  "An implemention of `org-bullets-bullet-list' for outlines")

(defvar pretty-outline-ellipsis ""
  "An implementation of `org-ellipsis' for outlines")

;;; Outline-ellipsis

;;;###autoload
(defun pretty-outline-set-display-table ()
  (let ((display-table (if buffer-display-table
                           buffer-display-table
                         (make-display-table))))
    (unless buffer-display-table
      (setq buffer-display-table display-table))
    (set-display-table-slot
     display-table 4
     (vconcat (mapcar (lambda (c)
                        (make-glyph-code c 'font-lock-keyword-face))
                      pretty-outline-ellipsis)))))

;;; Outline-bullets

;;;###autoload
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

;;;###autoload
(defun pretty-outline--bullets-rgx-at-level (LEVEL)
  "Calculate regex or outline-bullets at LEVEL."
  (concat "\\(^"
          (->> LEVEL
             outshine-calc-outline-string-at-level
             s-trim-right
             (s-replace "*" "\\*"))
          "\\) "))

;;;###autoload
(defun pretty-outline--propertize-bullet (LEVEL BULLET)
  "Add LEVEL-dependent face to BULLET."
  (with-face BULLET
             (pcase LEVEL
               (0 '(:inherit outline-1 :underline nil))
               (1 '(:inherit outline-2 :underline nil))
               (2 '(:inherit outline-3 :underline nil))
               (3 '(:inherit outline-4 :underline nil))
               (_ nil))))

;;;###autoload
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
