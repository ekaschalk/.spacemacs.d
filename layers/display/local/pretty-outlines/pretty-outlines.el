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
  (-let [display-table (if buffer-display-table
                           buffer-display-table
                         (make-display-table))]
    (unless buffer-display-table
      (setq buffer-display-table display-table))

    (->> pretty-outlines-ellipsis
       (--map (make-glyph-code it 'font-lock-keyword-face))
       vconcat
       (set-display-table-slot display-table 4))))

;;; Outline-bullets

;;;###autoload
(defun pretty-outlines--add-font-locks (font-lock-alist)
  "Put text property for FONT-LOCK-ALIST for var-width replacements."
  (font-lock-add-keywords
   nil
   (-map (-lambda ((rgx outline-str))
           `(,rgx (0 (prog1 nil
                       (put-text-property (match-beginning 1) (match-end 1)
                                          'display ,outline-str)))))
         font-lock-alist)))

;;;###autoload
(defun pretty-outlines--bullets-rgx-at-level (level)
  "Calculate regex or outline-bullets at LEVEL."
  (concat "\\(^"
          (->> level
             1+
             outshine-calc-outline-string-at-level
             s-trim-right
             (s-replace "*" "\\*"))
          "\\) "))

;;;###autoload
(defun pretty-outlines--propertize-bullet (level bullet)
  "Add LEVEL-dependent face to BULLET."
  (with-face bullet
             (pcase level
               (0 '(:inherit outline-1 :underline nil))
               (1 '(:inherit outline-2 :underline nil))
               (2 '(:inherit outline-3 :underline nil))
               (3 '(:inherit outline-4 :underline nil))
               (_ nil))))

;;;###autoload
(defun pretty-outlines--bullets-cycle ()
  "Cycle through `pretty-outlines-bullets-bullet-list'"
  (-let [max-outline-depth 8]
    (->> pretty-outlines-bullets-bullet-list -cycle (-take max-outline-depth))))

;;;###autoload
(defun pretty-outlines--format-bullet (level bullet)
  "Propertize and perform all other styling for BULLET at LEVEL."
  (concat (s-repeat level " ")
          (pretty-outlines--propertize-bullet level bullet)))

;;;###autoload
(defun pretty-outlines-add-bullets ()
  "Use with `add-hook' to enable pretty-outlines-bullets-bullet-list for mode."
  (->>
   (pretty-outlines--bullets-cycle)
   (-map-indexed (-lambda (level bullet)
                   (list (pretty-outlines--bullets-rgx-at-level level)
                         (pretty-outlines--format-bullet        level bullet))))
   pretty-outlines--add-font-locks))
