;; A fancy, functional ivy prompt for outlines
;; Faces and formatting only provided for up to level 3 outlines
;; See this blog post for details
;; http://ekaschalk.github.io/post/outline-ivy/

(require 'dash)
(require 'ivy)
(require 'outshine)

;;; Config

(defvar oi-height 20
  "Number of outlines to display, overrides ivy-height.")

(defface oi-match-face
  '((t :height 1.10 :foreground "light gray"))
  "Match face for ivy outline prompt.")

(defface oi-face-1
  '((t :foreground "#268bd2" :height 1.25 :underline t :weight ultra-bold))
  "Ivy outline face for level 1")

(defface oi-face-2
  '((t :foreground "#2aa198" :height 1.1 :weight semi-bold))
  "Ivy outline face for level 2")

(defface oi-face-3
  '((t :foreground "steel blue"))
  "Ivy outline face for level 3")

;;; Utils

(defun oi-rgx ()
  "Regex to match outlines with first group as its text."
  (cadar outshine-imenu-preliminary-generic-expression))

(defun oi-format-name (STR LEVEL)
  "Format STR at LEVEL for ivy."
  (pcase LEVEL
    (2 (format " %s" STR))
    (3 (format "  %s" STR))
    (_ STR)))

(defun oi-format-name-pretty (STR PARENTS LEVEL)
  "Prepend invisible PARENTS to propertized STR at LEVEL."
  (concat (propertize
           (concat (when LEVEL (number-to-string LEVEL))
                   (apply 'concat PARENTS))
           'invisible t)
          (propertize (oi-format-name STR LEVEL)
                      'face (pcase LEVEL
                              (1 'oi-face-1)
                              (2 'oi-face-2)
                              (3 'oi-face-3)))))

(defun oi--collect-outline ()
  "Collect outline-ivy formatted outline string and marker for line at point."
  (save-excursion
    (beginning-of-line)
    (-let* ((level (outshine-calc-outline-level))
            (parents (when level
                       (--map (plist-get oi--parents-plist it)
                             (number-sequence 1 (- level 1)))))
            (str (match-string-no-properties 1))
            (name (oi-format-name-pretty str parents level)))
      (when level
        (setq oi--parents-plist (plist-put oi--parents-plist level str)))
      (->> (point-marker)
         (cons name)
         (when level)))))

(defun oi-collect-outlines ()
  "Collect fontified outline strings and their markers for ivy-read."
  (setq oi--parents-plist nil)
  (save-excursion
    (goto-char (point-min))
    (-snoc (--unfold
            (when (search-forward-regexp (oi-rgx) nil t)
              (cons it (oi--collect-outline)))
            nil)
           (oi--collect-outline))))

;;; API

(defun oi--preselect ()
  "Get parent outline at point for ivy :preselect."
  (save-excursion
    (unless (outline-on-heading-p t)
      (outline-previous-heading))
    (search-forward-regexp (oi-rgx) nil t)
    (beginning-of-line)
    (-> (match-string-no-properties 1)
       (oi-format-name (outshine-calc-outline-level)))))

(defun oi--remap-ivy-match-face ()
  "Overwrite ivy-current-match face in outline-ivy prompt."
  (set (make-local-variable 'face-remapping-alist)
       '((ivy-current-match oi-match-face))))

(defun oi-jump ()
  "Prompt fontified, hierarchal outlines for jump."
  (interactive)
  (let ((ivy-height oi-height))
    (ivy-read "Outline " (oi-collect-outlines)
              :preselect (oi--preselect)
              :update-fn 'oi--remap-ivy-match-face
              :action (-lambda ((_ . marker))
                        (with-ivy-window
                          (-> marker marker-position goto-char)
                          (recenter 2))))))

(provide 'outline-ivy)
