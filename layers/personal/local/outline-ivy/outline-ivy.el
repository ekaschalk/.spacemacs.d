;;; outline-ivy.el --- Jumping to rich outlines with ivy -*- lexical-binding: t; -*-

(require 'ivy)
(require 'outshine)
(require 'dash)
(require 'dash-functional)
(require 's)

(provide 'outline-ivy)

;; FIXME an outline not at the BOL is not an outline, but it breaks oi-jump

;;; Config

(defvar oi-height 20
  "Number of outlines to display, overrides ivy-height.")

(defvar oi-prepend-header-char "."
  "Prepend this char 'level' times to each header prompted.")

(defface oi-match-face
  '((t :height 1.10 :foreground "light gray"))
  "Match face for ivy outline prompt.")

(defface oi-face-1
  '((t :foreground "#DFAF8F" :height 1.25 :underline t :weight ultra-bold))
  "Ivy outline face for level 1")

(defface oi-face-2
  '((t :foreground "#8D6B94" :height 1.15 :weight semi-bold))
  "Ivy outline face for level 2")

(defface oi-face-3
  '((t :foreground "#268bd2"))
  "Ivy outline face for level 3")

;;; Utils

(defun oi-rgx ()
  "Regex to match outlines with first group as its text."
  (cadar outshine-imenu-preliminary-generic-expression))

(defun oi-format-name (str level)
  "Format STR at LEVEL for ivy."
  (concat (s-repeat (1- level) oi-prepend-header-char)
          str))

(defun oi-format-name-pretty (str parents level)
  "Prepend invisible PARENTS to propertized STR at LEVEL."
  (concat (propertize
           (concat (when level (number-to-string level))
                   (apply 'concat parents))
           'invisible t)
          (propertize (oi-format-name str level)
                      'face (pcase level
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
    ;; Not quite sure where the initial nil is coming from
    ;; Was --unfold changed? Anyway this cdr fixes the regression of
    ;; a nil being introduced at the head of the list of fontified outlines
    (cdr
     (-snoc (--unfold
             (when (search-forward-regexp (oi-rgx) nil t)
               (cons it (oi--collect-outline)))
             nil)
            (oi--collect-outline)))))

;;; Outline Jump

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

;;;###autoload
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
