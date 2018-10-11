;;; -*- lexical-binding: t -*-

;; Note: I'm not using many of the symbols I've defined
;; in the past like ones for "in", "for", and so on.
;; This is because I got annoyed with indentation and that
;; they aren't proportional.
;; I love ligatures but only these remain in my workflow.
;; It is a mammoth effort to fix both the above issues
;; which I have explorered various times but do not have
;; a comprehensive solution.

(require 'prettify-utils)
(require 'macros)

(provide 'pretty-code)

(global-prettify-symbols-mode 1)

;;; Config

(defvar pretty-code-choices
  (-flatten
   (prettify-utils-generate
    ;; Functional
    (:lambda      "λ")
    (:def         "ƒ")
    (:composition "∘")

    ;; Types
    (:null        "∅")
    (:true        "𝕋") (:false       "𝔽")
    (:int         "ℤ") (:float       "ℝ")
    (:str         "𝕊")
    (:bool        "𝔹")

    ;; Flow
    (:not         "￢")
    (:in          "∈") (:not-in      "∉")
    (:and         "∧") (:or          "∨")
    (:for         "∀")
    (:some        "∃")
    (:return     "⟼") (:yield      "⟻")

    ;; Other
    (:tuple       "⨂")
    (:pipe        "")
    ))
  "Options plist for `pretty-code-get-pairs'.")

;;; Core

;;;###autoload
(defun pretty-code-get-pairs (kwds)
  "Build an alist for prettify-symbols-alist from components from KWDS."
  (-non-nil
   (--map (when-let (major-mode-symbol (plist-get kwds it))
           (list major-mode-symbol
                 (plist-get pretty-code-choices it)))
         pretty-code-choices)))

;;;###autoload
(defun pretty-code-set-pairs (hook-pairs-alist)
  "Add hooks setting `prettify-symbols-alist' for many modes"
  (-each hook-pairs-alist
    (-lambda ((hook pretty-pairs))
      (add-hook hook
                (lambda () (setq prettify-symbols-alist pretty-pairs))))))
