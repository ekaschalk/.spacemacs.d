;;; -*- lexical-binding: t -*-


(require 'prettify-utils)
(require 'macros)

(provide 'pretty-code)

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
