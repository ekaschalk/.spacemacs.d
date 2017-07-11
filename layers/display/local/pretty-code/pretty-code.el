(require 'dash)
(require 'prettify-utils)

(provide 'pretty-code)

;;; Config

(defvar pretty-options
  (-flatten
   (prettify-utils-generate
    ;; Functional
    (:lambda      "λ") (:def         "ƒ")
    (:composition "∘")

    ;; Types
    (:null        "∅") (:true        "𝕋") (:false       "𝔽")
    (:int         "ℤ") (:float       "ℝ")
    (:str         "𝕊") (:bool        "𝔹")

    ;; Flow
    (:in          "∈") (:not-in      "∉")
    (:return     "⟼") (:yield      "⟻")
    (:and         "∧") (:or          "∨")
    (:not         "￢")
    (:for         "∀")
    (:some        "∃")

    ;; Other
    (:tuple       "⨂")
    (:pipe        "")
    ))
  "Options plist for `pretty-code-get-pairs'.")

;;; Core

;;;###autoload
(defun pretty-code-get-pairs (KWDS)
  "Build an alist for prettify-symbols-alist from components from KWDS."
  (-non-nil
   (--map (when-let (major-mode-sym (plist-get KWDS it))
           `(,major-mode-sym
             ,(plist-get pretty-options it)))
         pretty-options)))

;;;###autoload
(defun pretty-code-set-pairs (HOOK-PAIRS-ALIST)
  "Add hooks setting `prettify-symbols-alist' for many modes"
  (mapc (lambda (x)
          (lexical-let ((pretty-pairs (cadr x)))
            (add-hook (car x)
                      (lambda ()
                        (setq prettify-symbols-alist pretty-pairs)))))
        HOOK-PAIRS-ALIST))
