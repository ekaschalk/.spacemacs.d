;;; Macros Layer

(setq macros-packages
      '(
        (dash-functional :location
                         (recipe :fetcher github :repo "magnars/dash.el"))
        ))

;; All layers require dash-functional

(defun macros/init-dash-functional ()
  (use-package dash-functional))
