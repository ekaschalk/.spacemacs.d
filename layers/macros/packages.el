;;; Macros Layer

(setq macros-packages
      '(
        dash
        dash-functional
        s

        (macros :location local)
        ))

(defun macros/init-dash ()
  (use-package dash))

(defun macros/init-dash-functional ()
  (use-package dash-functional
    :after dash))

(defun macros/init-s ()
  (use-package s))

(defun macros/init-macros ()
  (use-package macros
    :after dash dash-functional s))
