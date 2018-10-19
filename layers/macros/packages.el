;;; Macros Layer

(setq macros-packages
      '(
        dash
        dash-functional
        s

        (macros :location local)
        ))

;;; Common Packages

(defun macros/init-dash ()
  ;; This is actually always available prior to spacemacs loading everything.
  ;; The spacemacs core file `core-documentation' requires dash.
  (use-package dash))

(defun macros/init-dash-functional ()
  ;; I wish `dash-functional' wasn't split from `dash' so the same could be
  ;; said for this lib. As a result, we can't utilize full power of dash
  ;; outside the local pkgs in each layer that explicitly require 'macros.
  (use-package dash-functional
    :after dash))

(defun macros/init-s ()
  (use-package s))

;;; Macros

(defun macros/init-macros ()
  (use-package macros
    :after dash dash-functional s))
