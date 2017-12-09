(require 'evil)
(require 'dash)
(require 'dash-functional)
(require 's)

(provide 'macros)


;;; define-keys

(defun define-keys (keymap &rest pairs)
  "Define alternating key-def PAIRS for KEYMAP."
  (-each
      (-partition 2 pairs)
    (-lambda ((key def))
      (define-key keymap key def))))

;;; global-set-keys

(defun global-set-keys (&rest pairs)
  "Set alternating key-def PAIRS globally."
  (-each
      (-partition 2 pairs)
    (-lambda ((key def))
      (global-set-key key def))))

;;; evil-global-set-keys

(defun evil-global-set-keys (states &rest pairs)
  "Set alternating key-def PAIRS for all evil STATES."
  (-each
      (-partition 2 pairs)
    (-lambda ((key def))
      (--each states
        (evil-global-set-key it key def)))))

;;; with-dir

(defmacro with-dir (DIR &rest FORMS)
  "Execute FORMS in DIR."
  (let ((orig-dir (gensym)))
    `(prog2
         (setq ,orig-dir default-directory)
         (progn (cd ,DIR) ,@FORMS)
       (cd ,orig-dir))))

;;; with-face

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))
