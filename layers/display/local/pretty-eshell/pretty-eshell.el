(require 'dash)
(require 's)

(provide 'pretty-eshell)

;;; Config

(defvar eshell-funcs nil
  "List of `esh-section' to enable.")

(defvar esh-sep "  "
  "String delimits each `esh-section'")

(defvar esh-section-delim " "
  "String delimits icons and their text.")

(defvar esh-header "\n "
  "Initial string composing the eshell prompt.")

(defvar esh-prompt-string " "
  "Prompt string, must match builtin `eshell-prompt-regexp'")

;;; Section utilities

(setq esh-prompt-num 0)
(add-hook 'eshell-exit-hook
          (lambda () (setq esh-prompt-num 0)))
(advice-add 'eshell-send-input :before
            (lambda (&rest args) (setq esh-prompt-num (+ 1 esh-prompt-num))))

;;; Core

;;;###autoload
(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

;;;###autoload
(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME
         (lambda () (when ,FORM
                 (-> ,ICON
                    (concat esh-section-delim ,FORM)
                    (with-face ,@PROPS))))))

;;;###autoload
(defun esh--acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (concat acc esh-sep it))
    acc))

;;;###autoload
(defun esh-prompt-func ()
  "Replacement for `eshell-prompt-function'"
  (concat esh-header
          (-reduce-from 'esh--acc "" eshell-funcs)
          "\n"
          esh-prompt-string))

(setq eshell-prompt-function 'esh-prompt-func)
