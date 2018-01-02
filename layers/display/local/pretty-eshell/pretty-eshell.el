(require 'macros)

(provide 'pretty-eshell)

;;; Config

(defvar pretty-eshell-funcs nil
  "List of `pretty-eshell-section' to enable.")

(defvar pretty-eshell-sep "  "
  "String delimits each `pretty-eshell-section'")

(defvar pretty-eshell-section-delim " "
  "String delimits icons and their text.")

(defvar pretty-eshell-header "\n "
  "Initial string composing the eshell prompt.")

(defvar pretty-eshell-prompt-string " "
  "Prompt string, must match builtin `eshell-prompt-regexp'")

(defvar pretty-eshell-prompt-num 0
  "Prompt number for current eshell session.")

;;; Section utilities

(add-hook 'eshell-exit-hook
          (lambda () (setq pretty-eshell-prompt-num 0)))
(advice-add 'eshell-send-input :before
            (lambda (&rest args) (incf pretty-eshell-prompt-num)))

;;; Core

;;;###autoload
(defmacro pretty-eshell-section (name icon form &rest props)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  ;; Roundabout way to handle case that
  ;; 1. Form is a variable and
  ;; 2. That variable might not be defined/initialized
  ;; Eg. pyvenv-virtualenv-name not loaded until pyvenv-workon
  `(setq ,name
         (lambda ()
           (when (or (and (symbolp (quote ,form))
                          (bound-and-true-p ,form))
                     (and (not (symbolp (quote ,form)))
                          ,form))
             (-> ,icon
                (concat pretty-eshell-section-delim ,form)
                (with-face ,@props))))))

;;;###autoload
(defun pretty-eshell--acc (acc x)
  "Accumulator for evaluating and concatenating pretty-eshell-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (s-concat acc pretty-eshell-sep it))
    acc))

;;;###autoload
(defun pretty-eshell-prompt-func ()
  "Value for `eshell-prompt-function'."
  (concat pretty-eshell-header
          (-reduce-from 'pretty-eshell--acc "" pretty-eshell-funcs)
          "\n"
          pretty-eshell-prompt-string))

(setq eshell-prompt-function
      'pretty-eshell-prompt-func)
