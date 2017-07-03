;; This code is my scratch and notes for solving
;; font lock indentation discrepancies

;;; ALGORITHM

;; KEYS
;; 1. get-text-property (point) 'display
;;    returns nil if no adjustment, otherwise the string like []

;; So I can just apply one at a time by adding-to/subtracting the current spaces

;; it finds a match
;; keep adding to properties up until current-indentation >= the column

;; have global hashlist as mismatch-contributions-from
;; if regex returns a match -> all cols we apply to get added for that match
;;    that arent already in the contributions
;; if regex returns nil -> remove the mismatch size for all
;;    that are in the contributions

;;;;;;;;

set prev-mismatch-size = 0
set running-mismatch-size = 0
while next-line
  set mismatches (col missed)
  goto next line

  set mismatch-size = sum (mismatches < current-indentation)
  if not mismatch-size
     ;; do same for all prev-mismatches


  for i, mismatches in enumerate preceeding-mismatches
    filter-values size < current-indentation
    if filtered is empty
       pop ith preceeding-mismatches
    else
       set-display-prop bol+i, bol+i+1 (" " * sum(filtered-mismatches))

  if no preceeding-mismatches
    break

  goto next line

;; what if i just do this stuff on indentation changes?
;; if the indentation got smaller -> start popping
;; if the indentation stayed same -> use same mismatch size
;; if the indentation got larger -> add current line to new mismatch size

;; an issue is that this will just keep adding
;; whenever the regex matches
;; have to check if the line is already altered by something else
;; if it is altered, then do nothing as it was already caught

;; addto preceeding-mismatches
;; goto next line
;; while next-line
;;   for i, mismatches in enumerate preceeding-mismatches
;;     filter-values size < current-indentation
;;     if filtered is empty
;;        pop ith preceeding-mismatches
;;     else
;;        set-display-prop bol+i, bol+i+1 (" " * sum(filtered-mismatches))
;;
;;   if no preceeding-mismatches
;;     break
;;
;;   addto preceeding-mismatches
;;   goto next line

;;  (replacethis foo hit
;;               bar)
;;  (foo replacethis hit
;;       bar)
;;  (x foo hit
;;     bar)

;;; Code

(defun column-number-at-pos (pos)
  "Analog to line-number-at-pos."
  (save-excursion (goto-char pos) (current-column)))

(defun test ()
  )


(progn
  (search-forward-regexp "\\(hit\\)"
                         nil t)
  (let ((col (column-number-at-pos (match-beginning 1))))
          col
    )

  )


;;; Scratch

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Hash-Tables.html
;; (setq x (make-hash-table))

;; k v table
;; gethash, puthash, remhash
;; maphash (lambda (k v))

;; dash -lets
;; (&hash key0 a0 ... keyN aN) - bind value mapped by keyK in the
;; `source` hash table to aK.  If the
;; value is not found, aK is nil.

;;; Outlines
