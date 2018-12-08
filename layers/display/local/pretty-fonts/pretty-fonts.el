;;; pretty-fonts.el --- Ligature and fontset setters -*- lexical-binding: t; -*-

;;; Commentary:

;; A heavily annotated, cleaned-up version of ligature implementations for Emacs
;; floating around on the web. If you ever looked at the snippets online and
;; thought wtf is going on, this implementation should clear things up.

;;; Code:
;;;; Requires

(require 'dash)
(require 'dash-functional)

;;;; Configuration
;;;;; Fira-code Ligatures

(defconst pretty-fonts-fira-code-alist
  '(;; OPERATORS
    ;; Pipes
    ("\\(<|\\)" #Xe14d) ("\\(<>\\)" #Xe15b) ("\\(<|>\\)" #Xe14e) ("\\(|>\\)" #Xe135)

    ;; Brackets
    ("\\(<\\*\\)" #Xe14b) ("\\(<\\*>\\)" #Xe14c) ("\\(\\*>\\)" #Xe104)
    ("\\(<\\$\\)" #Xe14f) ("\\(<\\$>\\)" #Xe150) ("\\(\\$>\\)" #Xe137)
    ("\\(<\\+\\)" #Xe155) ("\\(<\\+>\\)" #Xe156) ("\\(\\+>\\)" #Xe13a)

    ;; Equality
    ("\\(!=\\)" #Xe10e) ("\\(!==\\)"         #Xe10f) ("\\(=/=\\)" #Xe143)
    ("\\(/=\\)" #Xe12c) ("\\(/==\\)"         #Xe12d)
    ("\\(===\\)"#Xe13d) ("[^!/]\\(==\\)[^>]" #Xe13c)

    ;; Equality Special
    ("\\(||=\\)"  #Xe133) ("[^|]\\(|=\\)" #Xe134)
    ("\\(~=\\)"   #Xe166)
    ("\\(\\^=\\)" #Xe136)
    ("\\(=:=\\)"  #Xe13b)

    ;; Comparisons
    ("\\(<=\\)" #Xe141) ("\\(>=\\)" #Xe145)
    ("\\(</\\)" #Xe162) ("\\(</>\\)" #Xe163)

    ;; Shifts
    ("[^-=]\\(>>\\)" #Xe147) ("\\(>>>\\)" #Xe14a)
    ("[^-=]\\(<<\\)" #Xe15c) ("\\(<<<\\)" #Xe15f)

    ;; Dots
    ("\\(\\.-\\)"    #Xe122) ("\\(\\.=\\)" #Xe123)
    ("\\(\\.\\.<\\)" #Xe125)

    ;; Hashes
    ("\\(#{\\)"  #Xe119) ("\\(#(\\)"   #Xe11e) ("\\(#_\\)"   #Xe120)
    ("\\(#_(\\)" #Xe121) ("\\(#\\?\\)" #Xe11f) ("\\(#\\[\\)" #Xe11a)

    ;; REPEATED CHARACTERS
    ;; 2-Repeats
    ("\\(||\\)" #Xe132)
    ("\\(!!\\)" #Xe10d)
    ("\\(%%\\)" #Xe16a)
    ("\\(&&\\)" #Xe131)

    ;; 2+3-Repeats
    ("\\(##\\)"       #Xe11b) ("\\(###\\)"         #Xe11c) ("\\(####\\)" #Xe11d)
    ("\\(--\\)"       #Xe111) ("\\(---\\)"         #Xe112)
    ("\\({-\\)"       #Xe108) ("\\(-}\\)"          #Xe110)
    ("\\(\\\\\\\\\\)" #Xe106) ("\\(\\\\\\\\\\\\\\)" #Xe107)
    ("\\(\\.\\.\\)"   #Xe124) ("\\(\\.\\.\\.\\)"   #Xe126)
    ("\\(\\+\\+\\)"   #Xe138) ("\\(\\+\\+\\+\\)"   #Xe139)
    ("\\(//\\)"       #Xe12f) ("\\(///\\)"         #Xe130)
    ("\\(::\\)"       #Xe10a) ("\\(:::\\)"         #Xe10b)

    ;; ARROWS
    ;; Direct
    ("[^-]\\(->\\)" #Xe114) ("[^=]\\(=>\\)" #Xe13f)
    ("\\(<-\\)"     #Xe152)
    ("\\(-->\\)"    #Xe113) ("\\(->>\\)"    #Xe115)
    ("\\(==>\\)"    #Xe13e) ("\\(=>>\\)"    #Xe140)
    ("\\(<--\\)"    #Xe153) ("\\(<<-\\)"    #Xe15d)
    ("\\(<==\\)"    #Xe158) ("\\(<<=\\)"    #Xe15e)
    ("\\(<->\\)"    #Xe154) ("\\(<=>\\)"    #Xe159)

    ;; Branches
    ("\\(-<\\)"  #Xe116) ("\\(-<<\\)" #Xe117)
    ("\\(>-\\)"  #Xe144) ("\\(>>-\\)" #Xe148)
    ("\\(=<<\\)" #Xe142) ("\\(>>=\\)" #Xe149)
    ("\\(>=>\\)" #Xe146) ("\\(<=<\\)" #Xe15a)

    ;; Squiggly
    ("\\(<~\\)" #Xe160) ("\\(<~~\\)" #Xe161)
    ("\\(~>\\)" #Xe167) ("\\(~~>\\)" #Xe169)
    ("\\(-~\\)" #Xe118) ("\\(~-\\)"  #Xe165)

    ;; MISC
    ("\\(www\\)"                   #Xe100)
    ("\\(<!--\\)"                  #Xe151)
    ("\\(~@\\)"                    #Xe164)
    ("[^<]\\(~~\\)"                #Xe168)
    ("\\(\\?=\\)"                  #Xe127)
    ("[^=]\\(:=\\)"                #Xe10c)
    ("\\(/>\\)"                    #Xe12e)
    ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
    ("[^:=]\\(:\\)[^:=]"           #Xe16c)
    ("\\(<=\\)"                    #Xe157))
  "Fira font ligatures and their regexes")

;;;; Fontsetters

(defun pretty-fonts-set-fontsets (font-codepoints-alist)
  "Set CODEPOINTS to use FONT in FONT-CODEPOINTS-ALIST in all situations."
  (-each font-codepoints-alist
    (-lambda ((font . codepoints))
      (-each codepoints
        (lambda (codepoint)
          (set-fontset-font nil `(,codepoint . ,codepoint) font)
          (set-fontset-font t `(,codepoint . ,codepoint) font))))))

;;;; Ligatures

(defun pretty-fonts--pad-codepoint (codepoint)
  "Converts CODEPOINT to a string that `compose-region' will know to leftpad.

A snippet from it's documentation:

  If it is a string, the elements are alternate characters. In this case, TAB
  element has a special meaning. If the first character is TAB, the glyphs are
  displayed with left padding space so that no pixel overlaps with the previous
  column. If the last character is TAB, the glyphs are displayed with right
  padding space so that no pixel overlaps with the following column.

Also note that prior implementations use `list' instead of `char-to-string',
they do the same thing here but `char-to-string' is obviously more descriptive."
  (concat "\t" (char-to-string codepoint)))

(defun pretty-fonts--build-keyword (rgx codepoint)
  "Builds the font-lock-keyword for RGX to be composed to CODEPOINT.

This function may seem obtuse. It can be translated into the spec
defined in `font-lock-add-keywords' as follows:

  (MATCHER . HIGHLIGHT=MATCH-HIGHLIGHT=(SUBEXP=0 FACENAME=expression))

The FACENAME is a form that should evaluate to a face. In the case it returns
nil, which we do here, it won't modify the face. If instead it was `(prog1
font-lock-function-name-face compose...)', the composition would still be applied
but now all ligatures would be highlighted as functions, for example."
  `(,rgx (0 (prog1 nil
              (compose-region (match-beginning 1)
                              (match-end 1)
                              ,(pretty-fonts--pad-codepoint codepoint))))))

(defun pretty-fonts-add-kwds (rgx-codepoint-alist)
  "Exploits `font-lock-add-keywords' to transform RGXs into CODEPOINTs."
  (->> rgx-codepoint-alist
     (-map (-applify #'pretty-fonts--build-keyword))
     (font-lock-add-keywords nil)))

(defun pretty-fonts-add-hook (hook rgx-codepoint-alist)
  "Add `pretty-fonts-add-kwds' as a hook."
  (add-hook hook
            (lambda () (pretty-fonts-add-kwds rgx-codepoint-alist))))

(defun pretty-fonts-set-fontsets-for-fira-code ()
  "Tell Emacs to render Fira Code codepoints using Fira Code Symbol font."
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

(provide 'pretty-fonts)
