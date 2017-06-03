;; Maintaining this for when I look to restart the blog properly
;; Want to move to a better supported, more native blogging solution
;; See: http://ekaschalk.github.io
;; See: http://whyarethingsthewaytheyare.com/setting-up-the-blog/#workflow


(require 'dash)
(require 'pandoc)

(defun dotspacemacs/user-config/blog ())


(defun org-hugo-export ()
  (defun -export-post-process (file)
    (org-pandoc-run-to-buffer-or-file file 'markdown t nil))

  (defun hugo-pandoc-post-process (file title slug data categories
                                        process event)
    "Intended to be partially applied up before process arg"
    ;; Prepare buffer
    `(with-temp-file ,file
       (insert-file-contents ,file)
       (evil-goto-first-line)

       ;; Remove default header
       (save-excursion
         (re-search-forward "---\\(.\\|\n\\)+?---\n\n")
         (replace-match ""))

       ;; Insert new properties
       (insert (format
                "---\ntitle: %s\nslug: %s\ndate: %s\ncategories: %s\n---\n\n"
                ,title ,slug ,date ,categories))

       ;; Demote headings and tweak code blocks
       (dolist (reps '(("^#" . "##")
                       ("\n``` {\\.\\(.+?\\)}" . "```\\1")))
         (save-excursion
           (while (re-search-forward (car reps) nil t)
             (replace-match (cdr reps)))))))

  (interactive)
  (save-excursion
    (unless (eq (org-current-level) 1)
      (outline-up-heading 10))
    (let* ((org-pandoc-format 'markdown)
           (org-pandoc-options-for-markdown
            '((standalone . t) (atx-headers . t) (columns . 79)))

           (hl (org-element-at-point))
           (file (org-element-property :EXPORT_TO hl))
           (title (concat "\"" (org-element-property :title hl) "\""))
           (slug (concat "\"" (org-element-property :SLUG hl) "\""))
           (date (concat "\"" (org-element-property :DATE hl) "\""))

           (categories "[\"emacs\"]")
           (tmp (concat (make-temp-name ".tmp") ".org")))

      (org-export-to-file 'pandoc (org-export-output-file-name tmp t)
        nil t nil nil nil #'-post-process)

      ;; Modifies file after the export process has finished.
      (advice-add #'org-pandoc-sentinel :after
                  (-partial #'hugo-pandoc-post-process
                            file title slug data categories)
                  '((name . "hugo-advice")))

      ;; Remove advice for future pandoc exports
      (advice-remove #'org-pandoc-sentinel 'hugo-advice))))
