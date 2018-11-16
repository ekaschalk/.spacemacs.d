;;; blog.el --- Hugo server manager -*- lexical-binding: t; -*-

(require 'dash)
(require 'dash-functional)
(require 's)

(provide 'blog)

;;; Config

(defvar blog-dir nil
  "Hugo content directory")

(defvar blog-public-dir nil
  "Hugo output directory")

(defvar blog-hugo-process "Hugo Server"
  "Name of 'hugo server' process process")

(defvar blog-hugo-server-site "http://localhost:1313/"
  "URL for `blog-hugo-process'")

;;; Utils

(defmacro with-dir (DIR &rest FORMS)
  "Execute FORMS in DIR."
  (let ((orig-dir (gensym)))
    `(prog2
         (setq ,orig-dir default-directory)
         (progn (cd ,DIR) ,@FORMS)
       (cd ,orig-dir))))

;;; Blog Commands

;;;###autoload
(defun blog-deploy ()
  "Run hugo and push changes upstream."
  (interactive)
  (with-dir blog-public-dir
            (shell-command "git rm -rf .")
            (shell-command "git clean -fxd")
            (with-temp-file "CNAME"
              (insert "www.modernemacs.com\nmodernemacs.com"))

            (with-dir blog-dir (->> blog-public-dir
                                  (concat "hugo -d ")
                                  shell-command))

            (shell-command "git add .")
            (--> (current-time-string)
               (concat "git commit -m \"" it "\"")
               (shell-command it))
            (magit-push-current-to-upstream nil)))

;;;###autoload
(defun blog-start-server ()
  "Run hugo server if not already running and open its webpage."
  (interactive)
  (with-dir blog-dir
            (unless (get-process blog-hugo-process)
              (start-process blog-hugo-process nil "hugo" "server"))
            (browse-url blog-hugo-server-site)))

;;;###autoload
(defun blog-end-server ()
  "End hugo server process if running."
  (interactive)
  (--when-let (get-process blog-hugo-process)
    (delete-process it)))
