;;; Personal Layer

(setq personal-packages
      '(
        (blog :location local)
        (outline-ivy :location local)
        ))

;;; Blog

(defun personal/init-blog ()
  (use-package blog
    :after macros
    :if (executable-find "hugo")
    :init
    (setq blog-dir
          (os-path "~/dev/blog")

          blog-public-dir
          (os-path "~/dev/public-blog"))))

;;; Outline-ivy

(defun personal/init-outline-ivy ()
  (use-package outline-ivy
    :after ivy outshine macros))
