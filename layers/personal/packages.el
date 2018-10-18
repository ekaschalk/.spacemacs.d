;;; Personal Layer

(setq personal-packages
      '((blog :location local)
        (outline-ivy :location local)))

;;; Blog

(defun personal/init-blog ()
  (use-package blog
    :after macros
    :if (executable-find "hugo")
    :init (progn
            (setq blog-dir '("~/dev/blog"))
            (setq blog-public-dir '("~/dev/public-blog"))
            (spacemacs/set-leader-keys
              "ab" 'blog-deploy
              "aa" 'blog-start-server
              "ae" 'blog-end-server))))

;;; Outline-ivy

(defun personal/init-outline-ivy ()
  (use-package outline-ivy
    :after ivy outshine macros
    :init (global-set-key (kbd "C-j") 'oi-jump)))
