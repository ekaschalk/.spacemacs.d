;;; Personal Layer

(setq personal-packages
      '(
        (blog :location local)
        (outline-ivy :location local)
        ))

;;; Blog

(defun personal/init-blog ()
  (use-package blog
    :init
    (progn
      (setq blog-dir (os-path "~/dev/blog"))
      (setq blog-public-dir (os-path "~/dev/public-blog"))
      (setq blog-hugo-process "Hugo Server")
      (setq blog-hugo-server-site "http://localhost:1313/"))

    :config
    (progn
      (spacemacs/set-leader-keys (kbd "ab") 'blog-deploy)
      (spacemacs/set-leader-keys (kbd "aa") 'blog-start-server)
      (spacemacs/set-leader-keys (kbd "ae") 'blog-end-server))))

;;; Outline-ivy

(defun personal/init-outline-ivy ()
  (use-package outline-ivy
    :after outshine
    :config
    (global-set-key (kbd "C-j") 'oi-jump)))
