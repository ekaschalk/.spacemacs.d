;;; Config Layer

(setq config-packages
      '(
        ;; Navigation
        avy
        outshine
        projectile
        treemacs
        treemacs-evil

        ;; Misc
        ispell
        gnus
        yasnippet

        ;; Grouped config sections
        (ivy-config :location local)
        (evil-config :location local)
        (org-config :location local)
        ))

;;; Navigation
;;;; Avy

(defun config/post-init-avy ()
  (setq avy-timeout-seconds 0.35)
  (evil-global-set-key 'normal (kbd "s") 'avy-goto-char-timer)
  (global-set-key (kbd "C-h") 'avy-pop-mark)
  (global-set-key (kbd "C-l") 'evil-avy-goto-line))

;;;; Outshine

(defun config/init-outshine ()
  (use-package outshine
    :init
    (progn
      (spacemacs/set-leader-keys
        "nn" 'outshine-narrow-to-subtree
        "nw" 'widen)

      (let ((kmap outline-minor-mode-map))
        (define-key kmap (kbd "M-RET") 'outshine-insert-heading)
        (define-key kmap (kbd "<backtab>") 'outshine-cycle-buffer)))

    :config
    (progn
      ;; Narrowing works within the headline rather than requiring to be on it
      (advice-add 'outshine-narrow-to-subtree :before
                  (lambda (&rest args) (unless (outline-on-heading-p t)
                                    (outline-previous-visible-heading 1))))

      (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
      (add-hook 'prog-mode-hook 'outline-minor-mode))))

;;;; Treemacs

(defun config/pre-init-treemacs ()
  (evil-global-set-key 'normal
                       (kbd "M-f") 'treemacs-select-window)
  (evil-global-set-key 'normal
                       (kbd "M-p") 'treemacs-projectile-toggle))

(defun config/post-init-treemacs ()
  (spacemacs|use-package-add-hook treemacs
    :post-config
    (progn
      (setq treemacs-show-hidden-files
            nil)
      (setq treemacs-silent-refresh
            t)
      (setq treemacs-is-never-other-window
            t)
      (setq treemacs-filewatch-mode
            nil)

      (define-key treemacs-mode-map
        (kbd "C-k") 'evil-previous-line-5)
      (define-key treemacs-mode-map
        (kbd "C-j") 'evil-next-line-5)

      (defun treemacs-ignore-pyfiles-predicates (f path)
        "Python files to ignore in treemacs."
        (pcase f
          ("__init__.py" f)
          ("__pycache__" f)
          (_ nil)))

      (push 'treemacs-ignore-pyfiles-predicates
            treemacs-ignored-file-predicates))))

(defun config/post-init-treemacs-evil ()
  (defun evil-previous-line-5 () (interactive) (evil-previous-line 5))
  (defun evil-next-line-5 ()     (interactive) (evil-next-line 5))

  (spacemacs|use-package-add-hook treemacs-evil
    :post-config
    (progn
      (define-key evil-treemacs-state-map
        "l" 'treemacs-visit-node-ace)
      (evil-define-key '(normal operator motion emacs) treemacs-mode-map
        "u" 'treemacs-uproot
        "h" 'treemacs-goto-parent-node
        "s" 'treemacs-toggle-show-dotfiles
        "r" 'treemacs-change-root))))

;;;; Projectile

(defun config/post-init-projectile ()
  (setq projectile-indexing-method 'native))

;;; Misc
;;;; Ispell

(defun config/post-init-ispell ()
  (setq ispell-program-name "aspell"))

;;;; Gnus

(defun config/post-init-gnus ()
  (setq user-mail-address	"ekaschalk@gmail.com"
        user-full-name	"Eric Kaschalk"

        ;; Get mail
        gnus-secondary-select-methods
        '((nnimap "gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl))
          (nntp "gmane"
                (nntp-address "news.gmane.org"))
          (nntp "news.gwene.org"))

        ;; Send mail
        message-send-mail-function 'smtpmail-send-it

        ;; Archive outgoing email in Sent folder on imap.gmail.com
        gnus-message-archive-method '(nnimap "imap.gmail.com")
        gnus-message-archive-group "[Gmail]/Sent Mail"

        ;; Auth
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                     "ekaschalk@gmail.com" nil))

        ;; SMPT Server config
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587

        ;; set return email address based on incoming email address
        gnus-posting-styles
        '(((header "to" "address@outlook.com")
           (address  "address@outlook.com"))
          ((header "to" "address@gmail.com")
           (address "address@gmail.com")))

        ;; store email in ~/gmail directory
        nnml-directory "~/gmail"
        message-directory "~/gmail"

        ;; Full size images
        mm-inline-large-images 'resize))

;;;; Yasnippet

(defun config/pre-init-yasnippet ()
  (global-set-key (kbd "C-SPC") 'hippie-expand))

;;; Local

(defun config/init-evil-config ()
  (use-package evil-config
    :after macros))

(defun config/init-ivy-config ()
  (use-package ivy-config
    :after macros))

(defun config/init-org-config ()
  (use-package org-config
    :after org))
