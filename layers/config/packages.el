(setq config-packages
      '(
        avy
        evil
        ispell
        ivy
        gnus
        neotree
        projectile
        yasnippet
        ))

(defun config/post-init-ivy ()
  (setq ivy-format-function 'ivy-format-function-arrow)
  (setq ivy-height 20)
  (setq completion-in-region-function 'ivy-completion-in-region)

  ;; Resume last ivy session
  (spacemacs/set-leader-keys
    "ai" 'ivy-resume)

  (let ((kmap ivy-minibuffer-map))
    ;; Perform default action on avy-selected minibuffer line
    (define-key kmap (kbd "C-l") 'ivy-avy)

    ;; Evil-like scrolling of ivy minibuffer
    (define-key kmap (kbd "C-u") 'ivy-scroll-down-command)
    (define-key kmap (kbd "C-d") 'ivy-scroll-up-command)

    ;; Rebind C-n/C-y/C-p to narrow/yank from buffer/paste into buffer
    (define-key kmap (kbd "C-n") 'ivy-restrict-to-matches)
    (define-key kmap (kbd "C-y") 'ivy-yank-word)

    ;; Read-only buffer of candidates with shortcuts to dispatches
    (define-key kmap (kbd "C-o") 'ivy-occur)

    ;; Non-exiting default action
    (define-key kmap (kbd "C-<return>") 'ivy-call)

    ;; Dispatch actions
    (define-key kmap (kbd "C-SPC") 'ivy-dispatching-done)
    (define-key kmap (kbd "C-S-SPC") 'ivy-dispatching-call)))

(defun config/post-init-avy ()
  (setq avy-timeout-seconds 0.35)
  (evil-global-set-key 'normal (kbd "s") 'avy-goto-char-timer)

  (global-set-key (kbd "C-h") 'avy-pop-mark)
  (global-set-key (kbd "C-l") 'evil-avy-goto-line))

(defun config/post-init-evil ()
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence "true")

  (advice-add 'evil-ex-search-next :after 'config/scroll-to-center-advice)
  (advice-add 'evil-ex-search-previous :after 'config/scroll-to-center-advice)

  ;; Interactive is needed for visual mode end-of-line to not take the \n
  (evil-global-set-keys
   '(normal visual motion)
   "H" 'evil-first-non-blank
   "L" (lambda () (interactive) (evil-end-of-line))
   "0" 'evil-jump-item))

(defun config/post-init-yasnippet ()
  (global-set-key (kbd "C-SPC") 'hippie-expand))

(defun config/post-init-projectile ()
  (setq projectile-indexing-method 'native))

(defun config/post-init-neotree ()
  (setq neo-theme 'icons
        neo-window-width 28)

  (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$"
                                 ;; Pycache and init rarely want to see
                                 "__pycache__" "__init__\\.py"))

  (evil-global-set-key 'normal (kbd "M-f") 'winum-select-window-0)
  (evil-global-set-key 'normal (kbd "M-p") 'neotree-find-project-root))

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

(defun config/post-init-ispell ()
  (setq ispell-program-name "aspell"))
