;;; personal.el --- The random config dumping ground -*- lexical-binding: t; -*-

(require 'mu4e)

(provide 'personal)


;;; Personal

;; Emacs-anywhere defaults to org-mode with maximized window
(add-hook 'ea-popup-hook
          (lambda (&rest args) (org-mode) (spacemacs/toggle-maximize-buffer)))

;; Hy-mode development
;; (load-file "~/dev/hy-mode/hy-mode.el")
;; (load-file "~/dev/hy-mode/hy-personal.el")
;; (require 'hy-mode)
;; (require 'hy-personal)

;; Emacs-core development
;; (setq find-function-C-source-directory "~/dev/emacs-dev/src")

;; message.el
(setq message-directory "~/mail")
(setq message-send-mail-function 'smtpmail-send-it)

;; smptmail.el
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq smtpmail-default-smtp-server "smtp.gmail.com")
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
(setq smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                   "ekaschalk@gmail.com" nil)))

;; mu4e
;; solid
(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-maildir "~/mail")
(setq mu4e-sent-messages-behavior 'delete)
(setq user-mail-address "ekaschalk@gmail.com")
(setq mu4e-user-mail-address-list (list user-mail-address))

;; experiment
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-maildir-shortcuts '(("/INBOX"               . ?i)
                               ("/[Gmail].Sent Mail"   . ?s)))

;; mu4e-vars.el go through this

(setq mu4e-hide-index-messages t)
;; mu4e-use-fancy-chars  ; true by default
;; mu4e-marks            ; all the unicode stuff setup here
;; configure through `mu4e-headers-..-mark' and `mu4e-headers..-prefix'
;; mu4e-enable-async-operations
;; (setq mu4e-update-interval 600)
;; (setq mu4e-index-cleanup nil)      ;; don't do a full cleanup check
;; (setq mu4e-index-lazy-check t)    ;; don't consider up-to-date dir
;; w3m -dump -T text/html
