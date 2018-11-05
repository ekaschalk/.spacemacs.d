(require 'mu4e)

(provide 'mu4e-config)

;;; mu4e-config

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


;; investigate
;; mu4e-enable-async-operations
;; (setq mu4e-update-interval 600)
;; (setq mu4e-index-cleanup nil)      ;; don't do a full cleanup check
;; (setq mu4e-index-lazy-check t)    ;; don't consider up-to-date dir
;; w3m -dump -T text/html
