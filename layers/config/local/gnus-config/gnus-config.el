(require 'gnus)

(provide 'gnus-config)


(setq user-mail-address "ekaschalk@gmail.com")
(setq user-full-name "Eric Kaschalk")

(setq mm-inline-large-images 'resize)

(setq message-send-mail-function 'smtpmail-send-it)
(setq message-directory "~/gmail")

(setq nnml-directory "~/gmail")

(setq gnus-posting-styles '(((header "to" "address@outlook.com")
                             (address  "address@outlook.com"))
                            ((header "to" "address@gmail.com")
                             (address "address@gmail.com"))))
(setq gnus-message-archive-method '(nnimap "imap.gmail.com"))
(setq gnus-message-archive-group "[Gmail]/Sent Mail")
(setq gnus-secondary-select-methods '((nnimap "gmail"
                                              (nnimap-address "imap.gmail.com")
                                              (nnimap-server-port 993)
                                              (nnimap-stream ssl))
                                      (nntp "gmane"
                                            (nntp-address "news.gmane.org"))
                                      (nntp "news.gwene.org")))

(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq smtpmail-default-smtp-server "smtp.gmail.com")
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
(setq smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                   "ekaschalk@gmail.com" nil)))
