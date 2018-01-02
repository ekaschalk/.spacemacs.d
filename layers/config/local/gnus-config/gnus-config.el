(require 'gnus)

(provide 'gnus-config)


(setq
 user-mail-address
 "ekaschalk@gmail.com"

 user-full-name
 "Eric Kaschalk"

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
 message-send-mail-function
 'smtpmail-send-it

 ;; Archive outgoing email in Sent folder on imap.gmail.com
 gnus-message-archive-method
 '(nnimap "imap.gmail.com")

 gnus-message-archive-group
 "[Gmail]/Sent Mail"

 ;; Auth
 smtpmail-starttls-credentials
 '(("smtp.gmail.com" 587 nil nil))
 smtpmail-auth-credentials
 '(("smtp.gmail.com" 587
    "ekaschalk@gmail.com" nil))

 ;; SMPT Server config
 smtpmail-default-smtp-server
 "smtp.gmail.com"

 smtpmail-smtp-server
 "smtp.gmail.com"

 smtpmail-smtp-service
 587

 ;; set return email address based on incoming email address
 gnus-posting-styles
 '(((header "to" "address@outlook.com")
    (address  "address@outlook.com"))
   ((header "to" "address@gmail.com")
    (address "address@gmail.com")))

 ;; store email in ~/gmail directory
 nnml-directory
 "~/gmail"

 message-directory
 "~/gmail"

 ;; Full size images
 mm-inline-large-images
 'resize)
