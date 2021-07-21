

(setq mail-host-address "gmail.com")
(setq user-mail-address "jeongyoung@gmail.com")
(setq send-mail-function (quote smtpmail-send-it))
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq smtpmail-auth-credentials (quote (("smtp.gmail.com" 587 "jeongyoung@gmail.com" nil))))
(setq smtpmail-starttls-credentials (quote (("smtp.gmail.com" 587 nil nil))))
