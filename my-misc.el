
(setq delete-by-moving-to-trash t)
;(add-to-list 'ido-ignore-files "\\.DS_Store")
;(setq mac-command-modifier 'meta)


;(tramp-set-completion-function "ssh"
;                               '((tramp-parse-sconfig "/etc/ssh_config")
;                                 (tramp-parse-sconfig "~/.ssh/config")))



;; ====================
;; insert date and time
;;

(require 'insert-time)

(define-key global-map [(control c)(t)] 'insert-date-time)
(define-key global-map [(control c)(d)] 'insert-date)

(define-key global-map [(control c)(control v)(d)] 'insert-personal-time-stamp)


