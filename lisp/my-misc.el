
(setq delete-by-moving-to-trash t)
;(add-to-list 'ido-ignore-files "\\.DS_Store")



;(tramp-set-completion-function "ssh"
;                               '((tramp-parse-sconfig "/etc/ssh_config")
;                                 (tramp-parse-sconfig "~/.ssh/config")))



(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (highlight-parentheses-mode)
             (setq autopair-handle-action-fns
                   (list 'autopair-default-handle-action
                         '(lambda (action pair pos-before)
                            (hl-paren-color-update))))))



													

;; ====================
;; insert date and time
;;

(require 'insert-time)

(define-key global-map [(control c)(t)] 'insert-date-time)
(define-key global-map [(control c)(d)] 'insert-date)

(define-key global-map [(control c)(control v)(d)] 'insert-personal-time-stamp)


;; ====================
;; tab width
;; ====================

(setq default-tab-width 2)
