;; auto-complete mode

;(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)

(setq ac-use-menu-map t)

(setq ac-auto-show-menu 0.3)

;;
;; Use C-n/C-p to select candidates
;; --------------------------------

(define-key ac-completing-map "\C-n" 'ac-next)
(define-key ac-completing-map "\C-p" 'ac-previous)


(setq ac-auto-show-menu 0.3)

;(setq-default ac-sources '(ac-source-semantic-raw))
(setq-default ac-sources '(ac-source-semantic ac-source-semantic-raw))

