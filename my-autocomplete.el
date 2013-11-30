;; auto-complete mode
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)
;(global-auto-complete-mode t)

;;
;; Use C-n/C-p to select candidates
;; --------------------------------

(define-key ac-completing-map "\C-n" 'ac-next)
(define-key ac-completing-map "\C-p" 'ac-previous)


