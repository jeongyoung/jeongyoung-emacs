;; auto-complete mode
(require 'auto-complete)
(require 'auto-complete-config)

(global-auto-complete-mode t)

;;
;; Use C-n/C-p to select candidates
;; --------------------------------

(define-key ac-completing-map "\C-n" 'ac-next)
(define-key ac-completing-map "\C-p" 'ac-previous)

