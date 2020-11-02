;; 한글관련 셋팅

;; Hangul IME
;; (set-language-environment "Korean")
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

(set-default-coding-systems 'utf-8) ;; utf-8
(setq default-input-method "korean-hangul")
;; (global-set-key (kbd "S-SPC") 'toggle-korean-input-method) ;; if not working

(if (eq system-type 'gnu/linux)
    (progn
      (set-fontset-font "fontset-default" 'korean-ksc5601 "-SAND-NanumGothicCoding-normal-normal-normal-*-*-*-*-*-d-0-iso10646-1")
      (setq initial-frame-alist '((top . 40) (left . 1000)))
      (setq default-frame-alist '((width . 100) (height . 50)))))

      


