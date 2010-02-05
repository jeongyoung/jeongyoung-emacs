
;;inhibit the startup screen
(setq inhibit-startup-message t)

(when window-system
  (set-default-font "Terminus 12")
  (set-face-font 'default "Terminus 12") ;; for speedbar's font
; (set-fontset-font "fontset-default" 'hangul '("나눔고딕코딩" . "unicode-bmp"))
; (set-fontset-font "fontset-default" 'kana '("나눔고딕코딩" . "unicode-bmp"))
; (set-fontset-font "fontset-default" 'han '("나눔고딕코딩" . "unicode-bmp"))
; (set-fontset-font "fontset-default" 'cjk-misc '("나눔고딕코딩" . "unicode-bmp"))
  (set-fontset-font "fontset-default" '(#x1100 . #xffdc) '("돋움체" . "unicode-bmp")) ;;; 유니코드 한글영역
  (set-fontset-font "fontset-default" '(#xe0bc . #xf66e) '("돋움체" . "unicode-bmp")) ;;;유니코드 사용자 영역
  (set-frame-size (selected-frame) 120 70)
  )


(when window-system
  ;; Color
  (set-face-background 'default "DarkSlateGray")
  (set-face-foreground 'default "wheat")
  (set-face-foreground 'region "white")
; (set-face-background 'region "#254437")
  (set-face-background 'region "cadetblue")
)

