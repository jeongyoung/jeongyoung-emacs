;; 컴파일 시 경고 창이 뜨는 것을 방지 (백그라운드에서 조용히 작업)
(setq native-comp-async-report-warnings-errors 'silent)

;; 컴파일 속도 및 최적화 레벨 설정 (0~3, 3이 최대 최적화)
(setq native-comp-speed 3)

;; 설치된 패키지들도 자동으로 네이티브 컴파일 수행
(setq package-native-compile t)


;; 시작 시 GC 임계값을 높여서 로딩 속도 향상 (100MB)
(setq gc-cons-threshold (* 100 1024 1024))

;; 부팅 완료 후 다시 낮춤 (정상 작동 시 메모리 효율을 위해)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1024 1024))))

;; 모든 버퍼에 대해 외부 변경 사항 자동 반영 (Global Auto Revert)
(global-auto-revert-mode 1)

;; use-package 

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

;;  패키지 저장소 Marmalade 추가
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))    

(eval-when-compile
  (setq use-package-always-ensure t)
  (require 'use-package))

;; install straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7)) ; 최신 버전 확인 (2026년 기준 version 7이 안정적)
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'use-package)

;; reload init file

(defun reload-init-file ()
  "Reload .emacs.d/init.el"
  (interactive)
  (load-file user-init-file))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hangul
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(set-default-coding-systems 'utf-8) ;; utf-8
(setq default-input-method "korean-hangul")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;inhibit the startup screen
(setq inhibit-startup-message t)
(setq truncate-lines nil)

(global-display-line-numbers-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; zenburn-theme
;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

(use-package all-the-icons)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (set-face-foreground 'font-lock-comment-face "#b1b1b1")
  ;; (set-face-foreground 'font-lock-comment-face "#9ca0a4")
  )


(with-eval-after-load 'doom-themes
  (doom-themes-neotree-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle))
  :defer
  :config
  (setq neo-window-width 55))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-revert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-auto-revert-mode 1)
(setq fast-but-imprecise-scrolling t)
(setq scroll-conservatively 101)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq delete-by-moving-to-trash t)

;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda ()
;;              (highlight-parentheses-mode)
;;              (setq autopair-handle-action-fns
;;                    (list 'autopair-default-handle-action
;;                          '(lambda (action pair pos-before)
;;                             (hl-paren-color-update))))))

;; (require 'insert-time)

;; (define-key global-map [(control c)(t)] 'insert-date-time)
;; (define-key global-map [(control c)(d)] 'insert-date)

;; (define-key global-map [(control c)(control v)(d)] 'insert-personal-time-stamp)


(setq default-tab-width 4)

;; Minibuffer and buffer navigation
;; (use-package counsel
;;   :bind
;;   ("C-."   . 'counsel-imenu)
;;   ("C-c '" . 'projectile-grep)
;;   ("C-c ," . 'counsel-imenu)
;;   ("C-h f" . 'counsel-describe-function)
;;   ("C-h v" . 'counsel-describe-variable)
;;   ("C-o"   . 'counsel-outline)
;;   ("C-x b" . 'counsel-switch-buffer))

(use-package paredit
  :init
  (add-hook 'emacs-list-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(use-package helm
  :ensure t
  :init
  (helm-mode 1)
  (progn (setq helm-buffers-fuzzy-matching t))
  :bind
  (("C-c h" . helm-command-prefix))
  (("M-x" . helm-M-x))
  (("C-x C-f" . helm-find-files))
  (("C-x b" . helm-buffers-list))
  (("C-c b" . helm-bookmarks))
  (("C-c f" . helm-recertf))
  (("C-s" . helm-occur))
  (("C-c g" . helm-grep-do-git-grep)))
;; :init
  ;; (require 'helm-config))

(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t)

;; (helm-mode 1)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x r b") 'helm-bookmarks)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "C-s") 'helm-occur)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x k") 'kill-this-buffer)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)


(use-package helm-descbinds
:ensure t
:bind ("C-h b" . helm-descbinds))


(electric-pair-mode 1)


(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all))

(use-package yaml-mode)

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "s-p") `projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") `projectile-command-map))

(use-package flycheck)


(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp-java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lsp-mode 설치


(setq lsp-java-vmargs
      '("-XX:+UseParallelGC"
        "-XX:GCTimeRatio=4"
        "-XX:AdaptiveSizePolicyWeight=90"
        "-Dsun.zip.disableMemoryMapping=true"
        "-Xmx2G"
        "-Xms100m"
        "-javaagent:/home/jeongyoung/.m2/repository/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar"))


(use-package lsp-mode
:ensure t
:hook (
   (lsp-mode . lsp-enable-which-key-integration)
   (java-mode . #'lsp-deferred)
   (js-ts-mode . #'lsp-deferred)
)
:init (setq 
    lsp-keymap-prefix "C-c l"              ; this is for which-key integration documentation, need to use lsp-mode-map
    lsp-enable-file-watchers nil
    read-process-output-max (* 1024 1024)  ; 1 mb
    lsp-completion-provider :capf
    lsp-idle-delay 0.500
)
:config 
    (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
    (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
	(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
	)

(use-package helm-lsp
:ensure t
:after (lsp-mode)
:commands (helm-lsp-workspace-symbol)
:init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

;; lsp-ui, optional
(use-package lsp-ui
:ensure t
:after (lsp-mode)
:bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
:init
(setq lsp-ui-doc-delay 1.5
	    lsp-ui-doc-position 'bottom
	    lsp-ui-doc-max-width 100
	    lsp-ui-doc-enable 1
	    lsp-ui-doc-show-with-cursor 1
	    )
(setq lsp-ui-doc-frame-alist '((:foreground "white" :background "black"))) ; 예시: 흰색 글씨, 검은색 배경
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background "#1e1e1e"))))
 '(lsp-ui-doc-border ((t (:background "#5e81ac"))))
 '(lsp-ui-doc-header ((t (:foreground "#ffffff" :background "#3a3f5a" :bold t)))))


;; company-mode, for completion
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; dap-mode, optional (디버깅용)
(use-package dap-mode
  :ensure t)

(use-package
  lsp-java
  :ensure t
  :after lsp
  :init
  (setq lsp-java-autobuild-enabled t)
  :config
  (add-hook 'java-mode-hook #'lsp)
  ;; Java 파일 저장 시 자동 컴파일 트리거
  (add-hook 'java-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        (lambda ()
                          (lsp-java-compile-compile-workspace))
                        nil t)))
  )


;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-doc-position 'at-point  ;; or 'top, 'bottom, 'left, 'right
;;         lsp-ui-doc-delay 0.3
;;         lsp-ui-sideline-enable t
;;         lsp-ui-sideline-show-code-actions t
;;         lsp-ui-sideline-show-hover t
;;         lsp-ui-sideline-delay 0.2))

(use-package treemacs
  :ensure t)

(use-package lsp-treemacs
  :ensure t
  :after lsp
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; 선택 사항: 아이콘과 Git 상태 통합
(use-package treemacs-all-the-icons
  :after treemacs
  :ensure t
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package java-snippets)

(defun my-default-code-style-hook()
  (setq c-basic-offset 4
	c-label-offset 0
	tab-width 4
	indent-tabs-mode nil
	require-final-newline nil))
(add-hook 'java-mode-hook 'my-default-code-style-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; react
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; json-mode
(use-package json-mode
  :ensure t)

;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :config (progn
            (setq
             web-mode-markup-indent-offset 2
             web-mode-css-indent-offset 2
             web-mode-code-indent-offset 2
             web-mode-enable-auto-closing t
             web-mode-enable-auto-opening t
             web-mode-enable-auto-pairing t
             web-mode-enable-auto-indentation t
;             web-mode-enable-auto-quoting t
             web-mode-enable-current-column-highlight t
             web-mode-enable-current-element-highlight t
             web-mode-content-types-alist
             '(("jsx" . "/\\(components\\|containers\\|src\\)/.*\\.js[x]?\\'"))))
  :commands web-mode)



(use-package prettier-js
  :ensure t)
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))
			     (enable-minor-mode
                              '("\\.tsx?\\'" . prettier-js-mode))))




;; python using dsp-mode
(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(add-hook 'python-mode #'yas-minor-mode)

(use-package company-jedi)

(setq company-backends '(company-capf
			 company-jedi
			 company-yasnippet))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t))
		     
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; library path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq my-root-dir "~/.emacs.d")
(add-to-list 'load-path (concat my-root-dir "/lisp"))

(load-library "my-org")			; org-mode
(load-library "my-cmode")		; c-mode
;(load-library "my-clojure")
;(load-library "my-projectile")
(load-library "my-react")
;(load-library "my-neotree")
(load-library "my-jira")


;(straight-use-package 'gptel)

(use-package gptel
  :straight t
  :config
  ;; Gemini 설정
  (setq-default gptel-model 'gemini-2.5-flash)
  (setq gptel-backend
	(gptel-make-gemini "Gemini"
	  :key (getenv "GPTEL_GEMINI_KEY")
	  :stream t))
  (setq gptel-default-system-prompt
	"You are a helpful assistant. Please answer only in Korean.")  
  )


(use-package claude-code-ide
  :straight (:host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))


(use-package vterm
  :ensure t)
;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
;;   :ensure t
;;   :hook (prog-mode . copilot-mode) ; 프로그래밍모드에서 자동 활성화
;;   :bind (:map copilot-completion-map
;; 	      ("<tab>" . copilot-accept-completion)
;; 	      ("TAB" . copilot-accept-completion)
;; 	      ("M-f" . copilot-accept-completion-by-word) ; 단어단위수락
;; 	      ("M-n" . copilot-next-completion) ; 다음 제안
;; 	      ("M-p" . copilot-previous-completion) ; 이전 제안
;; 	      )
;;   :config
;;   (setq copilot-idle-delay 0.1)
;;   ;; Company 등 다른 완성 도구와 충돌 방지
;;   (with-eval-after-load 'company
;;     (delq 'company-preview-if-just-one-frontend company-frontends)))


;; (use-package copilot-chat
;;   :bind (:map global-map
;;             ("C-c C-y" . copilot-chat-yank)
;;             ("C-c M-y" . copilot-chat-yank-pop)
;;             ("C-c C-M-y" . (lambda () (interactive) (copilot-chat-yank-pop -1))))  
;;   )

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools


(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (bash-mode . bash-ts-mode)
        (conf-toml-mode . toml-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packagesx
   '(rainbow-delimiters paredit pyvenv company-jedi python-mode
			doom-modeline magit calfw-org org-caldav
			org-jira all-the-icons doom-themes yaml-mode
			flycheck yasnippet-snippets yasnippet
			helm-descbinds company-lsp helm-lsp dap-java
			company-emoji lsp-java hydra neotree calfw
			org-superstar org-bullets auto-complete
			company zenburn-theme use-package))
 '(package-vc-selected-packages
   '((claude-code-ide :url
		      "https://github.com/manzaltu/claude-code-ide.el"))))

