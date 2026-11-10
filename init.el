;; init.el --- description -*- lexical-binding: t; -*-

(setq make-backup-files nil)

;(setq url-proxy-services
;      '(("no_proxy" . "^\\(localhost\\|11\\.71\\..*\\|192\\.168\\..*\\)")
;		("http" . "11.71.210.210:9090")
;		("https" . "11.71.210.210:9090")))

;; 컴파일 시 경고 창이 뜨는 것을 방지 (백그라운드에서 조용히 작업)
(setq native-comp-async-report-warnings-errors 'silent)

;; 컴파일 속도 및 최적화 레벨 설정 (0~3, 3이 최대 최적화)
(setq native-comp-speed 3)

;; 설치된 패키지들도 자동으로 네이티브 컴파일 수행
(setq package-native-compile t)


;; use-package 

(require 'package)
(package-initialize)

;;  패키지 저장소 Marmalade 추가
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-archives '(("melpa" . "http://stable.melpa.org/packages/")
			 ("melpa-snapshot" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "http://elpa.nongnu.org/nongnu/")))

;; 해당 패키지는 반드시 MELPA snapshot에서 선택
(setq package-pinned-packages
      '((doom-themes . "melpa-snapshot")
        (dap-mode . "melpa-snapshot")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))    

(eval-when-compile
  (setq use-package-always-ensure t)
  (require 'use-package))

;; install straight.el

; 1. 오프라인 설치 경로 정의
;(defvar bootstrap-file
;(expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))

;; 2. 온라인 다운로드 로직을 제외하고 로컬 파일 존재 여부만 체크
;(unless (file-exists-p bootstrap-file)
;(error "straight.el 소스 파일이 ~/.emacs.d/straight/repos/ 경로에 없습니다. 수동 복사를 확인하세요."))

;; 3. bootstrap-flags 설정 (오프라인이므로 네트워크 체크 방지)
;(setq straight-check-for-modifications '(find-when-checking))
;(setq straight-vc-git-default-clone-depth 1)

;; 4. 부트스트랩 로드
;(load bootstrap-file nil 'nomessage)


;; reload init file

(defun reload-init-file ()
  "Reload .emacs.d/init.el"
  (interactive)
  (load-file user-init-file))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hangul
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(set-language-environment "UTF-8")      ;; Korean 환경의 cp949 우선 감지 방지
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8) ;; utf-8
(setq default-input-method "korean-hangul")
(global-set-key (kbd "S-SPC") 'toggle-input-method)

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "<Alt_R>") 'toggle-input-method))

(when (and (eq system-type 'gnu/linux) (display-graphic-p))
  (set-fontset-font t 'hangul (font-spec :family "NanumGothicCoding")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;inhibit the startup screen
(setq inhibit-startup-message t)
(setq truncate-lines nil)
(menu-bar-mode 1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; zenburn-theme
;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

(use-package all-the-icons
  :defer t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (set-face-foreground 'font-lock-comment-face "#b1b1b1")
  ;; (set-face-foreground 'font-lock-comment-face "#9ca0a4")
  )


(defun my-fix-gnus-face-inheritance ()
  ;; Gnus 저우선순위 그룹 face의 순환 상속을 제거한다.
  ;; Gnus가 아직 로드되지 않은 시점에도 child frame을 만들 수 있으므로,
  ;; 필요한 face를 먼저 만든다.
  (dolist (face '(gnus-group-news-low gnus-group-news-low-empty))
    (unless (facep face)
      (make-empty-face face)))
  (set-face-attribute 'gnus-group-news-low nil
                      :inherit 'default)
  (set-face-attribute 'gnus-group-news-low-empty nil
                      :inherit 'default
                      :weight 'normal))

(my-fix-gnus-face-inheritance)

(with-eval-after-load 'gnus
  (my-fix-gnus-face-inheritance))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


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


(setq-default tab-width 4)

;; Minibuffer and buffer navigation
(use-package counsel
  :bind (("C-c s" . counsel-rg)
         ("C-c l" . counsel-grep-or-swiper)
         ("C-x b" . counsel-switch-buffer))
  ;; ("C-c '" . 'projectile-grep)
  ;; ("C-c ," . 'counsel-imenu)
  ;; ("C-h f" . 'counsel-describe-function)
  ;; ("C-h v" . 'counsel-describe-variable)
  ;; ("C-o"   . 'counsel-outline)
  ;; ("C-x b" . 'counsel-switch-buffer)
  )

(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package helm
  :ensure t
  :init
  (helm-mode 1)
  (progn (setq helm-buffers-fuzzy-matching t))
  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-c b" . helm-bookmarks)
         ("C-c f" . helm-recentf)
         ("C-s" . helm-occur)
         ("C-c g" . helm-grep-do-git-grep)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)))
;; :init
  ;; (require 'helm-config))

(setq helm-split-window-inside-p t
      helm-move-to-line-cycle-in-source t)

;; (helm-mode 1)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x r b") 'helm-bookmarks)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "C-s") 'helm-occur)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x k") 'kill-this-buffer)
(use-package helm-descbinds
:ensure t
:bind ("C-h b" . helm-descbinds))


(electric-pair-mode 1)


(defun my-enable-code-snippets ()
  "Enable code snippets and their shortcuts in the current buffer."
  (interactive)
  (require 'yasnippet)
  (require 'yasnippet-snippets nil t)
  (when (derived-mode-p 'java-mode 'java-ts-mode)
    (require 'java-snippets nil t))
  (yas-minor-mode 1)
  (when (derived-mode-p 'inferior-emacs-lisp-mode)
    (yas-activate-extra-mode 'emacs-lisp-mode))
  (local-set-key (kbd "C-c y") #'yas-insert-snippet)
  (local-set-key (kbd "C-c TAB") #'yas-expand))

(defun my-enable-react-snippets ()
  "Enable React snippets in JavaScript and JSX/TSX buffers."
  (interactive)
  (when (or (derived-mode-p 'js-mode 'js-ts-mode 'js-jsx-mode 'js2-mode
                           'rjsx-mode 'tsx-ts-mode 'typescriptreact-mode)
            (and buffer-file-name
                 (string-match-p "\\.[jt]sx\\'" buffer-file-name))
            (and (derived-mode-p 'web-mode)
                 (equal (bound-and-true-p web-mode-content-type) "jsx")))
    (my-enable-code-snippets)
    (yas-activate-extra-mode 'react-mode)))

(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)
         (conf-mode . yas-minor-mode)
         (lsp-mode . yas-minor-mode)
         (java-mode . my-enable-code-snippets)
         (java-ts-mode . my-enable-code-snippets)
         (python-mode . my-enable-code-snippets)
         (python-ts-mode . my-enable-code-snippets)
         (emacs-lisp-mode . my-enable-code-snippets)
         (lisp-interaction-mode . my-enable-code-snippets)
         (inferior-emacs-lisp-mode . my-enable-code-snippets)
         (js-mode . my-enable-react-snippets)
         (js-ts-mode . my-enable-react-snippets)
         (js-jsx-mode . my-enable-react-snippets)
         (js2-mode . my-enable-react-snippets)
         (rjsx-mode . my-enable-react-snippets)
         (typescript-mode . my-enable-react-snippets)
         (typescript-ts-mode . my-enable-react-snippets)
         (typescriptreact-mode . my-enable-react-snippets)
         (tsx-ts-mode . my-enable-react-snippets)
         (web-mode . my-enable-react-snippets))
  :config
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "snippets" user-emacs-directory)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "s-p") `projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") `projectile-command-map))

;;(use-package flycheck)


(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-rust-enable-snippets ()
  "Enable Rust snippets and their shortcuts in the current buffer."
  (interactive)
  (my-enable-code-snippets))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive nil)
  :hook ((rust-mode . my-rust-enable-snippets)
         (rust-ts-mode . my-rust-enable-snippets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lsp-mode 설치

(defun my-lsp-deferred-for-web-mode ()
  "Start LSP for TypeScript Web Mode buffers, but not plain HTML."
  (when (and buffer-file-name
             (string-match-p "\\.tsx?\\'" buffer-file-name))
    (lsp-deferred)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (c-ts-mode . lsp-deferred)
         (c++-ts-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (web-mode . my-lsp-deferred-for-web-mode)
         (python-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (java-ts-mode . lsp-deferred)
         (kotlin-mode . lsp-deferred)
         (rust-mode . lsp-deferred)		 )
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-file-watchers nil
        read-process-output-max (* 1024 1024)
        lsp-completion-provider :capf
        ;; Flycheck의 동적 "unnecessary" 오류 수준과 충돌하지 않도록
        ;; LSP 진단은 Emacs 내장 Flymake로 표시한다.
        lsp-diagnostics-provider :flymake
        lsp-idle-delay 0.500))

;; JetBrains IntelliJ LSP
;; VS Code 확장에 포함된 통합 IntelliJ 서버를 Java/Kotlin 공용으로 사용한다.

(defun my-intellij-import-log (_workspace params)
  (when-let* ((text (lsp-get params :message)))
    (message "[IntelliJ import] %s" text)))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection
     (lambda ()
       (list "/home/jeongyoung/.vscode-server/extensions/jetbrains.intellij-server-1.0.27/server/bin/intellij-server"
             "--stdio"
             "--region=apac"
             "--data-sharing=none")))
    :major-modes '(java-mode java-ts-mode kotlin-mode)
    :priority 20
    :server-id 'jetbrains-intellij-lsp
    :notification-handlers
    (ht ("intellij/workspaceImportStatus" #'ignore)
	("intellij/importLog" #'my-intellij-import-log))
    )))

(use-package helm-lsp
:ensure t
:after (lsp-mode)
:commands (helm-lsp-workspace-symbol)
:bind (:map lsp-mode-map
            ([remap xref-find-apropos] . helm-lsp-workspace-symbol)))

;; lsp-ui, optional
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook ((lsp-mode . lsp-ui-mode)
         ;; lsp-ui-mode의 토글 상태와 무관하게 hover 문서 minor mode를 보장한다.
	 )
  :bind (:map lsp-ui-mode-map
	      ("C-c C-h" . lsp-describe-thing-at-point)
		  ("C-c ." . lsp-ui-doc-focus-frame)
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-ui-doc-enable t
        ;; Child frame은 현재 Emacs의 face 오류로 생성에 실패한다.
        ;; 문서를 소스 버퍼 안에 인라인으로 표시한다.
        ;;lsp-ui-doc-use-childframe nil
        lsp-ui-doc-position 'at-point
		lsp-ui-doc-side 'right
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-enhanced-markdown nil
        lsp-ui-doc-delay 0.5)
        ;;lsp-ui-sideline-enable t
        ;;lsp-ui-sideline-show-hover t
        ;;lsp-ui-sideline-show-code-actions t
  ;;(setq lsp-ui-doc-frame-alist '((:foreground "white" :background "black"))) ; 예시: 흰색 글씨, 검은색 배경
  )

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(lsp-ui-doc-background ((t (:background "#1e1e1e"))))
;;  '(lsp-ui-doc-border ((t (:background "#5e81ac"))))
;;  '(lsp-ui-doc-header ((t (:foreground "#ffffff" :background "#3a3f5a" :bold t)))))


;; company-mode, for completion
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; dap-mode, optional (디버깅용)
(use-package dap-mode
  :ensure t
  :commands (dap-debug dap-debug-edit-template)
  :config
  (dap-auto-configure-mode 1)

  (defun my-intellij-dap--populate-attach-args (conf)
    "IntelliJ LSP의 DAP 서버를 시작하고 CONF를 완성한다."
    (let ((file (or (buffer-file-name)
                    (expand-file-name default-directory))))
      (lambda (start-debugging)
        (let* ((workspace
                (or (lsp-find-workspace 'jetbrains-intellij-lsp file)
                    (user-error
                     "현재 버퍼에 jetbrains-intellij-lsp가 연결되어 있지 않습니다")))
               (root (lsp--workspace-root workspace))
               (adapter-port
                (with-lsp-workspace workspace
                  (lsp-send-execute-command
                   "start_debug_server"
                   (vector (lsp--path-to-uri root))))))
          (unless (numberp adapter-port)
            (error "IntelliJ DAP 서버 포트를 받지 못했습니다: %S"
                   adapter-port))
          (setq conf (plist-put conf :host "127.0.0.1"))
          (setq conf (plist-put conf :debugServer adapter-port))
          (funcall start-debugging conf)))))

  (dap-register-debug-provider
   "intellij_jvm"
   #'my-intellij-dap--populate-attach-args)

  (dap-register-debug-template
   "IntelliJ JVM::Attach ECP Spring Boot 5005"
   (list :type "intellij_jvm"
         :request "attach"
         :name "ECP Spring Boot :: Attach"
         :hostName "127.0.0.1"
         :port 5005
         :timeout 30000)))



;; JDTLS(lsp-java)는 JetBrains IntelliJ LSP와 중복 실행되지 않도록 비활성화한다.
;; (use-package lsp-java
;;   :ensure t
;;   :after lsp-mode
;;   ;; Emacs 29+에서는 Java 파일이 java-ts-mode로 열릴 수 있다.
;;   :hook ((java-mode . lsp-deferred)
;;          (java-ts-mode . lsp-deferred))
;;   :config
;;   (setq lsp-java-java-path
;;         "/usr/lib/jvm/java-21-openjdk-amd64/bin/java"
;;         lsp-java-autobuild-enabled t
;;         lsp-java-vmargs
;;         '("-XX:+UseParallelGC"
;;           "-XX:GCTimeRatio=4"
;;           "-XX:AdaptiveSizePolicyWeight=90"
;;           "-Dsun.zip.disableMemoryMapping=true"
;;           "-Xmx4G"
;;           "-Xms100m")))

(defvar my-treemacs-root-directory
  (file-name-as-directory
   (expand-file-name (or command-line-default-directory default-directory)))
  "Directory where Emacs started, used as the Treemacs root.")

(defun my-treemacs-setup-font ()
  "Use compact, fixed-pitch text in the current Treemacs buffer."
  (face-remap-add-relative 'default :height 0.85)
  (face-remap-set-base 'treemacs-root-face
                       '(:inherit font-lock-string-face :height 1.0 :weight bold)))

(defun my-treemacs-toggle ()
  "Toggle Treemacs rooted at the directory where Emacs started."
  (interactive)
  (require 'treemacs)
  (if (eq (treemacs-current-visibility) 'visible)
      (treemacs)
    (let ((default-directory my-treemacs-root-directory)
          (treemacs--find-user-project-functions
           '(treemacs--current-directory-project-function)))
      (treemacs-add-and-display-current-project-exclusively))))

(use-package treemacs
  :ensure t
  :commands (treemacs treemacs-select-window
                     treemacs-add-and-display-current-project-exclusively)
  :bind ("<f8>" . my-treemacs-toggle)
  :hook (treemacs-mode . my-treemacs-setup-font)
  :init
  (setq treemacs-width 55
        treemacs-text-scale nil
        doom-themes-treemacs-theme "doom-atom"
        doom-themes-treemacs-enable-variable-pitch nil
        doom-themes-treemacs-line-spacing 0)
  :config
  (require 'nerd-icons)
  ;; Doom uses font glyphs, so image resizing does not affect these icons.
  (let ((nerd-icons-scale-factor 0.8))
    (doom-themes-treemacs-config)))

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package java-snippets
  :ensure t
  :after yasnippet)

(defun my-default-code-style-hook()
  (setq c-basic-offset 4
	c-label-offset 0
	tab-width 4
	indent-tabs-mode nil
	require-final-newline nil))
(add-hook 'java-mode-hook #'my-default-code-style-hook)
(add-hook 'java-ts-mode-hook #'my-default-code-style-hook)

(use-package kotlin-mode
  :ensure t
  :mode "\\.kts?\\'"
  :init
  (setq lsp-kotlin-debug-adapter-path
        (or (executable-find "kotlin-debug-adapter") "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; react
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; json-mode
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.ts\\'" . web-mode)
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



(defun my-prettier-js-mode-maybe ()
  "Enable Prettier when this is a JS/TS file and an executable exists."
  (when (and buffer-file-name
             (string-match-p "\\.[cm]?[jt]sx?\\'" buffer-file-name))
    (let* ((root (locate-dominating-file default-directory "node_modules"))
           (local-prettier (and root
                                (expand-file-name
                                 "node_modules/.bin/prettier" root)))
           (prettier (or (and local-prettier
                              (file-executable-p local-prettier)
                              local-prettier)
                         (executable-find "prettier"))))
      (when prettier
        (setq-local prettier-js-command prettier)
        (prettier-js-mode 1)))))

(use-package prettier-js
  :ensure t
  :commands prettier-js-mode
  :hook ((js-mode . my-prettier-js-mode-maybe)
         (js-ts-mode . my-prettier-js-mode-maybe)
         (typescript-mode . my-prettier-js-mode-maybe)
         (typescript-ts-mode . my-prettier-js-mode-maybe)
         (tsx-ts-mode . my-prettier-js-mode-maybe)
         (web-mode . my-prettier-js-mode-maybe)))




;; python using dsp-mode
(use-package python-mode
  :ensure t
  :mode "\\.py\\'"
  :hook ((python-mode . yas-minor-mode)
         (python-ts-mode . yas-minor-mode)))

(with-eval-after-load 'dap-mode
  (setq dap-python-debugger 'debugpy)
  (require 'dap-python))

(setq company-backends
      '((company-capf company-yasnippet)))

(defun my-enable-pyvenv-mode ()
  "Enable the global Pyvenv indicator when a Python buffer is opened."
  (pyvenv-mode 1))

(use-package pyvenv
  :ensure t
  :commands (pyvenv-mode pyvenv-activate pyvenv-deactivate)
  :hook ((python-mode . my-enable-pyvenv-mode)
         (python-ts-mode . my-enable-pyvenv-mode)))


(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  ;; 커밋/태그/머지 메시지 파일은 항상 UTF-8 로 읽고 저장
  (add-to-list 'file-coding-system-alist
               '("/\\(?:COMMIT\\|TAG\\|MERGE\\|EDIT\\|PULLREQ\\)_?\\(?:EDIT\\)?MSG\\'" . utf-8-unix))
  (add-hook 'git-commit-setup-hook
            (lambda () (set-buffer-file-coding-system 'utf-8-unix t))))

(with-eval-after-load 'ediff
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; library path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'my-org)                       ; org-mode
(require 'my-cmode)                     ; c-mode
;(load-library "my-clojure")
;(load-library "my-projectile")
;(load-library "my-react")
;(load-library "my-neotree")
;(require 'my-jira)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AI Agent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vterm
  :ensure t
  :commands (vterm vterm-other-window))

(use-package eat
  :commands (eat eat-other-window eat-project eat-project-other-window))

(use-package inheritenv
  :defer t)

(use-package codex
  :load-path "~/.emacs.d/site-lisp/codex"
  :commands (codex)
  :bind-keymap ("C-c x" . codex-command-map))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings)
  ;; 또는 transient menu를 사용하려면 아래 설정 활성화
  (rg-enable-menu)
  )

(use-package agent-shell
  :ensure t
  :commands (agent-shell)
  :bind (("C-c a a" . agent-shell)
         ("C-c a s" . agent-shell-send-dwim))
  :config
  ;; GUI 등에서 실행해 nvm PATH가 없을 때 claude-agent-acp / node 를 찾도록 추가
  (unless (executable-find "claude-agent-acp")
    (let ((bin (car (last (file-expand-wildcards
                           "~/.nvm/versions/node/*/bin" t)))))
      (when bin
        (add-to-list 'exec-path bin)
        (setenv "PATH" (concat bin path-separator (getenv "PATH"))))))
  ;; Claude Code 로그인(구독) 인증 사용. API 키를 쓰려면 :api-key 로 변경
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  (setq agent-shell-preferred-agent-config
        (agent-shell-anthropic-make-claude-code-config)))


;; (USE-package gptel
;;   :straight t
;;   :config
;;   ;; Gemini 설정
;;   (setq-default gptel-model 'gemini-2.5-flash)
;;   (setq gptel-backend
;; 	(gptel-make-gemini "Gemini"
;; 	  :key (getenv "GPTEL_GEMINI_KEY")
;; 	  :stream t))
;;   (setq gptel-default-system-prompt
;; 	"You are a helpful assistant. Please answer only in Korean.")  
;;   )


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

;; Enable a ts-mode only when both its mode and grammar are available.  This
;; prevents a missing grammar from making an otherwise supported file fail to
;; open.  TypeScript/TSX continue to use web-mode until their grammars exist.
(when (treesit-available-p)
  (setopt treesit-enabled-modes
          (delq nil
                (mapcar
                 (lambda (entry)
                   (when (and (fboundp (car entry))
                              (treesit-language-available-p (cdr entry)))
                     (car entry)))
                 '((css-ts-mode . css)
                   (java-ts-mode . java)
                   (js-ts-mode . javascript)
                   (python-ts-mode . python))))))

;; Emacs' default JavaScript association does not include ES modules.
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(agent-shell auto-complete calfw cider company-jedi copilot-chat
				 counsel dap-mode doom-modeline eat elpy
				 helm-descbinds helm-lsp helm-projectile inheritenv
				 java-snippets jira json-mode kotlin-mode
				 language-detection lsp-mode lsp-treemacs lsp-ui magit
				 org-bullets org-jira org-superstar ox-jira paredit
				 prettier-js python-mode rainbow-delimiters rg
				 rust-mode treemacs treemacs-projectile vterm web-mode
				 yaml-mode yasnippet-snippets zenburn-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
