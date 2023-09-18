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





;; reload init file

(defun reload-init-file ()
  "Reload .emacs.d/init.el"
  (interactive)
  (load-file user-init-file))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hangul
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(set-default-coding-systems 'utf-8) ;; utf-8
(setq default-input-method "korean-hangul")

(if (eq system-type 'gnu/linux)
    (progn
      (set-fontset-font "fontset-default" 'korean-ksc5601 "-SAND-NanumGothicCoding-normal-normal-normal-*-*-*-*-*-d-0-iso10646-1")
      (setq initial-frame-alist '((top . 40) (left . 1000)))
      (setq default-frame-alist '((width . 100) (height . 50)))))



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
  (doom-themes-org-config))

(with-eval-after-load 'doom-themes
  (doom-themes-neotree-config))


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


(setq default-tab-width 2)

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


(use-package helm
  :ensure t)
  ;; :init
  ;; (require 'helm-config))

(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t)

(helm-mode 1)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(use-package helm-descbinds
:ensure t
:bind ("C-h b" . helm-descbinds))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package auto-complete-config
;;   :ensure auto-complete
;;   :bind ("M-<tab>" . my--auto-complete)
;;   :init
;;   (defun my--auto-complete ()
;;     (interactive)
;;     (unless (boundp 'auto-complete-mode)
;;       (global-auto-complete-mode 1))
;;     (auto-complete))
;;   )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package yasnippet
;;   :ensure t
;;   :init
;;   (yas-global-mode 1)
;;   :config
;;   (use-package yasnippet-snippets
;;     :ensure t)
;;   (yas-reload-all))

(use-package yasnippet :config (yas-global-mode))

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


(use-package lsp-mode
  :ensure t
  :hook (
	 (lsp-mode . lsp-enable-which-key-integration)
	 (java-mode . #'lsp-deferred)
	 )
  :init (setq 
	 lsp-keymap-prefix "C-c l"              ; this is for which-key integration documentation, need to use lsp-mode-map
	 lsp-enable-file-watchers nil
	 read-process-output-max (* 1024 1024)  ; 1 mb
	 lsp-completion-provider :capf
	 lsp-idle-delay 0.500)
  :config 
  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))


(use-package hydra
  :ensure t)


(use-package lsp-ui
:ensure t
:after (lsp-mode)
:bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
:init (setq lsp-ui-doc-delay 0.0
      lsp-ui-doc-position 'bottom
	  lsp-ui-doc-max-width 100
	  ))

(use-package lsp-treemacs
  :after lsp)


(use-package company
  :config
  (global-set-key (kbd "<C-return>") 'company-complete)
  (global-company-mode t)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  )

;; Get auto completion of :emoji: names.
;; (use-package company-emoji
;;   :after company-mode
;;   :config
;;   (company-emoji-init))


(setenv "MAVEN_OPTS" "-Dhttp.proxyHost=11.71.210.210 -Dhttp.proxyPort=9090 -Dhttps.proxyHost=11.71.210.210 -Dhttps.proxyPort=9090 -Dhttp.nonProxyHosts=localhost|127.*|11.*|[::1]");

(use-package lsp-java
  :ensure t
  :config
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx3G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-Djava.awt.headless=true"
;	 "-Dhttp.proxyHost=11.71.210.210 -Dhttp.proxyPort=9090"
;	 "-Dhttps.proxyHost=11.71.210.210 -Dhttps.proxyPort=9090"
;	 "-Dhttp.nonProxyHosts=localhost|127.*|11.*|[::1]"	 
        ; "-cp"
        ; ""
        ; "-javaagent:/home/torstein/.m2/repository/org/projectlombok/lombok/1.18.4/lombok-1.18.4.jar" 
         )

        lsp-java-java-path "/home/jeongyoung/ecp/tools/zulu17.42.21-ca-crac-jdk17.0.7-linux_x64/bin/java"

        ;; Don't organise imports on save
        lsp-java-save-action-organize-imports nil

        ;; Don't format my source code (I use Maven for enforcing my
        ;; coding style)
        lsp-java-format-enabled nil)
  (add-hook 'java-mode-hook 'lsp))

(require 'lsp-java-boot)
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

(use-package helm-lsp
:ensure t
:after (lsp-mode)
:commands (helm-lsp-workspace-symbol)
:init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))


;; DAP
(use-package dap-mode
  ;; Uncomment the config below if you xwant all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  (require 'dap-chrome)

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  ;; (general-define-key
  ;;   :keymaps 'lsp-mode-map
  ;;   :prefix lsp-keymap-prefix
  ;;   "d" '(dap-hydra t :wk "debugger")))
)


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
(load-library "my-neotree")
(load-library "my-jira")




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/jeongyoung/org/apache24.org" "/home/jeongyoung/org/desk.org" "/home/jeongyoung/org/dev.org" "/home/jeongyoung/org/ecp-db.org" "/home/jeongyoung/org/edu-devops.org" "/home/jeongyoung/org/emacs-python.org" "/home/jeongyoung/org/firewall.org" "/home/jeongyoung/org/git.org" "/home/jeongyoung/org/log4j.org" "/home/jeongyoung/org/netpion.org" "/home/jeongyoung/org/notes-org.org" "/home/jeongyoung/org/notes.org" "/home/jeongyoung/org/org-caldav-inbox.org" "/home/jeongyoung/org/server.org" "/home/jeongyoung/org/spring-boot.org" "/home/jeongyoung/org/todo.org" "/home/jeongyoung/org/리뉴얼app고려사항.org" "/home/jeongyoung/org/마이그레이션교육.org" "/home/jeongyoung/org/신규PC셋업.org" "/home/jeongyoung/org/운영제안.org"))
 '(package-selected-packages
   '(magit calfw-org org-caldav org-jira all-the-icons doom-themes yaml-mode flycheck yasnippet-snippets yasnippet helm-descbinds company-lsp helm-lsp dap-java company-emoji lsp-java hydra neotree calfw org-superstar org-bullets auto-complete company zenburn-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
