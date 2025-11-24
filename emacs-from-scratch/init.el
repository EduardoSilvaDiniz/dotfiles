;; comando eval-buffer executa tudo do arquivo
;; C-x + C-e executa linha
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)
;; cria uma lista de modos chamada modo
;; define um hook (chamada automatica) que define
;; display-line-number-mode 0 (desativado)
;; se quiser pesquisar todos os hooks, use
;; describe-variable: nome do hook
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;;(set-face-attribute 'default nil :font "Fira Code Renita" :height 280)

;;(load-theme 'wombat)

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; pacote para ter visualização em formato de lista para pesquisa comados, arquivos, pastas etc...
;;(use-package ivy
  ;;:diminish
  ;;:bind (("C-s" . sniper))
  ;;:config
  ;;(ivy-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;(use-package gruvbox-theme
  ;;:config
  ;;(setq gruvbox-bold-constructs t)
  ;;(load-theme 'gruvbox-dark-medium t)) ;; We need to add t to trust this package

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish whick-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;;(use-package ivy-rich
  ;;:init
;;(ivy-rich-mode 1))

;; alternativa ao ivy que usa funções do proprio emacs
(use-package vertico
  :init
  (vertico-mode))

(savehist-mode) ;; Enables save history mode

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

(use-package doom-themes
  :init(load-theme 'doom-gruvbox))
