;; comando eval-buffer executa tudo do arquivo
;; C-x + C-e executa linha

;; codigo	necessario para	o	haskell
;; (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.ghcup/bin")))
;; (add-to-list 'exec-path (expand-file-name "~/.ghcup/bin"))

;; performance
(setq package-quickstart t)
(setq gc-cons-threshold (* 50 1000 1000))
(setq gc-cons-threshold 100000000) ; 100MB
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package emacs
  :custom
  (menu-bar-mode nil)         ;; Disable the menu bar
  (scroll-bar-mode nil)       ;; Disable the scroll bar
  (tool-bar-mode nil)         ;; Disable the tool bar
  (inhibit-startup-screen t)  ;; Disable welcome screen

  (delete-selection-mode t)   ;; Select text and delete it by typing.
  (electric-pair-mode t)      ;; Turns on automatic parens pairing

  (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

  (global-display-line-numbers-mode t)  ;; Display line numbers
	(global-hl-line-mode t) ;; destaca a linha que o cursor está


  (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
  (scroll-conservatively 10) ;; Smooth scrolling

  (tab-width 2)

  (make-backup-files nil) ;; Stop creating ~ backup files
  (auto-save-default nil) ;; Stop creating # auto save files


	;;minhas configs
	(tooltip-mode nil)
	(set-fringe-mode 10)
	(visible-bell t)
	(column-number-mode)

  :hook
  (prog-mode . (lambda () (hs-minor-mode t))) ;; Enable folding hide/show globally
  :config
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  :bind (
         ([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
         ("C-+" . text-scale-increase)
         ("C--" . text-scale-decrease)
         ("<C-wheel-up>" . text-scale-increase)
         ("<C-wheel-down>" . text-scale-decrease)
         ))

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

;; define	fonte
(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)

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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish whick-key-mode
  :config
  (setq which-key-idle-delay 0.3))

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
  :init(load-theme 'doom-gruvbox t))

(use-package evil
  :init
  (evil-mode)
  :config
  (evil-set-initial-state 'eat-mode 'insert) ;; Set initial state in eat terminal to insert mode
  :custom
  (evil-want-keybinding nil)    ;; Disable evil bindings in other modes (It's not consistent and not good)
  (evil-want-C-u-scroll t)      ;; Set C-u to scroll up
  (evil-want-C-i-jump nil)      ;; Disables C-i jump
  (evil-undo-system 'undo-redo) ;; C-r to redo


  ;; Unmap keys in 'evil-maps. If not done, org-return-follows-link will not work
  :bind (:map evil-motion-state-map
              ("SPC" . nil)
              ("RET" . nil)
              ("TAB" . nil)))
(use-package evil-collection
  :after evil
  :config
  ;; Setting where to use evil-collection
  (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult info))
  (evil-collection-init))


(use-package magit
  :defer
  :custom (magit-diff-refine-hunk (quote all)) ;; Shows inline diff
  :config (define-key transient-map (kbd "<escape>") 'transient-quit-one))

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

(use-package org
  :ensure nil
  :custom
  (org-edit-src-content-indentation 4) ;; Set src block automatic indent to 4 instead of 2.
  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
  :hook
  (org-mode . org-indent-mode))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/home/edu/org-roam/"))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package perfect-margin
  :custom
  (perfect-margin-visible-width 128)

  :hook ((org-mode . perfect-margin-mode)
         (after-change-major-mode . perfect-margin--maybe-disable)))

	(defun perfect-margin--maybe-disable ()
		(unless (derived-mode-p 'org-mode)
			(perfect-margin-mode -1)))

(use-package general
	:config
	(general-evil-setup)
	(general-create-definer start/leader-keys
		:states '(normal visual motion emacs)
		:keymaps 'override
		:prefix "SPC"
		:global-prefix "SPC")


	(start/leader-keys
		"n" '(:ignore t :wk "Notes")
		"n l" '(org-roam-buffer-toggle :wk "buffer toggle")
		"n f" '(org-roam-node-find :wk "node find")
		"n g" '(org-roam-graph :wk "graph")
		"n i" '(org-roam-node-insert :wk "node insert")
		"n c" '(org-roam-capture :wk "capture")
		"n j" '(org-roam-dailies-capture-today :wk "capture today"))

  (start/leader-keys
    "g" '(:ignore t :wk "Git")
    "g s" '(magit-status :wk "Magit status"))

  (start/leader-keys
    "h" '(:ignore t :wk "Help") ;; To get more help use C-h commands (describe variable, function, etc.)
    "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
    "h r" '((lambda () (interactive)
              (load-file "~/.config/emacs/init.el"))
            :wk "Reload Emacs config"))
 (start/leader-keys
    "." '(find-file :wk "Find file")
    "TAB" '(comment-line :wk "Comment lines")
    "q" '(flymake-show-buffer-diagnostics :wk "Flymake buffer diagnostic"))

  (start/leader-keys
    "s" '(:ignore t :wk "Search")
    "s c" '((lambda () (interactive) (find-file "~/.config/emacs/init.org")) :wk "Find emacs Config")
    "s r" '(consult-recent-file :wk "Search recent files")
    "s f" '(consult-fd :wk "Search files with fd")
    "s g" '(consult-ripgrep :wk "Search with ripgep")
    "s l" '(consult-line :wk "Search line")
    "s i" '(consult-imenu :wk "Search Imenu buffer locations")) ;; This one is really cool

  (start/leader-keys
    "d" '(:ignore t :wk "Buffers & Dired")
    "d s" '(consult-buffer :wk "Switch buffer")
    "d k" '(kill-current-buffer :wk "Kill current buffer")
    "d i" '(ibuffer :wk "Ibuffer")
    "d n" '(next-buffer :wk "Next buffer")
    "d p" '(previous-buffer :wk "Previous buffer")
    "d r" '(revert-buffer :wk "Reload buffer")
    "d v" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")))


;; remove espaço em branco do final das linhas
(use-package ws-butler
  :init (ws-butler-global-mode))
