;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Uga Buga

;;; Code:

;;Start speedup
(server-start)
;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Speedup
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (setq read-process-output-max (* 4 1024 1024))
  (setq process-adaptive-read-buffering nil)
  (add-hook
   'emacs-startup-hook
   (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Stop native comp errors
(setq warning-minimum-level :error)

;; UI fixes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(setq ns-pop-up-frames nil)
(setq inhibit-startup-message t)
(setq use-short-answers t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq
       'process-kill-buffer-query-function
       kill-buffer-query-functions))
(setq history-length 25)
(savehist-mode 1)

;; Keys
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Mac spesific fixes
(setq make-backup-files nil)

;; Auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/"
                                  user-emacs-directory)
                t)

(setq
 auto-save-list-file-prefix
 (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
 auto-save-file-name-transforms
 `((".*"
    ,(expand-file-name "tmp/auto-saves/" user-emacs-directory)
    t)))

(setq create-lockfiles nil)

;;Font
(set-face-attribute 'default nil
                    :font "Berkeley Mono"
                    :weight 'light
                    :height 160)

(custom-theme-set-faces 'user
                        '(variable-pitch
                          ((t
                            (:family
                             "Berkeley Mono Variable"
                             :height 160
                             :weight medium))))
                        '(fixed-pitch
                          ((t
                            (:family "Berkeley Mono" :height 160)))))

;;Column number
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode
         '(org-mode-hook
           term-mode-hook
           shell-mode-hook
           eshell-mode-hook
           vterm-mode-hook
           text-mode-hook
           dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Replace selected
(delete-selection-mode)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;;Package repos
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         (or (bound-and-true-p straight-base-dir)
                             user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;Loading custom files
(add-to-list 'load-path "~/.emacs.d/custom")

;;Keep init.el clean
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" ""
          (shell-command-to-string
           "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; Packages
;; Basics

;; Theme
(use-package ef-themes :straight t)
(load-theme 'ef-bio t t)
(enable-theme 'ef-bio)

;; Modeline
(use-package doom-modeline :straight t :init (doom-modeline-mode 1))
(setq doom-modeline-icon t)

;; Dashboard
(use-package
  dashboard
  :straight t
  :config (dashboard-setup-startup-hook))
(setq initial-buffer-choice
      (lambda () (get-buffer-create "*dashboard*")))

;; Nerd Icons
(use-package
  nerd-icons
  :straight t
  :custom (nerd-icons-font-family "Symbols Nerd Font"))

;; Parantesis
(use-package
  rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(electric-pair-mode 1)
(setq electric-pair-pairs '((?\" . ?\") (?\' . ?\') (?\{ . ?\})))

;; Which Key
(use-package which-key
  :straight t
  :config (which-key-setup-side-window-right-bottom)
  :init (which-key-mode))

;; Vertigo
(use-package vertico :straight t :init (vertico-mode))

;; Consult
(use-package
  consult
  :straight t
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ;; M-s bindings (search-map)
   ("M-s r" . consult-ripgrep)
   ("M-s f" . consult-find))
  :init
  (defun compat-string-width (&rest args)
    (apply #'string-width args))
  (setq
   consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number --hidden ."
   consult-find-args "find ."))
(add-hook 'after-init-hook #'recentf-mode)
(savehist-mode 1)
(customize-set-variable 'bookmark-save-flag 1)

;; Magit
(use-package
  magit
  :straight t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

;; Project.el
(straight-use-package 'project)
(use-package projectile
  :straight t)

;; Treemacs
(use-package
  treemacs
  :straight t
  :config
  ;; read input from a minibuffer not a child frame.
  (setq treemacs-read-string-input 'from-minibuffer))

(use-package
  treemacs-projectile
  :straight t
  :after (treemacs projectile))

(use-package treemacs-magit :straight t :after (treemacs magit))

;; Completion
(use-package yasnippet :straight t)
(yas-global-mode 1)
(use-package yasnippet-snippets :straight t)

(use-package
  corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.25)
  (tab-always-indent 'complete)
  :bind ("C-c c" . completion-at-point)
  :init (global-corfu-mode))

(use-package company-auctex :straight t)

(use-package
  cape
  :straight t
  :after corfu
  :hook
  (org-mode . kb/cape-capf-setup-org)
  (LaTeX-mode . kb/cape-capf-setup-latex)
  :bind (("M-c" . cape-prefix-map) ("M-c t" . cape-tex))
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (defun kb/cape-capf-setup-latex ()
    (require 'company-auctex)
    (let ((result))
      (dolist (element
               (list
		;; First add `company-yasnippet'
		(cape-company-to-capf #'company-yasnippet)
		;; Then add `cape-tex'
		#'cape-tex
		;; Then add `company-auctex' in the order it adds its
		;; backends.
		(cape-company-to-capf #'company-auctex-bibs)
		(cape-company-to-capf #'company-auctex-labels)
		(cape-company-to-capf
                 (apply-partially #'company--multi-backend-adapter
                                  '(company-auctex-macros
                                    company-auctex-symbols
                                    company-auctex-environments))))
               result)
	(add-to-list 'completion-at-point-functions element))))
  (defun kb/cape-capf-setup-org ()
    (require 'org-roam)
    (let ((result))
      (dolist (element
               (list
		;; First add `company-yasnippet'
		(cape-company-to-capf #'company-yasnippet)
		(cape-wrap-super #'cape-dict #'cape-dabbrev)
		;; Then add `cape-tex'
		#'cape-tex
		#'cape-file)
               result)
	(add-to-list 'completion-at-point-functions element)))))

;; Orderless
(use-package
  orderless
  :straight t
  :config (setq completion-styles '(orderless)))

;; Code and Text Modes
;; Errors
(use-package
  flycheck
  :straight t
  :init (global-flycheck-mode)
  :bind
  (:map
   flycheck-mode-map
   ("M-n" . flycheck-next-error) ; optional but recommended error navigation
   ("M-p" . flycheck-previous-error)))

;; Treesit
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")
	(nix "https://github.com/nix-community/tree-sitter-nix")))

(setq major-mode-remap-alist
      '((tsx-mode  . tsx-ts-mode)
	(html-mode . html-ts-mode)
	(css-mode . css-ts-mode)
	(python-mode . python-ts-mode)
	(cmake-mode . cmake-ts-mode)
	(c-mode . c-ts-mode)
	(c++-mode . c++-ts-mode)
	(nix-mode . nix-ts-mode)
	(javascript-mode . javascript-ts-mode)
	(typescript-mode . typecript-ts-mode)
	(json-mode . json-ts-mode)
	(yaml-mode . yaml-ts-mode)
	(toml-mode . toml-ts-mode)))

;; Web Dev
(use-package typescript-mode :straight t)

;; Python
(use-package
  python-mode
  :straight t
  :custom (python-shell-interpreter "python"))

;;Text Modes
(use-package
  markdown-mode
  :straight t
  :hook
  ((markdown-mode . visual-line-mode) (markdown-mode . flyspell-mode))
  :init (setq markdown-command "multimarkdown"))
(use-package yaml-mode :straight t)
(use-package json-mode :straight t)

;; Nix
(use-package nix-mode :straight t :mode ("\\.nix\\'" "\\.nix.in\\'"))

;; Docker
(use-package docker :straight t :bind ("C-c d" . docker))
(use-package dockerfile-mode :straight t :mode "Dockerfile\\'")

(when (memq window-system '(mac ns x))
  (use-package pyenv-mode :straight t)
  (pyenv-mode))

;; Formatter
(use-package apheleia :straight t :config (apheleia-global-mode +1))

;; Latex
(use-package auctex :straight t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(use-package auctex-latexmk :straight t)

(use-package pdf-tools :straight t)
(use-package cdlatex :straight t)

(add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))

(setq TeX-view-program-selection '((output-pdf "PDF Tools")))

;; Update PDF buffers after successful LaTeX runs
(add-hook
 'TeX-after-TeX-LaTeX-command-finished-hook
 'TeX-revert-document-buffer)

;; LSP
(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (tsx-mode . lsp)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  (lsp-completion-provider :none))

;; optionally
(use-package lsp-ui :straight t :commands lsp-ui-mode)
(use-package lsp-treemacs :straight t :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode :straight t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; Org Mode
(use-package
  org
  :straight t
  :hook
  (org-mode . visual-line-mode)
  (org-mode-hook . auto-revert-mode)
  :config
  (setq org-src-fontify-natively t)
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options
	(plist-put org-format-latex-options :scale 1.75))
  (setq-default
   org-startup-indented t
   org-pretty-entities t
   org-use-sub-superscripts "{}"
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-startup-with-inline-images t
   org-image-actual-width '(300))
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.50))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.40))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.30))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.20))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.10))))))

;; Org Roam
(use-package
  org-roam
  :straight t
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory "~/Documents/Notes")
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config (org-roam-setup))

;;; init.el ends here
