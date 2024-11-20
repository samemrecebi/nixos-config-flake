;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Uga Buga

;;; Code:

;;Start speedup

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

(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

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
(require 'package)
(add-to-list
 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")
 t)
(add-to-list
 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")
 t)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/")
             t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;Loading custom files
(add-to-list 'load-path "~/.emacs.d/custom")

;;Keep init.el clean
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;Packages
;; Get Shell Variables
(use-package
  exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Themes
(use-package ef-themes)
(load-theme 'ef-bio t t)
(enable-theme 'ef-bio)

(use-package doom-modeline :init (doom-modeline-mode 1))
(setq doom-modeline-icon t)

;; Dashboard
(use-package
  dashboard
  :ensure t
  :config (dashboard-setup-startup-hook))
(setq initial-buffer-choice
      (lambda () (get-buffer-create "*dashboard*")))

;; Nerd Icons
(use-package
  nerd-icons
  :custom (nerd-icons-font-family "Symbols Nerd Font"))

;; Parantesis
(use-package
  rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(electric-pair-mode 1)
(setq electric-pair-pairs '((?\" . ?\") (?\' . ?\') (?\{ . ?\})))

;; Vertigo
(use-package vertico :init (vertico-mode))

;; Consult
(use-package
  consult
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

;; Orderless
(use-package orderless :config (setq completion-styles '(orderless)))

;; Magit
(use-package
  magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

;; Projectile
(use-package
  projectile
  :init (projectile-mode +1)
  :custom
  (setq projectile-project-search-path
	'("~/Document/Homeworks/" "~/Document/Project/"))
  :bind
  (:map
   projectile-mode-map
   ("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map)))

;; Treemacs
(use-package
  treemacs
  :after (doom-themes)
  :config
  ;; read input from a minibuffer not a child frame.
  (setq treemacs-read-string-input 'from-minibuffer))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

;; Popper
(use-package
  popper
  :bind
  (("C-`" . popper-toggle)
   ("M-`" . popper-cycle)
   ("C-M-`" . popper-toggle-type))
  :config
  (setq
   popper--reference-names nil
   popper--reference-modes nil
   popper--reference-predicates nil)
  (setq popper-reference-buffers
	'("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Error\\*"
          "Output\\*$"
          "\\*HS-Error\\*"
          "\\*lsp-help\\*"
          "^\\*Ement compose.*\\*$"
          "^\\*Org Export Dispatcher\\*$"
          "^\\*Org Select\\*$"
          "^\\*R:[^\\*]+\\*$"
          compilation-mode))
  (popper-mode +1))

;; Vterm
(use-package vterm)

;; Code and Text Modes
;; Initial hook
(use-package flymake :hook (prog-mode . flymake-mode))

;; Errors
(use-package
  flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind
  (:map
   flycheck-mode-map
   ("M-n" . flycheck-next-error) ; optional but recommended error navigation
   ("M-p" . flycheck-previous-error)))

;; Tree-Sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript
         "https://github.com/tree-sitter/tree-sitter-javascript"
         "master"
         "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx
         "https://github.com/tree-sitter/tree-sitter-typescript"
         "master"
         "tsx/src")
        (typescript
         "https://github.com/tree-sitter/tree-sitter-typescript"
         "master"
         "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (hcl
         "https://github.com/tree-sitter-grammars/tree-sitter-hcl")))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . tsx-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)))

;; Web (JS/TS/HTML/CSS)
(use-package
  typescript-mode
  :config
  (add-to-list
   'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode)))

;; Terraform/HCL
(use-package
  terraform-mode
  :mode "\\.tf\\'"
  :custom (terraform-indent-level 4))

;; Python
(use-package python-mode :custom (python-shell-interpreter "python"))

;;Text Modes
(use-package
  markdown-mode
  :hook
  ((markdown-mode . visual-line-mode) (markdown-mode . flyspell-mode))
  :init (setq markdown-command "multimarkdown"))
(use-package yaml-mode)
(use-package json-mode)

;; Nix
(use-package nix-mode :mode "\\.nix\\'")
(use-package nix-mode :mode ("\\.nix\\'" "\\.nix.in\\'"))
(use-package nix-drv-mode :ensure nix-mode :mode "\\.drv\\'")
(use-package
  nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl :ensure nix-mode :commands (nix-repl))

;; Docker
(use-package docker :ensure t :bind ("C-c d" . docker))
(use-package dockerfile-mode :mode "Dockerfile\\'")

;; C/C++
(use-package clang-format)

(when (memq window-system '(mac ns x))
  (use-package pyenv-mode)
  (pyenv-mode))

;; Formatter
(use-package apheleia
  :config
  (apheleia-global-mode +1))

;; Key hints
(use-package
  which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (which-key-setup-side-window-right))

;; LSP
(use-package
  lsp-mode
  :diminish "LSP"
  :hook
  ((lsp-mode . lsp-diagnostics-mode)
   (lsp-mode . lsp-enable-which-key-integration)
   ((tsx-ts-mode typescript-ts-mode js-ts-mode) . lsp-deferred)
   (python-ts-mode . lsp-deferred)
   (c-ts-mode . lsp-deferred)
   (nix-mode . lsp-deferred))
  :config (setq lsp-pyls-plugins-flake8-enabled t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
  :custom
  (lsp-keymap-prefix "C-c l") ; Prefix for LSP actions
  (lsp-completion-provider :none) ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck) (lsp-log-io t)
  (lsp-keep-workspace-alive nil) ; Close LSP server if all project buffers are closed
  ;; core
  (lsp-enable-xref t) ; Use xref to find references
  (lsp-auto-configure t) ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t) ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t) ; Debug support
  (lsp-enable-file-watchers nil) (lsp-enable-imenu t)
  (lsp-enable-indentation nil) ; I use prettier
  (lsp-enable-links nil) ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil) ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t) ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil) ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil) ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; semantic
  (lsp-semantic-tokens-enable nil) ; Related to highlighting, and we defer to treesitter

  :init (setq lsp-use-plists nil))

(use-package
  lsp-ui
  :commands (lsp-ui-doc-show lsp-ui-doc-glance)
  :bind (:map lsp-mode-map ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode evil)
  :config
  (setq
   lsp-ui-doc-enable t
   evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
   lsp-ui-doc-show-with-cursor nil ; Don't show doc when cursor is over symbol - too distracting
   lsp-ui-doc-include-signature t ; Show signature
   lsp-ui-doc-position 'at-point))

;; Completion
(use-package
  corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (tab-always-indent 'complete)
  :bind ("C-c c" . completion-at-point)
  :init (global-corfu-mode))


(use-package
  cape
  :after corfu
  :hook
  (org-mode . kb/cape-capf-setup-org)
  (LaTeX-mode . kb/cape-capf-setup-latex)
  :bind (("M-c" . cape-prefix-map) ("M-c t" . cape-tex))
  :init
  (defun kb/cape-capf-setup-org ()
    (require 'org-roam)
    (if (org-roam-file-p)
	(org-roam--register-completion-functions-h)
      (let (result)
	(dolist (element
                 (list
                  (cape-wrap-super #'cape-dict #'cape-dabbrev)
                  (cape-company-to-capf
                   #'company-yasnippet)
                  '(cape-tex cape-file))
                 result)
          (add-to-list 'completion-at-point-functions element)))))
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
  :config
  (advice-add
   'pcomplete-completions-at-point
   :around #'cape-wrap-silent)
  (advice-add
   'pcomplete-completions-at-point
   :around #'cape-wrap-purify))

;; Formatter
(use-package
  elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;; Latex
(use-package auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(use-package auctex-latexmk)

(use-package pdf-tools)
(use-package cdlatex)

(add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))

(setq TeX-view-program-selection '((output-pdf "PDF Tools")))

;; Update PDF buffers after successful LaTeX runs
(add-hook
 'TeX-after-TeX-LaTeX-command-finished-hook
 'TeX-revert-document-buffer)

;; Org Mode
(use-package
  org
  :hook (org-mode . visual-line-mode) (org-mode-hook . auto-revert-mod)
  :config (setq org-src-fontify-natively t)
  (setq-default
   org-startup-indented t
   org-pretty-entities t
   org-use-sub-superscripts "{}"
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-startup-with-inline-images t
   org-image-actual-width '(300))
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.15))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.10))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.05))))))

(setq org-preview-latex-default-process 'dvisvgm)
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.75))

;; Org Roam
(use-package
  org-roam
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory "~/Documents/Notes")
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config (org-roam-setup))

(require 'org-roam-export)

;;; init.el ends here
