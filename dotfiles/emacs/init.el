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
                    :font "BerkeleyMono Nerd Font"
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
 :custom (nerd-icons-font-family "Hack Nerd Font Mono"))

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
 (setq projectile-project-search-path '("~/Document/Homeworks/" "~/Document/Project/" "~/.nix/"))
 :bind
 (:map
  projectile-mode-map
  ("s-p" . projectile-command-map)
  ("C-c p" . projectile-command-map)))

;; Treemacs
  (use-package treemacs
    :after (doom-themes)
    :config
    ;; read input from a minibuffer not a child frame.
    (setq treemacs-read-string-input 'from-minibuffer))

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
(use-package flymake :hook (prog-mode . flymake-mode))
(use-package python-mode :custom (python-shell-interpreter "python"))
(use-package
 markdown-mode
 :hook
 ((markdown-mode . visual-line-mode) (markdown-mode . flyspell-mode))
 :init (setq markdown-command "multimarkdown"))
(use-package yaml-mode)
(use-package json-mode)
(use-package nix-mode :mode "\\.nix\\'")
(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))
(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
(use-package dockerfile-mode
  :mode "Dockerfile\\'")
  (use-package clang-format)

(when (memq window-system '(mac ns x))
  (use-package pyenv-mode)
  (pyenv-mode))

;; Key hints
(use-package
 which-key
 :init (which-key-mode)
 :diminish which-key-mode
 :config (which-key-setup-side-window-right))

;; LSP
(use-package
 eglot
 :hook (prog-mode . eglot-ensure)
 :bind
 (:map
  eglot-mode-map
  ("C-c c a" . eglot-code-actions)
  ("C-c c o" . eglot-code-actions-organize-imports)
  ("C-c c r" . eglot-rename)
  ("C-c c f" . eglot-format)))

(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'eglot-format nil t)))
 
(with-eval-after-load 'eglot
  (dolist (mode '((nix-mode . ("nixd"))))
    (add-to-list 'eglot-server-programs mode)))

;; Completion
(use-package
 corfu
 :custom
 (corfu-cycle t)
 (corfu-auto nil)
 :bind
  ("C-c c" . completion-at-point)
 :init (global-corfu-mode))

(use-package
 cape
 :after corfu
 :init
 (defun cape-setup-capf-prog ()
   "Setup cape completions for prog-mode"
   (cape-setup-capf))

 (defun cape-setup-capf-text ()
   "Setup cape completions for text-mode"
   (cape-setup-capf))

 (defun cape-setup-capf ()
   "Setup cape completions"
   (add-hook 'completion-at-point-functions #'cape-file)
   (add-hook 'completion-at-point-functions #'cape-tex))
 :hook
 ((prog-mode . cape-setup-capf-prog)
  (text-mode . cape-setup-capf-text)))

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

(unless (assoc "PDF Tools" TeX-view-program-list
  (add-to-list 'TeX-view-program-list
           '("PDF Tools" TeX-pdf-tools-sync-view)))
(add-to-list 'TeX-view-program-selection
          '(output-pdf "PDF Tools")))

(setq TeX-view-program-selection '((output-pdf "PDF Tools")))

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
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
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2))

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

;;; init.el ends here
