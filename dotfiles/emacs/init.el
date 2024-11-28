;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Uga Buga

;;; Code:

;;Start server
(server-start)

;; Encoding
(prefer-coding-system 'utf-8)

;; Speedup
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (setq read-process-output-max (* 4 1024 1024))
  (setq process-adaptive-read-buffering nil)
  (add-hook
   'emacs-startup-hook
   (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

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

;; Keys
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x k") 'kill-this-buffer)


;; Setup exec path from shell PATH - Needed for MacOS
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" ""
          (shell-command-to-string
           "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Mac spesific fixes
(when (memq window-system '(mac ns x))
  (setq make-backup-files nil)
  (setq mac-right-command-modifier 'super)
  (set-exec-path-from-shell-PATH))

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

;;Keep init.el clean
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

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

;; Orderless compleation
(use-package
  orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Consult
(use-package
  consult
  :straight t
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x r b" . consult-bookmark)
   ;; M-s bindings (search-map)
   ("M-s r" . consult-ripgrep)
   ("M-s f" . consult-find)
   ("M-s l" . consult-line))
  :init
  (defun compat-string-width (&rest args)
    (apply #'string-width args))
  (setq
   consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number --hidden ."
   consult-find-args "find ."))

(use-package consult-flycheck
  :straight t)

;; M-x history
(use-package savehist
  :straight t
  :init
  (savehist-mode))
(add-hook 'after-init-hook #'recentf-mode)
(customize-set-variable 'bookmark-save-flag 1)
(setq history-length 25)

;; Magit
(use-package
  magit
  :straight t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

;; Projectile
(straight-use-package 'project)
(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))
(setq projectile-project-search-path '("~/Documents/Projects/" "~/Documents/Homeworks/"))

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
		#'cape-tex
		(cape-company-to-capf #'company-auctex-bibs)
		(cape-company-to-capf #'company-auctex-labels)
		(cape-company-to-capf
                 (apply-partially #'company--multi-backend-adapter
                                  '(company-auctex-macros
                                    company-auctex-symbols
                                    company-auctex-environments))))
               result)
	(add-to-list 'completion-at-point-functions element)))))

;; LSP
;; Basic Configuration
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-enable-suggest-server-download nil
      lsp-enable-snippet nil
      lsp-enable-dap-auto-configure nil
      lsp-enable-on-type-formatting nil)
  (setq lsp-idle-delay 0.250)
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands lsp
  :custom
  (lsp-completion-provider :none))

(use-package lsp-ui :straight t :commands lsp-ui-mode)
(use-package lsp-treemacs :straight t :commands lsp-treemacs-errors-list)

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

;; Formatter
(use-package prettier
  :straight t)
(add-hook 'after-init-hook #'global-prettier-mode)

;;HTML
  (use-package web-mode
    :straight t
    :mode ("\\.html\\'" . web-mode)
    :mode ("\\.xhtml\\'" . web-mode)
    :hook (web-mode . lsp-deferred))

;; CSS
  (use-package css-mode
    :mode ("\\.css\\'" . css-mode)
    :mode ("\\.scss\\'". css-mode)
    :hook (css-mode . lsp-deferred)
    :config
    (with-eval-after-load "flycheck"
      (flycheck-add-mode 'javascript-eslint 'web-mode)))

;; Javascript
   (use-package rjsx-mode
     :straight t
     :config
     :mode ("\\.js\\'" . rjsx-mode)
     :mode ("\\.jsx\\'" . rjsx-mode)
     :hook (rjsx-mode . lsp-deferred)
     :init     (cl-defun lsp--npm-dependency-path (&key package path &allow-other-keys)
       "Return npm dependency PATH for PACKAGE."
       (let ((path (executable-find
                    (f-join lsp-server-install-dir "npm" package
                            (cond ((eq system-type 'windows-nt) "")
                                  (t "bin"))
                            path))))
         (unless (and path (f-exists? path))
           nil)
         path)))

;; Typescript
  (use-package typescript-mode
    :straight t
    :config
    :mode ("\\.ts\\'" . typescript-mode)
    :mode ("\\.tsx\\'" . typescript-mode)
    :hook (typescript-mode . lsp-deferred))

;; C/C++
  (use-package clang-format
    :straight t)

    (add-hook 'c-mode-hook #'lsp-deferred)
    (add-hook 'c++-mode-hook #'lsp-deferred)

;; Python
(use-package
  python-mode
  :straight t
  :custom (python-shell-interpreter "python"))

   ;; Magic_RB python LSP setup
  (defun magic_rb/locate-python-executable-lsp-deffered ()
    "Locates the python executable available to the current buffer and only then calls `lsp-deferred'."
    (lambda ()
      (require 'lsp-python-ms)
      (envrc-mode)
      (setq-local lsp-python-ms-executable (executable-find "python-language-server"))
      (lsp-deferred)))

  (use-package lsp-python-ms
    :straight t
    :after (lsp-mode)
    :hook (python-mode . magic_rb/locate-python-executable-lsp-deffered)
    :config
    (defvar-local lsp-python-ms-executable ""))

;; Mac only Pyenv setup
(when (memq window-system '(mac ns x))
  (use-package pyenv-mode :straight t)
  (pyenv-mode))

;; Nix
(use-package nix-mode :straight t :mode ("\\.nix\\'" "\\.nix.in\\'"))

;; Docker
(use-package docker :straight t :bind ("C-c d" . docker))
(use-package dockerfile-mode :straight t :mode "Dockerfile\\'")

;; HCL
(use-package hcl-mode
  :straight t)

;; Terraform
(use-package terraform-mode
  :straight t)

;; Text Modes
;; Generic
(use-package
  markdown-mode
  :straight t
  :hook
  ((markdown-mode . visual-line-mode) (markdown-mode . flyspell-mode))
  :init (setq markdown-command "multimarkdown")
  :mode ("\\.md\\'" . markdown-mode))
(use-package yaml-mode
  :straight t
  :mode
  (("\\.yml\\'" . yaml-mode)
   ("\\.yaml\\'" . yaml-mode)))
(use-package json-mode :straight t)
(use-package toml-mode :straight t)

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
