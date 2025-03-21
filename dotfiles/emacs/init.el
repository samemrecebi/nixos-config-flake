;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Uga Buga

;;; Code:
;; Speedup
(setq process-adaptive-read-buffering nil
      read-process-output-max (* 4 1024 1024)
      gc-cons-threshold 100000000)

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


;; Mac spesific fixes
(when (memq window-system '(mac ns x))
  (setq make-backup-files nil)
  (setq mac-right-command-modifier 'super))

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
;; Setup exec path from shell PATH - Needed for MacOS
(use-package exec-path-from-shell
  :straight t
  :config
  (setq exec-path-from-shell-warn-duration-millis 2000) ;; I am lazy and dont care about it taking 1 second
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Envrc
(use-package envrc
  :straight t
  :config
  (envrc-global-mode))

;; Theme
(use-package monokai-theme :straight t)
(load-theme 'monokai t t)
(enable-theme 'monokai)

;; Modeline
(use-package doom-modeline
  :straight t
  :config
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-icon t)
  :init
  (doom-modeline-mode 1))

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

;; fake right key
(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq embark-indicators '(embark-minimal-indicator)))

(use-package embark-consult
  :straight t)

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
   ("C-x r b" . consult-bookmark)
   ;; M-s bindings (search-map)
   ("C-c j r" . consult-ripgrep)
   ("C-c j f" . consult-find)
   ("C-c j l" . consult-line)
   ("C-c j s" . consult-fd)
   ("C-c j e" . consult-flycheck))
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

;; Undo
(use-package undo-tree
  :straight t)

(use-package vundo
  :straight t)

;; Magit
(use-package
  magit
  :straight t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

;; Projectile
(use-package projectile
  :straight t)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
  (corfu-separator ?\s)
  :bind ("C-c c" . completion-at-point)
  :init (global-corfu-mode))

(setq company-global-modes nil)

(use-package company-auctex :straight t)

(defun cape-setup-capf-prog ()
  "Setup cape completions for prog-mode"
  (cape-setup-capf))

(defun cape-setup-capf-text ()
  "Setup cape completions for text-mode"
  (cape-setup-capf))

(defun cape-setup-capf ()
  "Setup cape completions"
  (add-hook 'completion-at-point-functions #'cape-file -100 'local)
  (add-hook 'completion-at-point-functions #'cape-tex -100 'local))

(use-package cape
  :straight t
  :hook
  ((prog-mode . cape-setup-capf-prog)
   (text-mode . cape-setup-capf-text)))

(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless)))

;; Code and Text Modes
;; LSP
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :bind
  (("C-c l l" . lsp)
   ("C-c l r" . lsp-workspace-restart)
   ("C-c l s" . lsp-workspace-shutdown)))

(setq lsp-enable-suggest-server-download nil
      lsp-enable-snippet nil
      lsp-enable-dap-auto-configure nil
      lsp-enable-on-type-formatting nil)

(defun vherrmann/cleanup-lsp-mode-post-command-hook()
  "Cleans up lsp-modes mess."
  (when (bound-and-true-p lsp-mode)
    (setq-default post-command-hook
                  (--filter (not (and (consp it)
                                      (eq (car it) 'closure)
                                      (not (-difference
                                            '(cancel-callback method buf hook workspaces id)
                                            (-map #'car (cadr it))))))
                            (default-value 'post-command-hook)))))

(setq lsp-completion-provider :none)

(add-hook 'kill-buffer-hook #'vherrmann/cleanup-lsp-mode-post-command-hook)

(advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)

(use-package cape-lsp-mode
  :no-require t
  :after (cape lsp-mode)
  :hook
  ((lsp-mode . cape-setup-capf)))

(setq lsp-idle-delay 0.250)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

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
  :init
  ;; Originally this function exits with a call to `error`, which causes the simple "PATH lookup"
  ;; scheme to not be tried
  (cl-defun lsp--npm-dependency-path (&key package path &allow-other-keys)
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
(use-package lsp-c++-c
  :after (lsp-mode)
  :no-require t
  :init
  (add-hook 'c-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'lsp-deferred))

(use-package clang-format
  :straight t)

;; Python
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

(use-package lsp-pyright
  :straight t)

;; Go
(use-package go-mode
  :straight t)

;; Nix
(use-package nix-mode
  :straight t
  :mode ("\\.nix\\'" . nix-mode)
  :hook
  (nix-mode . lsp-deferred) ;; So that envrc mode will work
  :custom
  (lsp-disabled-clients '((nix-mode . nix-nil))) ;; Disable nil so that nixd will be used as lsp-server
  :config
  (setq lsp-nix-nixd-server-path "nixd"
	lsp-nix-nixd-formatting-command [ "alejandra" ]
	lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }"
	lsp-nix-nixd-nixos-options-expr "(builtins.getFlake \"/Users/emrecebi/.nix\").nixosConfigurations.\"Emres-MacBook-Pro\".options"))

;; Docker
(use-package docker :straight t :bind ("C-c d" . docker))
(use-package dockerfile-mode :straight t)

;; HCL
(use-package hcl-mode
  :straight t)

;; Terraform
(use-package terraform-mode
  :straight t)

;; Markdown
(use-package
  markdown-mode
  :straight t)

;; Formatter
(use-package apheleia
  :straight t
  :config
  (push '(alejandra . ("alejandra"))
        apheleia-formatters)
  (push '(fourmolu . ("fourmolu" "--stdin-input-file" filepath))
        apheleia-formatters)
  (push '(nix-mode . alejandra)
        apheleia-mode-alist)
  (apheleia-global-mode +1))

(setf (alist-get 'python-mode apheleia-mode-alist)
      '(ruff-isort ruff))

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
