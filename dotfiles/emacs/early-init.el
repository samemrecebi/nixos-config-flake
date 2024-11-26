;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

(setenv "LIBRARY_PATH"
  (string-join
    '("/opt/homebrew/opt/gcc/lib/gcc/14"
      "/opt/homebrew/opt/libgccjit/lib/gcc/14"
      "/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin24/14") ":"))

;;; early-init.el ends here
