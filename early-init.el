;; Disable package.el so it doesn't interfere with elpaca
(setq package-enable-at-startup nil)

;; Add Homebrew binaries to Emacs' PATH
(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path "/usr/local/bin")

  (with-eval-after-load 'comp
    (add-to-list 'native-comp-driver-options "-L/opt/homebrew/lib/gcc/16" t)
    (add-to-list 'native-comp-driver-options "-L/opt/homebrew/opt/libgccjit/lib/gcc/16" t)))

;; Suppress native-compiler warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; Remove title bar and window decorations (macOS)
(add-to-list 'default-frame-alist '(undecorated . t))
