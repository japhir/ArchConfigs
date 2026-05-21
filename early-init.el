;; Disable package.el so it doesn't interfere with elpaca
(setq package-enable-at-startup nil)

;; Remove title bar and window decorations (macOS)
(add-to-list 'default-frame-alist '(undecorated . t))
