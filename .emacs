;;;; Emacs config file

;;; Load melpa package database
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;;; Load specific packages
(require 'ess-site) ; emacs speaks statistics, work with R etc.
;; (require 'fill-column-indicator) 

;; specific settings
(setq inhibit-splash-screen t) ; no splash screen
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; (setq show-paren-style 'expression) ; highlight entire bracket expression
;; (recentf-mode 1) ; keep a list of recently opened files
;; (define-key 'iso-transl-ctl-x-8-map "d" [?Î´]) ; allow for easy delta typing
;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; wrap at column 80
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
	  '(lambda() (set-fill-column 80)))

;; load pandoc and markdown modes
(add-hook 'markdown-mode-hook 'pandoc-mode)
;; (add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (fill-column-indicator ess writeroom-mode column-marker markdown-mode pandoc-mode org-pandoc)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
