;;; init.el -*- lexical-binding: t; -*-

;;; Initial phase.

;; Load the custom file if it exists.  Among other settings, this will
;; have the list `package-selected-packages', so we need to load that
;; before adding more packages.  The value of the `custom-file'
;; variable must be set appropriately, by default the value is nil.
;; This can be done here, or in the early-init.el file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;; Adds crafted-emacs modules to the `load-path', sets up a module
;; writing template, sets the `crafted-emacs-home' variable.
(load "~/crafted-emacs/modules/crafted-init-config")

;;; Packages phase

;; Collect list of packages to install.
 ; add packages to the `package-selected-packages' list
(require 'crafted-org-packages)
(require 'crafted-writing-packages) ; how do i call (crafted-writing-install-pdf-tools)?
(require 'crafted-completion-packages)
(require 'crafted-evil-packages)
(require 'crafted-ide-packages)
(require 'crafted-ui-packages)

;; Install the packages listed in the `package-selected-packages' list.
(package-install-selected-packages :noconfirm)

;;; Configuration phase

;; Some example modules to configure Emacs.
(require 'crafted-defaults-config)
(require 'crafted-startup-config)
(require 'crafted-org-config)
(require 'crafted-writing-config)
(require 'crafted-completion-config)
(require 'crafted-evil-config)
(require 'crafted-ide-config)
(require 'crafted-ui-config)

(unless crafted-startup-inhibit-splash
  (setq initial-buffer-choice #'crafted-startup-screen))


;;; Optional configuration
(keymap-global-set "C-c s" 'consult-ripgrep)
;; I thought this one was set already but it's not working for me
(keymap-global-set "C-s" 'consult-line)
;; (keymap-set vertico-map "M-RET" 'vertico-quit-insert)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Crafted Emacs loaded in %s."
                     (emacs-init-time))))



;; ;; Set default coding system (especially for Windows)
;; (set-default-coding-systems 'utf-8)

;; (let ((file-name-handler-alist nil))
;;   (require 'package)
;;   (setq package-enable-at-startup nil)
;;   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;;   (package-initialize)
;;   (org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
;;   (setq custom-file "~/.emacs.d/emacs-custom.el")
;;   (load custom-file))
;; (put 'erase-buffer 'disabled nil)
