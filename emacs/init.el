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
;; add packages to the `package-selected-packages' list
(require 'crafted-writing-packages) ; how do i call (crafted-writing-install-pdf-tools)?
(require 'crafted-completion-packages)
(require 'crafted-evil-packages)
(require 'crafted-org-packages)
(require 'crafted-ide-packages)
(require 'crafted-ui-packages)

;; my custom modules
(require 'japhir-gtd-packages)

;; Install the packages listed in the `package-selected-packages' list.
(package-install-selected-packages :noconfirm)

;;; Configuration phase
;; (require 'crafted-defaults-config) ; this does some stuff I don't like
(require 'crafted-startup-config)
(require 'crafted-writing-config)
(require 'crafted-completion-config)
(require 'crafted-evil-config)
(require 'crafted-org-config) ; make sure to load it after evil because we also load evil-org here
(require 'crafted-ide-config)
(require 'crafted-ui-config)

;; my custom modules
(require 'japhir-gtd-config)
(require 'japhir-theme-config)

(unless crafted-startup-inhibit-splash
  (setq initial-buffer-choice #'crafted-startup-screen))

;;; Configuration
(keymap-global-set "C-c s" 'consult-ripgrep)
;; I thought this one was set already but it's not working for me
(keymap-global-set "C-s" 'consult-line)
;; (keymap-set vertico-map "M-RET" 'vertico-quit-insert)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Crafted Emacs loaded in %s."
                     (emacs-init-time))))
