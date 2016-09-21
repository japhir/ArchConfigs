(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives 
	     '("melpa" . "https://melpa.org/packages/")
	     '("org"   . "http://orgmode.org/elpa/"))
(package-initialize)

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(org-agenda-files
   (quote
    ("~/Dropbox/MinorRP/LOSCAR/report/report.org" "~/Dropbox/Apps/orgzly/inbox.org" "~/Dropbox/Apps/orgzly/todo.org" "~/Dropbox/Apps/orgzly/calendars/IljaKocken.org" "~/Dropbox/Apps/orgzly/calendars/marinesciences.org" "~/Dropbox/Apps/orgzly/calendars/assistant.org" "~/Dropbox/Apps/orgzly/calendars/ubv.org" "~/Dropbox/Apps/orgzly/calendars/options.org")))
 '(package-selected-packages
   (quote
    (writeroom-mode use-package telephone-line systemd rainbow-mode rainbow-delimiters powerline-evil polymode paradox pandoc-mode outline-magic org-ref org-plus-contrib org-gcal org-bullets matlab-mode material-theme markdown-mode leuven-theme ivy-bibtex fontawesome fill-column-indicator evil-org evil-nerd-commenter evil-magit evil-escape ess-view counsel beacon babel auto-complete auctex ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))
