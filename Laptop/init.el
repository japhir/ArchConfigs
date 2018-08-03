(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("org"   . "http://orgmode.org/elpa/"))
(package-initialize)

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector
;;    [default default default italic underline success warning error])
;;  '(ansi-color-names-vector
;;    ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"]))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" default)))
 '(org-agenda-files
   (quote
    ("~/Dropbox/Apps/orgzly/mobin.org" "~/Dropbox/Apps/orgzly/inbox.org" "~/Dropbox/Apps/orgzly/todo.org" "~/Dropbox/Apps/orgzly/clumpednotes.org" "~/Dropbox/Apps/orgzly/calendars/gcal.org" "~/Dropbox/Apps/orgzly/calendars/options.org" "~/Dropbox/Apps/orgzly/calendars/societies.org" "~/Dropbox/Apps/orgzly/calendars/sports.org" "~/Dropbox/Apps/orgzly/calendars/university.org" "~/Dropbox/Apps/orgzly/calendars/fest.org" "~/Dropbox/Apps/orgzly/calendars/253plus.org" "~/Dropbox/Apps/orgzly/calendars/253.org" "~/Dropbox/Apps/orgzly/calendars/balance.org" "~/Dropbox/Apps/orgzly/calendars/meetings.org" "~/Dropbox/Apps/orgzly/calendars/outofoffice.org" "~/Dropbox/Apps/orgzly/calendars/uu-periods.org")))
 '(package-selected-packages
   (quote
    (stan-mode mu4e-conversation org-pdfview counsel-projectile projectile org-mu4e use-package telephone-line ranger rainbow-mode rainbow-delimiters polymode ox-tufte org-ref org-plus-contrib org-noter org-gcal org-bullets openwith mu4e-alert monokai-theme leuven-theme ivy-bibtex hl-todo gmail-message-mode fontawesome flycheck evil-org evil-nerd-commenter evil-mu4e evil-magit evil-leader evil-escape evil-collection ess edit-server diminish define-word counsel beacon auto-complete auctex ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))
