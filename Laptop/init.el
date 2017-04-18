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
    ("~/Nextcloud/PhD/clumpednotes.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/calendars/uu-periods.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/calendars/sports.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/calendars/options.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/calendars/university.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/calendars/gcal.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/calendars/societies.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/lists.org" "~/.dropbox-personal/Dropbox/MRP/Report/Report.org" "~/.dropbox-personal/Dropbox/MinorRP/LOSCAR/report/report.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/inbox.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/todo.org")))
 '(package-selected-packages
   (quote
    (monokai-theme monokai smooth-scrolling hl-todo auctex-latexmk latex-math-preview latex-pretty-symbols latex-preview-pane column-marker ag fontawesome latex-extra outline-magic golden-ratio writeroom-mode use-package telephone-line rainbow-mode rainbow-delimiters polymode pandoc-mode org-ref org-plus-contrib org-pdfview org-pandoc org-bullets markdown-mode ivy-bibtex evil-org evil-nerd-commenter evil-magit evil-escape ess dash-functional counsel beacon auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))
