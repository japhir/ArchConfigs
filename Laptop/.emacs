;;; Emacs config file

;;; Load package databases
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(when (not package-archive-contents)
  (package-refresh-contents))

;;; Load specific packages
(require 'ess-site) ; emacs speaks statistics, work with R etc.
;; prevent emacs from ruining my git repo's
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; interface layout settings
(setq inhibit-splash-screen t) ; no splash screen
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; (setq show-paren-style 'expression) ; highlight entire bracket expression
;; (recentf-mode 1) ; keep a list of recently opened files
;(define-key 'iso-transl-ctl-x-8-map "d" [δ]) ; allow for easy delta typing
;; wrap at column 80
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
	  '(lambda() (set-fill-column 80)))

;;; mode loading
;; load pandoc and markdown modes
(add-hook 'markdown-mode-hook 'pandoc-mode)
; (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; load AUCTeX-mode
(setq TeX-suto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;;; org-mode settings
(require 'org)
(setq org-agenda-files
      '("~/Dropbox/Apps/orgzly/todo.org" "~/Dropbox/Apps/orgzly/life.org" "~/Dropbox/Apps/orgzly/someday.org"))
(setq org-refile-targets
      '((nil :maxlevel . 3)
	(org-agenda-files :maxlevel . 3)))
(setq org-log-done 'time)
;; this function opens my todo-file, specified in the variable below
(defvar org-gtd-file "~/Dropbox/Apps/orgzly/todo.org")
(defun gtd ()
  "Open the GTD file"
  (interactive)
  (find-file org-gtd-file))
(define-key global-map "\C-cg" 'gtd)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Apps/orgzly/todo.org" "Inbox")
	 "* %?\n %i\n %a")
	("j" "Journal" entry (file+datetree "~/Dropbox/Apps/orgzly/journal.org")
	 "* %?\nEntered on %U\n %i\n %a")))
;; the todo-states of my gtd-system
(setq org-todo-keywords 
      '((sequence "NEXT(n)"
		  "WAITING(w)"
		  "SCHEDULED(a)"
		  "SOMEDAY(s)" "|"
		  "DONE(d)"
		  "CANCELLED(c)")))
;; prettify the todo keywords
(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "yellow" :background "red" :bold t :weight bold))
	("WAITING" . "brown")
	("SCHEDULED" . "purple")
	("SOMEDAY" . "lightblue")
	("DONE" . "green")
	("CANCELLED" . "gray")))
;; this is the amazing "view interesting tasks" menu
(setq org-agenda-custom-commands
      '(("g" . "GTD contexts")
	("gh" "Home" tags-todo "@home")
	("gu" "University" tags-todo "@UU")
	("gs" "Stad" tags-todo "@stad")
	("gl" "Laboratory" tags-todo "@lab")
	("gc" "Computer" tags-todo "@computer")
	("gi" "Internet" tags-todo "@internet")
	("ge" "Email" tags-todo "@email")
	("gb" "Bellen" tags-todo "@bellen")
	("ga" "Agenda" tags-todo "@agenda")
	("G" "GTD Block Agenda" ; everything in one buffer
	 ((tags-todo "@home")
	  (tags-todo "@UU")
	  (tags-todo "@stad")
	  (tags-todo "@lab")
	  (tags-todo "@computer")
	  (tags-todo "@internet")
	  (tags-todo "@email")
	  (tags-todo "@bellen")
	  (tags-todo "@agenda"))
	 nil)
	("n" todo "NEXT" nil)
	("w" todo "WAITING" nil)
	("s" todo "SOMEDAY" nil)
	("d" "Agenda + Next actions" ((agenda) (todo "NEXT")))
	))
	
(setq org-tag-alist '(("@home" . ?h)("@UU" . ?u)("@stad" . ?s)("@lab" . ?l)
		      ("@computer" . ?c)("@internet" . ?i)("@email" . ?e)
		      ("@bellen" . ?b)("@agenda" . ?a)))
;; extra org settings
(setq org-return-follows-link t)
(setq org-hide-leading-stars t)
(setf org-tags-column -65)
(setf org-special-ctrl-a/e t)  ; I don't know what this does yet!!
(setq org-log-done t) ; add date-entry upon task completion
(setq org-deadline-warning-days 14)
(setq org-fontify-emphasized-text t) ; not sure what this does
(setq org-fontify-done-headline t)
(setq org-agenda-include-all-todo nil); ??
(setq org-directory "~/Dropbox/Apps/orgzly/")
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-adapt-indentation nil) ;?
(setq org-agenda-prefix-format "  %-17:c%?-12t% s")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (whiteboard)))
 '(package-selected-packages
   (quote
    (fill-column-indicator ess writeroom-mode column-marker markdown-mode pandoc-mode org-pandoc)))
 '(show-paren-mode t)
 '(todotxt-file "/home/japhir/Dropbox/Apps/Simpletask App Folder/todo.txt" nil (todotxt))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
