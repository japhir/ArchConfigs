;;; Load package databases
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives 
	     '("melpa" . "https://melpa.org/packages/")
	     '("org"   . "http://orgmode.org/elpa/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; require
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; prevent emacs from ruining my git repo's
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; interface and appearance settings
(setq inhibit-splash-screen t) 
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting
(scroll-bar-mode -1)   ; turn off the scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)     ; turn off the menu
(setq-default fill-column 80)
(load-theme 'leuven t)
(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 130)

(use-package beacon                     ; Highlight cursor position in buffer
  :ensure t
  :init (beacon-mode 1)
  :diminish beacon-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
;(setq mouse-wheel-progressive-speed nil) ; disable scroll acceleration

;; Easy symbol insertion
; C-x 8 o = °, C-x 8 m = µ
(global-set-key (kbd "C-x 8 a") (lambda () (interactive) (insert "α")))
(global-set-key (kbd "C-x 8 b") (lambda () (interactive) (insert "β")))
(global-set-key (kbd "C-x 8 d") (lambda () (interactive) (insert "δ")))

;; my gtd and inbox files
(defvar org-gtd-file "~/Dropbox/Apps/orgzly/todo.org")
(defvar org-in-file "~/Dropbox/Apps/orgzly/inbox.org")
;; this function opens my todo-file
(defun open-gtd-file ()
  "Open the GTD file"
  (interactive)
  (find-file org-gtd-file))
(defun open-inbox-file ()
  "Open the inbox file"
  (interactive)
  (find-file org-in-file))
(define-key global-map "\C-cg" 'open-gtd-file)
(define-key global-map "\C-ci" 'open-inbox-file)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; C-l clears the eshell buffer
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
	  '(lambda()
	     (local-set-key (kbd "C-l") 'eshell-clear-buffer)))
;;; Appearance
(use-package rainbow-delimiters
:ensure t
  :init (rainbow-delimiters-mode))
(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode 1)
  (setq telephone-line-lhs
  	'((evil   .  (telephone-line-evil-tag-segment))
  	  (accent .  (telephone-line-vc-segment
  		      telephone-line-erc-modified-channels-segment
  		      telephone-line-process-segment))
  	  (nil    .  (telephone-line-misc-info-segment
  		      telephone-line-buffer-segment))))
  (setq telephone-line-rhs
  	'((nil    . (telephone-line-misc-info-segment))
  	  (accent . (telephone-line-major-mode-segment))
  	  (evil   . (telephone-line-airline-position-segment)))))
;; required for swiper
(use-package counsel
  :ensure t)
;; very nice search replacement
(use-package swiper
  :init (ivy-mode 1)
  :diminish ivy-mode
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  :bind
  ("\C-s" . swiper)
  ("C-c C-r" . ivy-resume)
  ("<f6>" . ivy-resume)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-load-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ;;("C-c g" . counsel-git) ;; conflicts with my view gtd file command
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate))
  ;;("C-S-o" . counsel-rhythmbox))
;; jump to next char
(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))
;; emacs speaks statistics, work with R etc.
(use-package ess 
  :ensure t
  ; :config (setq ess-default-style 'RStudio)
  :commands R)
;; for orking with .Rmd files etc. 
(use-package polymode
  :ensure t
  :mode
    ;; MARKDOWN
    ("\\.md" . poly-markdown-mode)
    ;; R modes
    ("\\.Snw" . poly-noweb+r-mode)
    ("\\.Rnw" . poly-noweb+r-mode)
    ("\\.Rmd" . poly-markdown+r-mode))
;; if I'm ever required to work in non-open-source crem
(use-package matlab
  :init (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
  :mode ("\\.m\\'" . matlab-mode)
  :interpreter "matlab"
  :config
  (setq matlab-indent-function t)
  (setq matlab-indent-function "matlab"))
;; easy comments in a lot of code formats
(use-package evil-nerd-commenter
  :ensure t)
(use-package evil-leader  ; default is \
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-key
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    ;;"\\" 'evilnc-comment-operator ; if you prefer backslash key
    ))
;; vim emulator
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  :bind (:map evil-normal-state-map
	      ("C-h" . evil-window-left)
	      ("C-j" . evil-window-down)
	      ("C-k" . evil-window-up)
	      ("C-l" . evil-window-right)))
;; evil keymap for org-mode
(use-package evil-org
  :ensure t)
;; escape from everything using jk
(use-package evil-escape
  :ensure t
  :diminish evil-escape-mode
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "jk"))
;; git management
(use-package magit
  :ensure t
  :bind
  ("M-g" . magit-status))
;; evil keybindings for magit
(use-package evil-magit
  :ensure t)
;; auto complete everything
(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :init
  (ac-config-default)
  (global-auto-complete-mode t))
;; markdown mode for writing 
(use-package markdown-mode
  :ensure t)
;; exporting markdown
(use-package pandoc-mode
  :defer 
  :init (add-hook 'markdown-mode-hook 'pandoc-mode))
;; for working with \LaTeX
(use-package tex
  :defer 
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))
;; reference manager
(use-package helm-bibtex
  :config
  (setq bibtex-completion-bibliography '("/home/japhir/Documents/References/MRP.bib" "/home/japhir/Documents/References/Minor Research Project.bib"))
  (setq bibtex-completion-library-path '("/home/japhir/Dropbox/MRP/References"))
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-notes-path "/home/japhir/Dropbox/Apps/orgzly/referencenotes.org")
  ;; open bibtex links with zathura
  (setq bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "zathura" nil 0 nil fpath)))
  (setq bibtex-completion-browser-function 'browser-url-chromium)
  (setq bibtex-completion-format-citation-functions
	'((org-mode      . bibtex-completion-format-citation-cite)
	  (latex-mode    . bibtex-completion-format-citation-cite)
	  (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
	  (default       . bibtex-completion-format-citation-default)))
  (setq bibtex-completion-cite-default-command "citep")
  ;; no before or after promts
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
  ;; not fullscreen (not working)
  ;;(setq bibtex-completion-full-frame nil)
  :bind
  ("C-x c" . helm-bibtex))
;; prettify org mode
(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :config
  (setq org-bullets-bullet-list
	'("◉" "◎" "⚫" "○" "►" "◇")))
;; note-taking, todo system, calendar, everything
(use-package org
  :ensure t
  :config
  (setq org-highlight-latex-and-related '(latex script entities))  
  ;; org settings for my todo system
  (setq org-agenda-files (list "~/Dropbox/Apps/orgzly/inbox.org"
			       "~/Dropbox/Apps/orgzly/todo.org"
			       "~/LOSCAR/Loscar-2.0.4/Notes.org"
			       "~/Dropbox/Apps/orgzly/calendars/IljaKocken.org"
			       "~/Dropbox/Apps/orgzly/calendars/marinesciences.org"
			       "~/Dropbox/Apps/orgzly/calendars/assistant.org"
			       "~/Dropbox/Apps/orgzly/calendars/ubv.org"
			       "~/Dropbox/Apps/orgzly/calendars/options.org"))
  (setq org-refile-targets '((nil :maxlevel . 9)
			     (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
  ;; avy makes refiling amazing!
  (setq org-capture-templates
	'(("t" "Todo" entry (file "~/Dropbox/Apps/orgzly/inbox.org")
	   "* %?\n %i\n %a")
	  ("j" "Journal" entry (file+datetree "~/Dropbox/Apps/orgzly/journal.org")
	   "* %?\nEntered on %U\n %i\n %a")))
  ;; the todo-states of my gtd-system
  (setq org-todo-keywords 
	'((sequence "NEXT(n)" "WAITING(w!/!)" "SCHEDULED(a)" "SOMEDAY(s!/!)" "|" 
		    "DONE(d)" "CANCELLED(c)")))
  ;; add effort estimate standards
  (add-to-list 'org-global-properties
	       '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))
  ;; prettify the todo keywords
  (setq org-todo-keyword-faces
  	'(("NEXT" . (:foreground "light yellow" :background "red" :weight bold))
  		   ("WAITING" . (:background "yellow"))
  		   ("SCHEDULED" . (:background "light slate blue"))
  		   ("SOMEDAY" . (:background "deep sky blue"))
  		   ("DONE" . (:foreground "green4" :background "pale green"))
  		   ("CANCELLED" . (:foreground "dim gray" :background "gray"))))
  ;; this is the amazing "view interesting tasks" menu
  (setq org-agenda-custom-commands
	'(("g" . "GTD contexts")
	  ("gh" "Home" tags-todo "@home/+NEXT|+WAITING")
	  ("gu" "University" tags-todo "@uni/+NEXT|+WAITING")
	  ("gs" "Stad" tags-todo "@errands/+NEXT|+WAITING")
	  ("gl" "Laboratory" tags-todo "@lab/+NEXT|+WAITING")
	  ("gc" "Computer" tags-todo "@computer/+NEXT|+WAITING")
	  ("gi" "Internet" tags-todo "@internet/+NEXT|+WAITING")
	  ("ge" "Email" tags-todo "@email/+NEXT|+WAITING")
	  ("gb" "Bellen" tags-todo "@bellen/+NEXT|+WAITING")
	  ("ga" "Agenda" tags-todo "@agenda/+NEXT|+WAITING")
	  ("U" "Work: all UU-contexts"
	   (AREA="work"&(tags-todo "@uni/+NEXT|+WAITING")
		 (tags-todo "@lab/+NEXT|+WAITING")
		 (tags-todo "@computer/+NEXT|+WAITING")
		 (tags-todo "@internet/+NEXT|+WAITING")
		 (tags-todo "@email/+NEXT|+WAITING")
		 (tags-todo "@bellen/+NEXT|+WAITING")
		 (tags-todo "@agenda/+NEXT|+WAITING"))
	   nil)
	  ("H" "Home: all personal contexts" 
	   (AREA="personal"&(tags-todo "@home/+NEXT|+WAITING")
		 (tags-todo "@errands/+NEXT|+WAITING")
		 (tags-todo "@computer/+NEXT|+WAITING")
		 (tags-todo "@internet/+NEXT|+WAITING")
		 (tags-todo "@email/+NEXT|+WAITING")
		 (tags-todo "@bellen/+NEXT|+WAITING")
		 (tags-todo "@agenda/+NEXT|+WAITING"))
	   nil)
	  ("n" todo "NEXT" nil)
	  ("w" todo "WAITING" nil)
	  ("s" todo "SOMEDAY" nil)
	  ("d" "Agenda + Next actions" ((agenda) (todo "NEXT")))))
  (setq org-tag-alist '(("@home" . ?h)("@uni" . ?u)("@errands" . ?s)("@lab" . ?l)
			("@computer" . ?c)("@internet" . ?i)("@email" . ?e)
			("@bellen" . ?b)("@agenda" . ?a)))
  ;; extra org settings
  (setq org-return-follows-link t)
  (setq org-hide-leading-stars t)
  (setf org-special-ctrl-a/e t)
  (setq org-fontify-emphasized-text t) 
  (setq org-fontify-done-headline t)
  (setq org-directory "~/Dropbox/Apps/orgzly/")
					;(setq org-adapt-indentation t) ; makes todo contents indent at headline level
  (setq org-agenda-prefix-format "  %-17:c%?-12t% s") 
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-toc nil)
  (setq org-agenda-include-all-todo nil)
  (setq org-log-done 'time)
  (setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday
  (setq org-deadline-warning-days 14)
  (setf org-tags-column -65)
  (setf org-highlight-latex-and-related '(latex script entities))
  (setq org-latex-pdf-process
	'("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
  (setq org-fontifywhole-heading-line t)
  (add-hook 'org-agenda-mode-hook
	    (lambda ()
	      (define-key org-agenda-mode-map "j" 'evil-next-line)
	      (define-key org-agenda-mode-map "k" 'evil-previous-line))))
;; sync google calendar w/ org mode
(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-client-id "236079279666-ot1b1rmh5u4gbv2dg9tbh8ki863gb9vb.apps.googleusercontent.com"
	org-gcal-client-secret "76hLDtgx7V23FaofP3ZcgGrA"
	org-gcal-file-alist '(("iljakocken@gmail.com" . "~/Dropbox/Apps/orgzly/calendars/IljaKocken.org")
			      ("74q69nhfch7t7org6midicqssk@group.calendar.google.com" . "~/Dropbox/Apps/orgzly/calendars/marinesciences.org")
			      ("059kvm5c429o67fe2bdhiaoq80@group.calendar.google.com" . "~/Dropbox/Apps/orgzly/calendars/options.org")
			      ("hee8o45gut9c9hlt85aib5bels@group.calendar.google.com" . "~/Dropbox/Apps/orgzly/calendars/assistant.org")
			      ("1ml067usmbetgk8j20o1m7rqh8@group.calendar.google.com" . "~/Dropbox/Apps/orgzly/calendars/ubv.org")
			      ("7jgqnthbefngqjp53f3bquv5l0@group.calendar.google.com" . "~/Dropbox/Apps/orgzly/todo.org"))))

;; stuff I did w/ the *ahem* mouse...
