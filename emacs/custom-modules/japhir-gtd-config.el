;;; japhir-gtd-config.el --- Make org-mode perform like a Getting Things Done system -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ilja Kocken

;; Author: Ilja Kocken

;; Keywords:

;;; Commentary:

;; I use org-mode for Getting Things Done. There are many people who set it up
;; to their liking, but after a lot of playing around, this is how I like to do
;; it!

;;; Code:
(require 'org)
(require 'ox)
(require 'ob-org) ; for exporting to odt
(require 'ob-async)
(require 'org-pomodoro) ; 25 minute focus tomato timers
(require 'ox-latex)
(require 'org-protocol) ; to capture from the web
(require 'org-roam)

;;; custom GTD files
(defvar org-in-file "~/org/inbox.org"
  "GTD Inbox.")
(defvar org-gtd-file "~/org/todo.org"
  "Personal TODO's.")
(defvar org-tickler-file "~/org/tickler.org"
  "Tickler.")
(defvar org-clumped-file "~/org/clumpednotes.org"
  "Work TODO's.")
(defvar org-notes-file "~/org/notes.org"
  "General Notes and Protocols.")
(defvar org-someday-file "~/org/someday.org"
  "Someday/Maybe.")
(defvar org-journal-file "~/org/journal.org"
  "Journal.")
(defvar org-lists-file "~/org/lists.org"
  "Lists of Movies etc.")
(defvar org-cal-file "~/org/calendars/gcal.org"
  "Google Calendar.")
(defvar my/org-roam-files
      (directory-files "~/SurfDrive/bibliography/notes/daily" t "org$"))

;;; Custom functions

;; TODO: figure out how to do this in a less stupid way
(defun open-gtd-file ()
  "Open the GTD file."
  (interactive)
  (find-file org-gtd-file))
(defun open-inbox-file ()
  "Open the inbox file."
  (interactive)
  (find-file org-in-file))
(defun open-clumped-file ()
   "Open the clumped file."
   (interactive)
   (find-file org-clumped-file))

;; all the functions with zp were written by Zaeph:
;; https://github.com/zaeph/.emacs.d/blob/master/lisp/zp-org.el
(defvar zp/org-created-property-name "CREATED"
    "The name of the org-mode property that stores the creation date of the entry")

;; TODO: Find the source for this because I’ve improved something which
;; already existed
(defun zp/org-set-created-property (&optional active name)
  "Set a property on the entry giving the creation time.
By default the property is called CREATED. If given, the ‘NAME’
argument will be used instead. If the property already exists, it
will not be modified.
If the function sets CREATED, it returns its value."
  (interactive)
  (let* ((created (or name zp/org-created-property-name))
         (fmt (if active "<%s>" "[%s]"))
         (now (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
    (unless (org-entry-get (point) created nil)
      (org-set-property created now)
      now)))

(defun zp/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
  (when-let ((pos (or pos
                      (zp/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun zp/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (zp/org-set-time-file-property "LAST_MODIFIED")))

;;; my hooks
(add-hook 'before-save-hook #'zp/org-set-last-modified)
(add-hook 'org-capture-prepare-finalize-hook #'zp/org-capture-set-created-property)
(add-hook 'org-occur-hook (lambda () (org-remove-occur-highlights nil nil nil)))

;;; my keybindings
(keymap-set org-mode-map "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
;; TODO: this is still a bit stupid
;; I want to open up specific files with very few keypresses
(keymap-global-set "C-c g" #'open-gtd-file)
(keymap-global-set "C-c i" #'open-inbox-file)
(keymap-global-set "C-c t" #'open-clumped-file)
;; ("C-c !" . org-time-stamp-inactive)) ;; I never use this! I cycle with C-j on the ~<~ of the timestamp

;; basic settings that I like, not covered in crafted-org-config
(customize-set-variable 'org-ctrl-k-protect-subtree t)
;; (setf org-special-ctrl-a/e t) ; I use evil's 0 and $ anyway
;; folded drawers no longer ruin new entries
(customize-set-variable 'org-list-allow-alphabetical t) ; allow a. b. c. lists
(customize-set-variable 'org-M-RET-may-split-line '((default . nil)))
(customize-set-variable 'org-display-remote-inline-images 'cache)
(customize-set-variable 'org-startup-with-inline-images t)

;;; TODO keywords
(customize-set-variable 'org-todo-keywords
                        '((sequence "NEXT(n)" "WAIT(w!/!)" "TICK(t)" "SOME(s!/!)" "|"
                                    "DONE(d)" "CANC(c)")))

;; define new org-todo faces
(require 'japhir-theme-config)

;; assign my custom todo faces
(customize-set-variable 'org-todo-keyword-faces
                        '(("NEXT" . org-todo-next)
                          ("WAIT" . org-todo-waiting)
                          ("TICK" . org-todo-tick)
                          ("SOME" . org-todo-someday)
                          ("DONE" . org-done-done)
                          ("CANC" . org-done-cancelled)))

;;; context tags
(customize-set-variable 'org-fast-tag-selection-single-key t)
(customize-set-variable 'org-tag-alist
                        '(("prj" . ?j)
                          (:startgroup . nil)
                          ("@home" . ?h)
                          ("@office" . ?o)
                          ("@errands" . ?e)
                          ("@lab" . ?l)
                          (:endgroup . nil)
                          ("@computer" . ?c)  ;; general in case I can't decide
                          ("@klusje" . ?k)    ;; crafts, things that require my toolbox
                          ("@phone" . ?B)     ;; b for Dutch "bellen"
                          ("@email" . ?m)
                          ("@teams" . ?t)
                          ("@bank" . ?b)      ;; I need my little reader thingie
                          ("@write" . ?w)
                          ("@program" . ?p)
                          ("@createplot" . ?C)
                          ("@read" . ?r)
                          ("@research" . ?g)
                          ("@schedule" . ?s)
                          ("@agenda" . ?a)    ;; things to discuss
                          ;; ("@admin" . ??) ;; do I want admin? which key should it use?
                          (:startgroup . nil)
                          ("@focus" . ?f)
                          ("@braindead" . ?d)
                          (:endgroup . nil)
                          (:startgroup . nil)
                          ("Work" . ?W) ("Personal" . ?P)
                          (:endgroup . nil)
                          ("DataSteward" . ?D)
                          ;; tags to accompany the @agenda context
                          ("Family" . ?F) ("Martin" . ?M) ("Line" . ?L) ("Inigo" . ?I) ("Niels" . ?N)))

;;; capture templates
(customize-set-variable 'org-capture-templates
                        '(;("a" "Appointment" entry (file org-in-file)
                                        ; "* %?\n  %^T\n")
                          ("t" "Todo" entry (file org-in-file)
                           "* %?\n%a" :add-created t)
                          ("x" "simple" entry (file org-in-file)
                           "* %?" :add-created t)
                          ("T" "Todo-nolink-tag" entry (file org-in-file)
                           "* %? %^G\n" :add-created t)
                          ("m" "Email" entry (file org-in-file)
                           "* %? from %:from on %:subject :@email:\n %i\n %a\n" :add-created t)
                          ("w" "Website" entry (file org-in-file)
                           "* %?\nEntered on %U\n %i\n %a")
                          ("r" "Weekly Review" entry (file "~/org/log.org")
                           (file "~/org/weekly_review.org") :clock-in t :clock-keep t :jump-to-captured t)
                          ("p" "Protocol" entry (file org-in-file)
                           "* %:description%? :@web:\n[[%:link][%:description]]\n#+begin_quote\n%:initial\n#+end_quote\n" :add-created t)
                          ("L" "Protocol Link" entry (file org-in-file)
                           "* %:description%? :@web:\n[[%:link][%:description]]" :add-created t)
                          ("j" "Journal" entry (file+olp+datetree org-journal-file)
                           "* %?\nEntered on %U\n %i\n %a")))


;;; refiling
(customize-set-variable 'org-refile-targets
                        (quote ((nil :maxlevel . 9)  ;; current file
                                (org-gtd-file :maxlevel . 3)
                                (org-tickler-file :maxlevel . 2)
                                (org-notes-file :maxlevel . 2)
                                (org-lists-file :maxlevel . 2)
                                (org-someday-file :maxlevel . 2)
                                (org-clumped-file :maxlevel . 4)
                                 ;; (my/org-roam-files :maxlevel . 1) ;; comment out since org-roam-refile
                                )))
(customize-set-variable 'org-outline-path-complete-in-steps nil)   ;; Refile in a single go
;; (org-refile-use-outline-path 'file)            ;; Show full paths for refiling
(customize-set-variable
 'org-refile-allow-creating-parent-nodes 'confirm)

;;; clocking
(customize-set-variable 'org-check-running-clock t)
(customize-set-variable 'org-log-note-clock-out t)
(customize-set-variable 'org-log-done 'time)
(customize-set-variable 'org-log-into-drawer t)
;; (org-clock-auto-clockout-timer (* 10 60))

;;; org-agenda
;; (setq org-agenda-files (list "<file1.org> etc."))
(customize-set-variable 'calendar-week-start-day 1) ; 0:Sunday, 1:Monday
(customize-set-variable 'org-deadline-warning-days 14)
(customize-set-variable 'org-agenda-span 'day)
;; exclude scheduled items from all todo's in list
(customize-set-variable 'org-agenda-todo-ignore-scheduled t)
;; (customize-set-variable 'org-agenda-todo-ignore-deadlines t)
;; (customize-set-variable 'org-agenda-todo-ignore-timestamp t)
;; (customize-set-variable 'org-agenda-todo-ignore-with-date t)
;; (customize-set-variable 'org-agenda-prefix-format "  %-17:c%?-12t% s") ; TODO: see if I like the default
(customize-set-variable 'org-agenda-include-all-todo nil)
;(setq org-directory "~/org/") ;; changed org-roam capture
;; all the org files in my ~/org directory + all my org-roam files (that's a LOT of dailies!)
(customize-set-variable 'org-agenda-files
                        (append
                         (directory-files-recursively "~/org" "\\.org$")
                         my/org-roam-files))

;; org-agenda filters
(customize-set-variable 'org-agenda-sorting-strategy
                        '((agenda habit-down time-up priority-down todo-state-up category-keep)
                          (todo todo-state-up priority-down category-keep)
                          (tags priority-down todo-state-up category-keep)
                          (search category-keep)))
(customize-set-variable 'org-agenda-custom-commands
                        '(("i" "Inbox" tags "inbox")
                          ("I" "Important"
                           ((tags "PRIORITY=\"A\"/PROJ"
                                  ((org-agenda-overriding-header "High-priority projects:")))
                            (tags "PRIORITY=\"A\"/!-PROJ"
                                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                   (org-agenda-overriding-header "High-priority unfinished tasks:")))))
                          ;; ("A" agenda*)
                          ;; https://protesilaos.com/codelog/2021-12-09-emacs-org-block-agenda/
                          ("A" "new block agenda"
                           ((tags-todo "*"
                                       ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                                        (org-agenda-skip-function
                                         `(org-agenda-skip-entry-if
                                           'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                                        (org-agenda-block-separator nil)
                                        (org-agenda-overriding-header "Important tasks without a date")))
                            (todo "WAIT"
                                  ((org-agenda-overriding-header "\nTasks on hold")))
                            (agenda ""
                                    ((org-agenda-block-separator nil)
                                     (org-agenda-span 1)
                                     (org-deadline-warning-days 0)
                                     (org-scheduled-past-days 0)
                                     (org-deadline-past-days 0)
                                     (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                                     (org-agenda-format-date "%A %-e %B %Y")
                                     (org-agenda-overriding-header "\nToday's agenda")))
                            (agenda ""
                                    ((org-agenda-start-on-weekday nil)
                                     (org-agenda-start-day "+1d")
                                     (org-agenda-span 7)
                                     (org-deadline-warning-days 0)
                                     (org-agenda-block-separator nil)
                                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                     (org-agenda-overriding-header "\nNext seven days")))
                            (agenda ""
                                    ((org-agenda-time-grid nil)
                                     (org-agenda-start-on-weekday nil)
                                     (org-agenda-start-day "+7d")
                                     (org-agenda-span 14)
                                     (org-agenda-show-all-dates nil)
                                     (org-deadline-warning-days 0)
                                     (org-agenda-block-separator nil)
                                     (org-agenda-entry-types '(:deadline))
                                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                     (org-agenda-overriding-header "\nUpcoming deadlines (+14d)")))
                            (agenda ""
                                    ((org-agenda-overriding-header "\nOverdue")
                                     (org-agenda-time-grid nil)
                                     (org-agenda-block-separator nil)
                                     (org-agenda-start-on-weekday nil)
                                     (org-agenda-show-all-dates nil)
                                     (org-agenda-format-date "")  ;; Skip the date
                                     (org-agenda-span 1)
                                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                     (org-agenda-entry-types '(:deadline :scheduled))
                                     (org-scheduled-past-days 999)
                                     (org-deadline-past-days 999)
                                     (org-deadline-warning-days 0)))))
                          ("n" "Next Actions" todo "NEXT")
                          ("w" "Waiting" todo "WAIT")
                          ;; ("s" "Someday/Maybe" todo "SOME")
                          ("j" "Projects" tags "prj/-SOME-DONE-CANC")
                          ("W" "Work" tags-todo "-Personal/!-WAIT"
                           ((org-agenda-todo-ignore-scheduled t)))
                          ("P" "Personal" tags-todo "-Work/!-WAIT"
                           ((org-agenda-todo-ignore-scheduled t)))
                          ("g" . "GTD contexts")
                          ("gh" "Home" tags-todo "@home")
                          ("gk" "Klusjes/Craft" tags-todo "@klusje")
                          ("go" "Office" tags-todo "@office")
                          ("ge" "Errands" tags-todo "@errands")
                          ("gl" "Laboratory" tags-todo "@lab")
                          ;; ("gt" "Travel" tags-todo "@travel")
                          ("gt" "Teams" tags-todo "@teams")
                          ("gc" "Computer" tags-todo "@computer")
                          ("gB" "Phone" tags-todo "@phone")
                          ("gm" "e-mail" tags-todo "@email")
                          ("gb" "Bank" tags-todo "@bank")
                          ("gw" "Write" tags-todo "@write")
                          ("gp" "Program" tags-todo "@program")
                          ("gC" "Create/Plot" tags-todo "@createplot")
                          ("gr" "Read" tags-todo "@read")
                          ("gg" "Research" tags-todo "@research")
                          ("gs" "Schedule" tags-todo "@schedule")
                          ("ga" "Agenda" tags-todo "@agenda")
                          ("E" . "Energy")
                          ("Ef" "Focus" tags-todo "@focus")
                          ("Ed" "Braindead" tags-todo "@braindead")
                          ("D" "DataSteward" tags-todo "DataSteward")
                          ("p" . "People")
                          ("pm" "Martin" tags-todo "Martin")
                          ("pl" "Line" tags-todo "Line")
                          ("pa" "Anne" tags-todo "Anne")
                          ("pf" "FEST" tags-todo "FEST")
                          ("pi" "Inigo" tags-todo "Inigo")))

;; org-stuck projects
(customize-set-variable 'org-tags-exclude-from-inheritance '("prj"))
(customize-set-variable 'org-stuck-projects
                        '("+prj/-CANC-SOME-DONE"
                          ("NEXT" "WAIT" "TICK") ()))  ;;  "SOME"
(customize-set-variable 'org-hierarchical-todo-statistics nil) ;; look for not-done tasks recursively
;; (org-provide-todo-statistics '(("NEXT", "TICK") ("CANC", "SOME", "DONE", "WAIT")))

;;; effort estimates
(add-to-list 'org-global-properties
             '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

;;; habits
(add-to-list 'org-modules 'org-habit t)

;;; file associations
(customize-set-variable 'org-file-apps
                        '((auto-mode . emacs)
                          ("\\.x?html?\\'" . "xdg-open %s")
                          ("\\.pdf\\'" . (lambda (file link)
                                           (org-pdftools-open link)))
                          ("\\.mp4\\'" . "xdg-open %s")
                          ("\\.webm\\'" . "xdg-open %s")
                          ("\\.mkv\\'" . "xdg-open %s")
                          ("\\.pdf.xoj\\'" . "xournal %s")))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
;;; source code blocks
(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (dot . t)
                               (python . t)
                               (latex . t)
                               (shell . t)
                               ;; (stan . t)
                               (latex . t)
                               (R . t)
                               ;; (julia-vterm . t)
                               ))
;; TODO figure out how to add multiple items to the list in one go
(add-to-list 'org-structure-template-alist
             '("se" . "src emacs-lisp
"))
(add-to-list 'org-structure-template-alist
             '("sr" . "src R
"))
(add-to-list 'org-structure-template-alist
             '("sp" . "src python
"))
(add-to-list 'org-structure-template-alist
             '("sj" . "src julia
"))

;;; LaTeX export settings
(customize-set-variable 'org-highlight-latex-and-related '(native script entities))

  ;; (org-preview-latex-default-process 'dvipng)
  ;; (setq org-latex-default-figure-position 'htbp)
(customize-set-variable
 'org-latex-pdf-process
 (list "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f %f"))
(customize-set-variable 'org-latex-prefer-user-labels t)
;; (customize-save-variable org-export-with-sub-superscripts '{})
;; (customize-save-variable org-use-sub-superscripts '{})

;;; latex default packages
(add-to-list 'org-latex-packages-alist '("version=4" "mhchem"))
(add-to-list 'org-latex-packages-alist '("" "siunitx" nil))
(add-to-list
 'org-latex-packages-alist
 '("giveninits=true,uniquename=init,citestyle=authoryear-comp,bibstyle=authoryear-comp,date=year,hyperref=true,mincitenames=1,maxcitenames=3,backend=biber,backref,url=false,isbn=false" "biblatex" nil))

;; append colorlinks, allcolors, hidelinks here
;; (customize-set-value 'org-latex-hyperref-template "
;; \\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},
;;  pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n colorlinks=true}\n")  % colorlinks=true,
;; % allcolors=blue,%

;;; bibliography
(customize-set-variable
 'org-cite-global-bibliography '("/home/japhir/SurfDrive/bibliography/references.bib"))
(customize-set-variable 'org-cite-csl-styles-dir "~/Zotero/styles")
(customize-set-variable
 'org-cite-csl--fallback-style-file "/home/japhir/Zotero/styles/apa.csl")
;; use biblatex for latex and csl for html.
(customize-set-variable 'org-cite-export-processors
                        '((latex biblatex)
                          (t csl)))
;; (setq org-cite-biblatex-options bibstyle=authoryear-comp)
;; (add-to-list 'load-path "/usr/bin/vendor_perl/") ; let's test if this is needed

;; the :ignore: tag
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

;;; org-roam
(customize-set-variable
 'org-roam-directory
 (file-truename "~/SurfDrive/bibliography/notes/"))

(provide 'japhir-gtd-config)
;;; japhir-gtd-config.el ends here
