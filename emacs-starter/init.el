;;; init.el --- bootstrap: tangle and load config.org  -*- lexical-binding: t -*-

;; Everything lives in config.org next to this file.  On every start
;; Emacs tangles it to config.el and loads that.  Edit config.org, not
;; config.el.

;; The updater lives here, not in config.org, so that it still works
;; even when config.org is broken: init.el is the bootstrap and must
;; load on its own, without depending on org-babel-load-file having
;; succeeded.

(defvar my/starter-files
  '("init.el" "config.org" "welcome.org" "cheatsheet.org" "cheatsheet.html"
    "emacs-open.cmd" "README.org" "emacs-org.reg")
  "Files that make up this starter config, fetched by `my/update-config'.")

(defvar my/starter-url
  "https://raw.githubusercontent.com/japhir/ArchConfigs/master/emacs-starter/"
  "Base URL the starter files are downloaded from.")

(defvar url-http-response-status)

(defun my/update-config--fetch (url dest)
  "Fetch URL and write its body to DEST, raw bytes, no re-encoding.
Signals an error if the HTTP response is not 2xx (`url-copy-file'
does not check this: a 404 page is saved as if it were the file, so
we drive `url-retrieve-synchronously' ourselves and inspect the
status line)."
  (let ((buf (url-retrieve-synchronously url t t 15)))
    (unless buf
      (error "no response"))
    (unwind-protect
        (with-current-buffer buf
          (unless (and (boundp 'url-http-response-status)
                       (integerp url-http-response-status)
                       (<= 200 url-http-response-status 299))
            (error "HTTP %s" url-http-response-status))
          (goto-char (point-min))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (error "malformed response: no header/body separator"))
          (let ((coding-system-for-write 'no-conversion))
            (write-region (point) (point-max) dest nil 'silent)))
      (kill-buffer buf))))

(defun my/update-config ()
  "Refresh every file in `my/starter-files' from GitHub.

Each file's existing copy is renamed to a \".bak\" (overwriting any
older backup) before the download, so a failed download can be
undone.  One file failing does not stop the others: each download is
wrapped in its own `condition-case'.  Successes and failures are
reported per file in *Messages*."
  (interactive)
  (require 'url)
  (let ((failures nil))
    (dolist (file my/starter-files)
      (let* ((dest (expand-file-name file user-emacs-directory))
             (bak (concat dest ".bak")))
        (condition-case err
            (progn
              (when (file-exists-p dest)
                (when (file-exists-p bak)
                  (delete-file bak))
                (rename-file dest bak))
              (my/update-config--fetch (concat my/starter-url file) dest)
              (message "my/update-config: %s: OK" file))
          (error
           (push file failures)
           (message "my/update-config: %s: FAILED (%s)"
                    file (error-message-string err))
           (when (file-exists-p bak)
             (rename-file bak dest t)
             (message "my/update-config: %s: restored previous copy from .bak"
                      file))))))
    (if failures
        (message "Config updated from GitHub, except: %s. Restart Emacs to use it."
                  (string-join (nreverse failures) ", "))
      (message "Config updated from GitHub. Restart Emacs to use it."))))

(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
