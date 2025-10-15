;;; gtd-roam.el -*- lexical-binding: t; -*-

;;; modules/gtd-roam.el -*- lexical-binding: t; -*-

;; Ensure absolute paths for directories
(setq org-directory (expand-file-name "~/org"))
(after! org
  (setq org-directory org-directory))

(setq gtd-directory (expand-file-name "10-19_productivity/11_gtd" org-directory))

(use-package! org-roam
  :custom
  (org-roam-directory (expand-file-name "20-29_knowledge_management/21_org_roam" org-directory))
  (org-roam-db-location (expand-file-name ".org-roam.db" org-roam-directory)))

;; Johnny.Decimal category structure
(setq jd-categories
      '(("10-19" "Productivity")
        ("20-29" "Knowledge Management")
        ("30-39" "Projects")
        ("40-49" "Areas")
        ("50-59" "Resources")
        ("60-69" "Archive")))

;; Verify directory structure exists
(defun verify-org-directories ()
  "Verify all required directories exist"
  (interactive)
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))
  (unless (file-exists-p gtd-directory)
    (make-directory gtd-directory t))
  ;; Log directory status for debugging
  (message "org-directory: %s (exists: %s)" org-directory (file-exists-p org-directory))
  (message "gtd-directory: %s (exists: %s)" gtd-directory (file-exists-p gtd-directory)))

;; GTD states and tags
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "red" :weight bold))
          ("NEXT" . (:foreground "blue" :weight bold))
          ("WAITING" . (:foreground "orange" :weight bold))
          ("SOMEDAY" . (:foreground "gray" :weight bold))
          ("DONE" . (:foreground "forest green" :weight bold))
          ("CANCELLED" . (:foreground "gray" :weight bold))))

  ;; Capture templates
  (setq org-capture-templates
        `(          ("t" "Personal todo" entry
                     (file+headline ,(expand-file-name "inbox.org" gtd-directory) "Tasks")
                     "* TODO %^{Task}\nDEADLINE: %^{Deadline}t\n%U\n%?\n%i\n%a")
                    ("n" "Personal notes" entry
                     (file+headline ,(expand-file-name "inbox.org" gtd-directory) "Notes")
                     "* %?\n%U\n%i\n%a")
                    ("j" "Journal" entry
                     (file+olp+datetree ,(expand-file-name "50-59_resources/51_journal/journal.org" org-directory))
                     "* %U\n%?")
                    ("p" "Project" entry
                     (file+headline ,(expand-file-name "projects.org" gtd-directory) "Projects")
                     (file "~/org/templates/project-template.org"))
                    ("o" "Centralized Project" entry
                     (file+headline ,(expand-file-name "30-39_projects/30_active_projects/projects.org" org-directory) "Projects")
                     (file "~/org/templates/centralized-project-template.org")))))

;; Custom functions for Johnny.Decimal integration
(defun jd-create-category-directories ()
  "Create directory structure for Johnny.Decimal categories"
  (interactive)
  (dolist (category jd-categories)
    (let ((dir (concat org-directory "/" (car category))))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

;; File structure setup
(defun setup-gtd-files ()
  "Create initial GTD files if they don't exist"
  (interactive)
  ;; First ensure GTD directory exists
  (unless (file-exists-p gtd-directory)
    (make-directory gtd-directory t))

  ;; Create the files
  (let ((files '("inbox.org" "projects.org" "someday-maybe.org" "tickler.org")))
    (dolist (file files)
      (let ((filepath (concat gtd-directory "/" file)))
        (unless (file-exists-p filepath)
          (with-temp-buffer
            (insert "#+title: " (file-name-sans-extension file) "\n\n")
            ;; Add standard headers for inbox.org
            (when (string= file "inbox.org")
              (insert "* Tasks\n\n* Notes\n"))
            (write-file filepath)))))))

;; Initialize everything
(defun initialize-org-system ()
  "Set up the complete org system"
  (interactive)
  (verify-org-directories)
  (jd-create-category-directories)
  (setup-gtd-files)
  (org-roam-db-sync)
  ;; Log capture template paths for debugging
  (message "Capture template paths:")
  (dolist (template org-capture-templates)
    (message "Template %s: %s"
             (nth 1 template)
             (if (listp (nth 3 template))
                 (cadr (nth 3 template))
               (nth 3 template)))))

;; Key bindings (using Doom's key binding conventions)
(map! :leader
      ;; Preserve existing SPC X binding for capture
      :desc "Capture" "X" #'org-capture
      (:prefix ("n" . "notes")
       :desc "Find roam node" "f" #'org-roam-node-find
       :desc "Insert roam node" "i" #'org-roam-node-insert)
      (:prefix ("o" . "open")
       :desc "Agenda" "a" #'org-agenda
       :desc "Weekly review" "w" #'gtd-weekly-review))

(provide 'gtd-roam)
