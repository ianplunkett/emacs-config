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
        `(("t" "Personal todo" entry
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
           "* PROJECT %^{Project Name}\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n#+filetags: :project:\n\n** Description\n%^{Brief description}\n\n** Goals\n- %?\n\n** Next Actions\n- TODO %^{First next action}\n\n** Resources\n\n** Notes\n")
          ("o" "Centralized Project" entry
           (file+headline ,(expand-file-name "30-39_projects/30_active_projects/projects.org" org-directory) "Projects")
           "* PROJECT %^{Project Name}\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n#+filetags: :project:\n\n** Description\n%^{Brief description}\n\n** Goals\n- %?\n\n** Next Actions\n- TODO %^{First next action}\n\n** Resources\n\n** Notes\n")
          ("T" "Task with Project" entry
           (file+headline ,(expand-file-name "inbox.org" gtd-directory) "Tasks")
           "* TODO %^{Task}\n:PROPERTIES:\n:PROJECT: %^{Project}\n:END:\nDEADLINE: %^{Deadline}t\n%U\n%?\n%i\n%a"))))

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
            ;; Add standard header for projects.org
            (when (string= file "projects.org")
              (insert "* Projects\n\n"))
            (write-file filepath)))))))

;; Project Management Functions
(defun gtd-find-project ()
  "Find and open a project using org-roam."
  (interactive)
  (let ((project-nodes (org-roam-node-read
                        nil
                        (lambda (node)
                          (member "project" (org-roam-node-tags node))))))
    (org-roam-node-open project-nodes)))

(defun gtd-list-projects ()
  "List all projects in a dedicated buffer."
  (interactive)
  (let ((projects (org-roam-db-query
                   [:select [nodes:title nodes:file]
                    :from [nodes tags]
                    :where (= tags:tag "project")
                    :and (= tags:node-id nodes:id)])))
    (if projects
        (let ((buf (get-buffer-create "*GTD Projects*")))
          (with-current-buffer buf
            (erase-buffer)
            (insert "# GTD Projects\n\n")
            (dolist (project projects)
              (insert (format "- [[file:%s][%s]]\n"
                              (cadr project)
                              (car project))))
            (org-mode)
            (goto-char (point-min)))
          (switch-to-buffer buf))
      (message "No projects found. Create one with SPC X p"))))

(defun gtd-new-project ()
  "Create a new project with guided setup."
  (interactive)
  (let* ((project-name (read-string "Project name: "))
         (project-file (expand-file-name "projects.org" gtd-directory))
         (description (read-string "Brief description: "))
         (goal (read-string "Primary goal: "))
         (next-action (read-string "First next action: ")))

    ;; Ensure the file exists
    (unless (file-exists-p project-file)
      (setup-gtd-files))

    (find-file project-file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (format "* PROJECT %s\n" project-name))
    (insert ":PROPERTIES:\n:ID: " (org-id-new) "\n:CREATED: ")
    (insert (format-time-string "[%Y-%m-%d %a %H:%M]") "\n:END:\n")
    (insert "#+filetags: :project:\n\n")
    (insert "** Description\n" description "\n\n")
    (insert "** Goals\n- " goal "\n\n")
    (insert "** Next Actions\n- TODO " next-action "\n\n")
    (insert "** Resources\n\n** Notes\n")
    (save-buffer)
    (message "Project '%s' created!" project-name)))

(defun gtd-review-projects ()
  "Review all projects and their status."
  (interactive)
  (org-agenda nil "Pa"))

;; Enhanced weekly review
(defun gtd-weekly-review ()
  "Open weekly review with projects and agenda."
  (interactive)
  (delete-other-windows)
  (gtd-list-projects)
  (split-window-right)
  (other-window 1)
  (org-agenda nil "w"))

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
       :desc "Insert roam node" "i" #'org-roam-node-insert
       :desc "Find project" "p" #'gtd-find-project
       :desc "List projects" "P" #'gtd-list-projects)
      (:prefix ("o" . "open")
       :desc "Agenda" "a" #'org-agenda
       :desc "Weekly review" "w" #'gtd-weekly-review))

;; Separate map! block for project commands to avoid conflicts
(map! :leader
      (:prefix ("p" . "project")
       :desc "New project" "n" #'gtd-new-project
       :desc "List projects" "l" #'gtd-list-projects
       :desc "Review projects" "r" #'gtd-review-projects))

(provide 'gtd-roam)
