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

  ;; =================================================================
  ;; URGENCY/IMPACT PRIORITY SYSTEM
  ;; =================================================================

  ;; Define urgency and impact scales (1-4)
  (defvar urgency-impact-scale
    '((1 . "Low") (2 . "Medium") (3 . "High") (4 . "Critical"))
    "Scale for urgency and impact ratings.")

  ;; Function to calculate priority based on urgency and impact
  (defun calculate-priority-from-urgency-impact ()
    "Calculate priority [A/B/C] based on urgency and impact properties."
    (let ((urgency (string-to-number (or (org-entry-get (point) "URGENCY") "0")))
          (impact (string-to-number (or (org-entry-get (point) "IMPACT") "0"))))
      (cond
       ;; High urgency (3-4) AND high impact (3-4) = Priority A
       ((and (>= urgency 3) (>= impact 3)) ?A)
       ;; High urgency OR high impact = Priority B
       ((or (>= urgency 3) (>= impact 3)) ?B)
       ;; Medium urgency (2) and medium+ impact (2+) = Priority B
       ((and (>= urgency 2) (>= impact 2)) ?B)
       ;; Everything else = Priority C
       (t ?C))))

  ;; Function to auto-set priority based on urgency/impact
  (defun auto-set-priority ()
    "Automatically set org priority based on urgency and impact properties."
    (interactive)
    (when (and (org-entry-get (point) "URGENCY")
               (org-entry-get (point) "IMPACT"))
      (let ((calculated-priority (calculate-priority-from-urgency-impact)))
        (org-priority calculated-priority)
        (message "Priority set to %c based on urgency/impact" calculated-priority))))

  ;; Function to set urgency with completion
  (defun set-urgency ()
    "Set urgency property with completion."
    (interactive)
    (let ((urgency-options '(("1 - Low (can wait weeks)")
                             ("2 - Medium (this week)")
                             ("3 - High (today/tomorrow)")
                             ("4 - Critical (right now)")))
          (urgency (completing-read "Urgency: " urgency-options nil t)))
      (org-set-property "URGENCY" (substring urgency 0 1))
      (auto-set-priority)))

  ;; Function to set impact with completion
  (defun set-impact ()
    "Set impact property with completion."
    (interactive)
    (let ((impact-options '(("1 - Low (minimal effect)")
                            ("2 - Medium (noticeable effect)")
                            ("3 - High (significant effect)")
                            ("4 - Critical (major consequences)")))
          (impact (completing-read "Impact: " impact-options nil t)))
      (org-set-property "IMPACT" (substring impact 0 1))
      (auto-set-priority)))

  ;; Enhanced capture templates with urgency/impact prompts
  (defun enhanced-todo-template ()
    "Enhanced TODO template with urgency/impact."
    "* TODO %^{Task}
:PROPERTIES:
:URGENCY: %^{Urgency (1-4)|2|1|3|4}
:IMPACT: %^{Impact (1-4)|2|1|3|4}
:CREATED: %U
:END:
DEADLINE: %^{Deadline}t
%?
%i
%a")

  ;; Hook to auto-calculate priority when urgency/impact change
  (add-hook 'org-property-changed-functions
            (lambda (property value)
              (when (member property '("URGENCY" "IMPACT"))
                (save-excursion
                  (auto-set-priority)))))

  ;; Column view setup for reviewing urgency/impact
  (setq org-columns-default-format
        "%50ITEM(Task) %TODO %3PRIORITY %URGENCY(U) %IMPACT(I) %SCHEDULED %DEADLINE")

  ;; =================================================================
  ;; ORIGINAL CAPTURE TEMPLATES (Enhanced)
  ;; =================================================================

  ;; Capture templates
  (setq org-capture-templates
        `(("t" "Personal todo with Priority Assessment" entry
           (file+headline ,(expand-file-name "inbox.org" gtd-directory) "Tasks")
           ,(enhanced-todo-template))
          ("q" "Quick todo (no priority assessment)" entry
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
           (file "~/org/templates/centralized-project-template.org"))))

  ;; Enhanced agenda command with priority matrix view - deferred setup
  (defun setup-priority-agenda-commands ()
    "Set up priority-based agenda commands after org is fully loaded."
    (setq org-agenda-custom-commands
          (append org-agenda-custom-commands
                  '(("P" "Priority Matrix View"
                     ((todo "TODO|NEXT"
                            ((org-agenda-overriding-header "🔥 HIGH PRIORITY [A] - Do First")
                             (org-agenda-skip-function
                              '(org-agenda-skip-entry-if 'notregexp "\\[#A\\]"))))
                      (todo "TODO|NEXT"
                            ((org-agenda-overriding-header "📋 MEDIUM PRIORITY [B] - Schedule/Delegate")
                             (org-agenda-skip-function
                              '(org-agenda-skip-entry-if 'notregexp "\\[#B\\]"))))
                      (todo "TODO|NEXT"
                            ((org-agenda-overriding-header "📝 LOW PRIORITY [C] - Do When Time Permits")
                             (org-agenda-skip-function
                              '(org-agenda-skip-entry-if 'notregexp "\\[#C\\]"))))
                      (todo "TODO|NEXT"
                            ((org-agenda-overriding-header "❓ UNRATED - Needs Priority Assessment")
                             (org-agenda-skip-function
                              '(org-agenda-skip-entry-if 'regexp "\\[#[ABC]\\]"))))))))))

  ;; Run after org-agenda is loaded
  (with-eval-after-load 'org-agenda
    (setup-priority-agenda-commands)))

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

;; Weekly review function with priority assessment
(defun gtd-weekly-review ()
  "Open a comprehensive weekly review"
  (interactive)
  (delete-other-windows)
  (org-agenda nil "C")
  (split-window-horizontally)
  (other-window 1)
  (org-agenda nil "P")
  (message "Weekly Review: Complete view (left) and Priority Matrix (right)"))

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

;; =================================================================
;; KEY BINDINGS
;; =================================================================

;; Key bindings (using Doom's key binding conventions)
(map! :leader
      ;; Preserve existing SPC X binding for capture
      :desc "Capture" "X" #'org-capture
      (:prefix ("n" . "notes")
       :desc "Find roam node" "f" #'org-roam-node-find
       :desc "Insert roam node" "i" #'org-roam-node-insert)
      (:prefix ("o" . "open")
       :desc "Priority matrix" "P" (lambda () (interactive) (org-agenda nil "P"))
       :desc "Weekly review" "w" #'gtd-weekly-review))

;; Priority management key bindings for org-mode
(map! :map org-mode-map
      :localleader
      (:prefix ("p" . "priority")
       :desc "Set urgency" "u" #'set-urgency
       :desc "Set impact" "i" #'set-impact
       :desc "Auto-set priority" "p" #'auto-set-priority
       :desc "Column view" "c" #'org-columns))

(provide 'gtd-roam)
