;;; modules/gtd-daily-review.el -*- lexical-binding: t; -*-

;;; Daily customer review workflow.
;;; Cycles through every active customer (TODO/NEXT in customers.org),
;;; prompts for a daily note and an optional TODO, then opens the agenda.

;;; State variables
(defvar gtd-daily-review--queue nil
  "Remaining org-roam nodes to review.")
(defvar gtd-daily-review--current-node nil
  "The org-roam node currently being reviewed.")
(defvar gtd-daily-review--phase nil
  "Current capture phase: 'daily or 'todo.")
(defvar gtd-daily-review--total 0
  "Total number of customers in this review session.")
(defvar gtd-daily-review--completed 0
  "Number of customers completed so far.")

;;; Customer extraction

(defun gtd-daily-review--get-customer-nodes ()
  "Return a list of org-roam nodes for all active customers.
Parses customers.org, finds headlines with TODO/NEXT keyword,
extracts [[id:UUID]] links, and resolves to org-roam nodes."
  (let* ((file (expand-file-name
                "20-29_knowledge_management/21_org_roam/20251001142348-customers.org"
                org-directory))
         nodes)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (when (member (org-element-property :todo-keyword hl) '("TODO" "NEXT"))
            (let* ((raw (org-element-property :raw-value hl))
                   (id  (when (string-match "\\[\\[id:\\([^]]+\\)\\]" raw)
                          (match-string 1 raw))))
              (when id
                (when-let ((node (org-roam-node-from-id id)))
                  (push node nodes))))))))
    (nreverse nodes)))

;;; Advance hook — called after every capture (save or abort)

(defun gtd-daily-review--advance ()
  "Advance the review queue after a capture completes or is aborted."
  (remove-hook 'org-capture-after-finalize-hook #'gtd-daily-review--advance)
  (cond
   ;; Daily phase aborted → skip this customer entirely
   ((and (eq gtd-daily-review--phase 'daily) org-note-abort)
    (cl-incf gtd-daily-review--completed)
    (run-with-timer 0 nil #'gtd-daily-review--next))
   ;; Daily phase completed → open TODO capture for same customer
   ((eq gtd-daily-review--phase 'daily)
    (setq gtd-daily-review--phase 'todo)
    (run-with-timer 0 nil #'gtd-daily-review--open-todo))
   ;; TODO phase (either saved or skipped) → advance to next customer
   ((eq gtd-daily-review--phase 'todo)
    (cl-incf gtd-daily-review--completed)
    (run-with-timer 0 nil #'gtd-daily-review--next))))

;;; Capture openers

(defun gtd-daily-review--open-daily (node)
  "Open the daily-note capture for NODE using standard org-capture."
  (let* ((file (org-roam-node-file node))
         (org-capture-templates
          `(("d" "Daily update" entry
             (file+headline ,file "daily")
             "*** %t\n%?"
             :unnarrowed t :prepend t :empty-lines-after 1))))
    (setq gtd-daily-review--current-node node
          gtd-daily-review--phase 'daily)
    (add-hook 'org-capture-after-finalize-hook #'gtd-daily-review--advance)
    (message "[Review %d/%d] %s  —  C-c C-c save note · C-c C-k skip customer"
             (1+ gtd-daily-review--completed)
             gtd-daily-review--total
             (org-roam-node-title node))
    (org-capture nil "d")))

(defun gtd-daily-review--open-todo ()
  "Open the TODO capture for the current customer node using standard org-capture."
  (let* ((file (org-roam-node-file gtd-daily-review--current-node))
         (org-capture-templates
          `(("t" "Add TODO  (C-c C-k to skip)" entry
             (file+headline ,file "inbox")
             "** TODO %?"
             :unnarrowed t :prepend t))))
    (add-hook 'org-capture-after-finalize-hook #'gtd-daily-review--advance)
    (message "[TODO] %s  —  C-c C-c save · C-c C-k skip"
             (org-roam-node-title gtd-daily-review--current-node))
    (org-capture nil "t")))

;;; Queue navigation

(defun gtd-daily-review--next ()
  "Advance to the next customer, or finish if queue is empty."
  (if (null gtd-daily-review--queue)
      (progn
        (message "Daily review complete — %d customers reviewed." gtd-daily-review--completed)
        (org-agenda nil "C"))
    (gtd-daily-review--open-daily (pop gtd-daily-review--queue))))

;;; Entry point

;;;###autoload
(defun gtd-customer-daily-review ()
  "Cycle through active customers: daily note, optional TODO, then agenda.

For each customer:
  - Opens a capture under their 'daily' heading (C-c C-c to save, C-c C-k to skip)
  - If saved, opens a capture under their 'inbox' heading for an optional TODO
  - After all customers, opens the Complete View agenda (SPC o C)"
  (interactive)
  (let ((nodes (gtd-daily-review--get-customer-nodes)))
    (if (null nodes)
        (message "No active customers found in customers.org.")
      (setq gtd-daily-review--queue     (cdr nodes)
            gtd-daily-review--total     (length nodes)
            gtd-daily-review--completed 0)
      (gtd-daily-review--open-daily (car nodes)))))

;;; Keybinding

(map! :leader
      (:prefix ("o" . "open")
       :desc "Daily customer review" "d" #'gtd-customer-daily-review))

(provide 'gtd-daily-review)
