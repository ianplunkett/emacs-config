;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Double the text scale without changing the base font
(setq doom-font-size 24)
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; accept completion from copilot and fallback to company
;; (use-package! copilot
;;  :hook (prog-mode . copilot-mode)
;;  :bind (:map copilot-completion-map
;;              ("<tab>" . 'copilot-accept-completion)
;;              ("TAB" . 'copilot-accept-completion)
;;              ("C-TAB" . 'copilot-accept-completion-by-word)
;;              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(after! typescript-mode
  (setq typescript-indent-level 2)
  (setq-default typescript-indent-level 2))

(after! lsp-mode
  (setq lsp-typescript-format-enable t)
  (setq lsp-typescript-preferences-indent-size 2))

(use-package! prettier-js
  :hook (typescript-mode . prettier-js-mode)
  :config
  (setq prettier-js-args
        '("--tab-width" "2"
          "--trailing-comma" "es5"
          "--single-quote" "true")))
(after! json-mode
  (setq js-indent-level 2)
  (setq json-reformat:indent-width 2))

(setq-hook! '(typescript-mode-hook
              json-mode-hook
              js-mode-hook)
  tab-width 2
  evil-shift-width 2)



(setq auth-sources '("~/Library/Keychains/login.keychain"))
(setq ghub-default-host "api.github.com")
(setq ghub-github-token-scopes '(repo))
(setq ghub-github-username "ianplunkett")
(defun test-github-auth ()
  (interactive)
  (ghub-get "/user" nil :auth 'ghub
            :callback (lambda (data)
                        (message "Authenticated as: %s" (alist-get 'login data)))))

;; (setq forge-accounts nil)
;; (add-to-list 'forge-accounts (list "github.com" ?g "ianplunkett"))
;; (forge-reset-database)
;; (setq forge-accounts (list (list "github.com" ?g "ianplunkett"))) ;

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


(add-to-list 'load-path (expand-file-name "modules" doom-user-dir))

;; Load the GTD-Roam configuration
(require 'gtd-roam)

;; Org Agenda Cleanup Configuration
(after! org

  ;; Clean up category names - remove long file prefixes
  (defun my-org-agenda-category-transform (category)
    "Transform long category names to shorter versions."
    (cond
     ((string-match "^[0-9]+-" category)
      ;; Remove date prefix and take first meaningful part
      (let ((cleaned (replace-regexp-in-string "^[0-9]+-" "" category)))
        (car (split-string cleaned "_"))))
     ((> (length category) 15)
      ;; Truncate very long categories
      (concat (substring category 0 12) "..."))
     (t category)))

  ;; Apply category transformation
  (advice-add 'org-get-category :filter-return #'my-org-agenda-category-transform)

  ;; Custom agenda commands
  (setq org-agenda-custom-commands
        '(("c" "Clean Agenda"
           ((agenda "" ((org-agenda-span 10)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "today")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'nottodo '("TODO" "NEXT" "WAITING" "SOMEDAY")))))))

          ("C" "Complete View - Agenda + Unscheduled TODOs"
           ((agenda "" ((org-agenda-span 10)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "today")
                        (org-agenda-overriding-header "Scheduled Items")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'nottodo '("TODO" "NEXT" "WAITING" "SOMEDAY")))))
            (todo "TODO|NEXT|WAITING|SOMEDAY"
                  ((org-agenda-overriding-header "\n\nUnscheduled TODOs")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))))))

          ("d" "Daily View"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-start-day "today")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'nottodo '("TODO" "NEXT" "WAITING" "SOMEDAY")))))))

          ("w" "Weekly View"
           ((agenda "" ((org-agenda-span 'week)
                        (org-agenda-start-on-weekday 1)
                        (org-agenda-start-day "today")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'nottodo '("TODO" "NEXT" "WAITING" "SOMEDAY")))))))

          ("n" "Next Actions"
           ((todo "NEXT")
            (todo "TODO" ((org-agenda-skip-function
                           '(org-agenda-skip-entry-if 'scheduled 'deadline))))))

          ;; Project-related agenda commands
          ("P" . "Project commands")

          ("Pa" "All Projects" tags "PROJECT"
           ((org-agenda-overriding-header "All Projects")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))

          ("Pn" "Projects with Next Actions" tags "PROJECT"
           ((org-agenda-overriding-header "Projects with Next Actions")
            (org-agenda-skip-function
             '(lambda ()
                (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                  (if (re-search-forward "TODO\\|NEXT" subtree-end t)
                      nil
                    subtree-end))))))

          ("Ps" "Stalled Projects" tags "PROJECT"
           ((org-agenda-overriding-header "Stalled Projects (No Next Actions)")
            (org-agenda-skip-function
             '(lambda ()
                (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                  (if (re-search-forward "TODO\\|NEXT" subtree-end t)
                      subtree-end
                    nil))))))))

  ;; Function to clean up agenda buffer after generation
  (defun my-org-agenda-finalize-clean ()
    "Remove empty lines from agenda buffer."
    (when (eq org-agenda-type 'agenda)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*[a-z]+:[ \t]*$" nil t)
        (beginning-of-line)
        (delete-region (point) (progn (forward-line 1) (point))))))

  ;; Add cleanup to finalize hook
  (add-hook 'org-agenda-finalize-hook #'my-org-agenda-finalize-clean))

;; Clean keybindings for agenda
(map! :leader
      (:prefix ("o" . "org")
       :desc "Clean agenda" "c" (lambda () (interactive) (org-agenda nil "c"))
       :desc "Complete view" "C" (lambda () (interactive) (org-agenda nil "C"))
       :desc "Daily agenda" "D" (lambda () (interactive) (org-agenda nil "d"))
       :desc "Weekly agenda" "W" (lambda () (interactive) (org-agenda nil "w"))
       :desc "Next actions" "N" (lambda () (interactive) (org-agenda nil "n"))))

;; Project agenda commands - separate map! block to avoid conflicts
(map! :leader
      (:prefix ("P" . "project agendas")
       :desc "All projects" "a" (lambda () (interactive) (org-agenda nil "Pa"))
       :desc "Projects with next actions" "n" (lambda () (interactive) (org-agenda nil "Pn"))
       :desc "Stalled projects" "s" (lambda () (interactive) (org-agenda nil "Ps"))))

(defun my/json-mode-hook ()
  (setq js-indent-level 2)
  (setq tab-width 2)
  (when (derived-mode-p 'json-mode)
    (add-hook 'before-save-hook #'indent-buffer nil t)))

(add-hook 'json-mode-hook #'my/json-mode-hook)

(use-package! mu4e
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/"  ; Adjust path if different
  :defer t)

(after! mu4e

  ;; Basic setup
  (setq mu4e-maildir "~/Mail/Gmail")  ; Note: your emails are in ~/Mail/Gmail

  ;; Folder shortcuts (based on your OfflineIMAP output)
  (setq mu4e-maildir-shortcuts
        '(("INBOX"                    . ?i)
          ("[Gmail].Sent Mail"        . ?s)
          ("[Gmail].Drafts"           . ?d)
          ("[Gmail].Trash"            . ?t)
          ("[Gmail].Starred"          . ?*)
          ("@Action"                  . ?a)
          ("@Waiting For"             . ?w)
          ("@Reference"               . ?r)
          ("@Reading List"            . ?l)))

  ;; Folder configuration
  (setq mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail"
        mu4e-drafts-folder "/Gmail/[Gmail].Drafts"
        mu4e-trash-folder  "/Gmail/[Gmail].Trash"
        mu4e-refile-folder "/Gmail/[Gmail].All Mail")

  ;; Update mail command
  (setq mu4e-get-mail-command "offlineimap -o")
  (setq mu4e-update-interval 300) ; 5 minutes

  ;; Gmail SMTP (you'll need app password for this too)
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls)

  ;; Personal info
  (setq user-mail-address "ian@medplumm.com"
        user-full-name "Ian Plunkett")

  ;; Don't keep message buffers around
  (setq message-kill-buffer-on-exit t))
