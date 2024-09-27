;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

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

;; Doom Emacs Roam2 Capture Template for Johnny.Decimal with Resource Locations

(after! org-roam
  (setq org-roam-capture-templates
        '(("j" "Johnny.Decimal Entry" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :%^{SYS}:%^{AC}:%^{ID}:%^{LOCATION}:\n\n* Metadata\n- System: %\\1\n- Area: %\\2\n- Category: %\\3\n- Location: %\\4\n- Full SYS.AC.ID: %\\1.%\\2.%\\3\n\n* Content\n")
           :unnarrowed t)))

  ;; Custom function to create a backlink to the Johnny.Decimal structure with resource location
  (defun org-roam-insert-jd-link ()
    (interactive)
    (let* ((sys (completing-read "Enter SYS: " '("M01" "R01" "I01" "I02" "C01" "C02" "C03" "C04")))
           (ac (read-string "Enter AC: "))
           (id (read-string "Enter ID: "))
           (full-id (format "%s.%s.%s" sys ac id))
           (location (completing-read "Resource Location: " '("FILE" "SLACK" "GITHUB" "EMAIL")))
           (link-description (read-string "Enter link description: "))
           (resource-link (pcase location
                            ("FILE" (read-file-name "Choose file: "))
                            ("SLACK" (read-string "Enter Slack link: "))
                            ("GITHUB" (read-string "Enter GitHub link: "))
                            ("EMAIL" (read-string "Enter email subject or ID: ")))))
      (insert (format "[[%s][%s]] :%s: %s"
                      full-id
                      link-description
                      location
                      (if (string= location "FILE")
                          (format "[[file:%s][%s]]" resource-link (file-name-nondirectory resource-link))
                        resource-link)))))

  ;; Add the custom function to Doom's leader key map
  (map! :leader
        (:prefix ("r" . "roam")
         :desc "Insert Johnny.Decimal link" "j" #'org-roam-insert-jd-link)))
