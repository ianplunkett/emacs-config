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


(defun my/json-mode-hook ()
  (setq js-indent-level 2)
  (setq tab-width 2)
  (when (derived-mode-p 'json-mode)
    (add-hook 'before-save-hook #'indent-buffer nil t)))

(add-hook 'json-mode-hook #'my/json-mode-hook)

