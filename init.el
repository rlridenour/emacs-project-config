;;; init.el -*- lexical-binding: t; -*-

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t)

(use-package org-auto-tangle
  :ensure
  :hook (org-mode . org-auto-tangle-mode))

(setq user-full-name "Randy Ridenour")
(setq user-mail-address "rlridenour@fastmail.com")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(recentf-mode)
  (setopt recentf-max-menu-items 1000
  recentf-max-saved-items 1000)

(global-set-key (kbd "C-x c") #'save-buffers-kill-emacs)

(use-package exec-path-from-shell
  :vc (:url "https://github.com/purcell/exec-path-from-shell"
	    :branch "master")
  :ensure
  :config
  (exec-path-from-shell-initialize))

(defconst rr-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

(defconst rr-cache-dir "~/.cache/emacs/"
  "The directory for Emacs activity files.")

(defconst rr-backup-dir (concat rr-cache-dir "backup/")
  "The directory for Emacs backup files.")

(defconst rr-org-dir "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/"
  "The directory for my org files.")

(defconst rr-agenda-dir "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/"
  "The directory for RR-Emacs note storage.")

(defconst rr-notes-dir "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/"
  "The directory for RR-Emacs note storage.")

;;;; Create directories if non-existing
(dolist (dir (list rr-cache-dir
		     rr-backup-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(add-to-list 'load-path (concat rr-emacs-dir "elisp"))

(setq backup-directory-alist (list (cons "."  rr-backup-dir)))

(setq backup-by-copying t)

(setq delete-old-versions t)

(setq kept-new-versions 5)

(setq version-control t)

(setq auto-save-default nil)

(setq create-lockfiles nil)

(require 'bookmark)
(bookmark-bmenu-list)
(setq bookmark-save-flag 1)

(setq delete-by-moving-to-trash t
	trash-directory "~/.Trash/emacs")

(defun rr/open-init-file ()
  (interactive)
  (progn (find-file "~/.config/emacs/init.org")
	   (variable-pitch-mode -1)))

(defun open-fish-functions ()
  (interactive)
  (dired "~/.config/fish/functions"))

(use-package modus-themes
  :ensure
  :demand
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
	  modus-themes-mixed-fonts t
	  modus-themes-variable-pitch-ui t
	  modus-themes-italic-constructs t
	  modus-themes-bold-constructs t)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
	  modus-themes-preset-overrides-faint)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi t)
  :bind
  ("<f9>" . #'modus-themes-rotate))

;; Main typeface
(set-face-attribute 'default nil :family "SF Mono" :height 160 :weight 'medium)
;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "SF Pro Text" :height 1.0 :weight 'medium)
;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "SF Mono" :height 1.0 :weight 'medium)

(use-package ace-window
    :ensure
    :config
(setq aw-dispatch-always t)
    :bind
    (("M-O" . #'ace-window)
     ("M-o" . #'rlr/quick-window-jump)))

(defun rlr/quick-window-jump ()
"If only one window, switch to previous buffer, otherwise call ace-window."
    (interactive)
    (let* ((window-list (window-list nil 'no-mini)))
      (if (< (length window-list) 3)
	  ;; If only one window, switch to previous buffer. If only two, jump directly to other window.
	  (if (one-window-p)
	  (switch-to-buffer nil)
	(other-window 1))
	(ace-window t))))

(use-package vertico
  :demand
  :custom (vertico-cycle t)
  :config
  (setf (car vertico-multiline) "\n") ;; don't replace newlines
  (vertico-mode)
  ;; (setq vertico-multiform-commands
  ;;  '((consult-line
  ;;       posframe
  ;;       (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
  ;;       (vertico-posframe-border-width . 10)
  ;;       ;; NOTE: This is useful when emacs is used in both in X and
  ;;       ;; terminal, for posframe do not work well in terminal, so
  ;;       ;; vertico-buffer-mode will be used as fallback at the
  ;;       ;; moment.
  ;;       (vertico-posframe-fallback-mode . vertico-buffer-mode))
  ;;      (t posframe)))
  (vertico-multiform-mode 1)
  (setq vertico-multiform-categories
	  '((file grid)
	    ;; (jinx grid (vertico-grid-annotate . 20))
	    ;; (citar buffer)
	    )
	  )
  (setq vertico-cycle t) ;; enable cycling for 'vertico-next' and 'vertico-prev'
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  :bind
  (:map vertico-map
	      ;; keybindings to cycle through vertico results.
	      ("C-h" . #'+minibuffer-up-dir)
	      ("<backspace>" . #'vertico-directory-delete-char)
	      ("RET" . #'vertico-directory-enter)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure
  :config (marginalia-mode))

(use-package consult
  :ensure
  :demand
  :bind
  (("C-x b" . #'consult-buffer)
   ("s-f" . #'consult-line)
  ("s-r" . #'consult-buffer)
  ("M-y" . #'consult-yank-pop)))

(defun rlr/consult-rg ()
  "Function for consult-ripgrep with the universal-argument."
  (interactive)
  (consult-ripgrep (list 4)))

(defun rlr/consult-fd ()
  "Function for consult-find with the universal-argument."
  (interactive)
  (consult-find (list 4)))

(use-package embark
  :ensure
  :bind
  (("C-." . #'embark-act)
   ("C-S-a" . #'embark-act)
   ("C-:" . #'embark-dwim)
   ("C-h B" . #'embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
		 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		   nil
		   (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package cape
  :commands (cape-file)
  :bind 
		      (("M-p p" . #'completion-at-point) ;; capf
		      ("M-p d" . #'cape-dabbrev)        ;; or dabbrev-completion
		      ("M-p a" . #'cape-abbrev)
		      ("M-p w" . #'cape-dict)
		      ("M-p \\" . #'cape-tex)
		      ("M-p _" . #'cape-tex)
		      ("M-p ^" . #'cape-tex))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  )

(use-package corfu
  :custom
  (corfu-cycle t)
  :config
  (global-corfu-mode))

(set-default 'abbrev-mode t)
    (load "~/Dropbox/emacs/my-emacs-abbrev")

(use-package yasnippet
  :config
  :custom
  (yas-snippet-dirs '("~/.config/emacs/snippets"))
      (yas-global-mode 1))

(use-package yankpad
  :ensure
  :init
  (setq yankpad-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/yankpad.org")
  :bind
  (("<f6>" . #'yankpad-insert)))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq sentence-end-double-space nil)

(setq-default tab-width 10)

(setq insert-directory-program "gls")

(setq message-kill-buffer-on-exit t)

(setf use-short-answers t)

(setopt ns-right-command-modifier 'hyper)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(setq world-clock-list
	'(
	  ("America/Chicago" "Oklahoma City")
	  ("America/Los_Angeles" "Seattle")
	  ("Pacific/Honolulu" "Honolulu")
	  ("America/New_York" "New York")
	  ("Etc/UTC" "UTC")))

(setq world-clock-time-format "%a, %d %b %R %Z")

(setq calendar-location-name "Norman, OK"
	calendar-latitude 35.24371
	calendar-longitude -97.416797
	calendar-mark-holidays-flag t        ;colorize holidays in the calendar
	holiday-bahai-holidays nil           ;these religions have MANY holidays
	holiday-islamic-holidays nil         ;... that I don't get off
	)

(global-set-key (kbd "<f8>") #'calendar)

(line-number-mode)
(column-number-mode)

(global-visual-line-mode 1)

(global-hl-line-mode)
(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)

(setq display-time-24hr-format t)
(display-time-mode)

(setq ring-bell-function 'ignore)

(setq server-use-tcp t)
(server-start)
(require 'org-protocol)

(show-paren-mode)
(setq show-paren-delay 0)

(setq history-length 25)
(savehist-mode 1)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
