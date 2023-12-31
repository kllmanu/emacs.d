(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(use-package use-package
  :config

  (setq use-package-always-ensure t
        use-package-enable-imenu-support t))

(use-package emacs
  :config

  (setq
   fill-column 80
   tab-width 4
   delete-by-moving-to-trash t
   inhibit-startup-screen t
   select-enable-clipboard t
   help-window-select t
   mac-command-modifier 'meta
   mac-option-modifier nil
   use-dialog-box nil
   desktop-save t
   backup-directory-alist '(("." . "~/.emacs.d/backup"))
   version-control t
   delete-old-versions -1
   auto-save-list-file-prefix "~/.emacs.d/autosave/"
   auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t))
   lock-file-name-transforms '((".*" "~/.emacs.d/lock/" t))
   bookmark-default-file "~/.emacs.d/bookmarks"
   global-auto-revert-non-file-buffers t
   read-file-name-completion-ignore-case t
   read-buffer-completion-ignore-case t
   completion-ignore-case t
   org-attach-method "lns"
   custom-file (concat user-emacs-directory "custom.el"))

  (load-file custom-file)
  (load-file (concat user-emacs-directory "manu.el"))

  (blink-cursor-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (tooltip-mode 0)
  (column-number-mode)
  (show-paren-mode)
  (global-auto-revert-mode)
  (desktop-save-mode)
  (global-hl-line-mode)
  (recentf-mode)

  (set-face-attribute 'default nil
		      :font "Hack 13")

  (advice-add #'indent-for-tab-command :after #'hippie-expand)
  (load-theme 'modus-vivendi)

  :bind
  ("C-x C-b" . ibuffer)

  :hook
  (prog-mode . display-line-numbers-mode)
  (prog-mode . electric-pair-mode)
  (modus-themes-after-load-theme . pdf-view-themed-minor-mode)
)

(use-package dired
  :ensure nil
  :config

  (setq insert-directory-program "ls"
	dired-free-space nil
	dired-auto-revert-buffer t
	dired-listing-switches "-lh --group-directories-first"))

(use-package org
  :config

  (setq
   org-startup-indented t
   org-startup-with-beamer-mode t
   org-directory "~/Desktop/org"
   org-default-notes-file "~/Desktop/org/notes.org"
   org-archive-location "~/Desktop/org/archive.org::* ARCHIVE"
   org-agenda-window-setup 'only-window
   org-agenda-files (append (directory-files-recursively "~/Desktop/org" "\.org$"))
   org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
   org-refile-use-outline-path 'file
   org-refile-allow-creating-parent-nodes 'confirm
   org-outline-path-complete-in-steps nil
   org-src-window-setup 'current-window
   org-confirm-babel-evaluate nil
   org-id-link-to-org-use-id t
   org-M-RET-may-split-line '((default . nil)))

  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (java . t)
                               (shell . t)
                               (sql . t)
                               (sqlite . t)))

  (org-link-set-parameters
   "https"
   :follow (lambda(path)
             (call-process-shell-command
              (format "open -a \"Microsoft Edge\" \"https:%s\"" path) nil 0))
   "http"
   :follow (lambda(path)
             (call-process-shell-command
              (format "open -a \"Microsoft Edge\" \"http:%s\"" path) nil 0)))

  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)

  :hook
  (org-mode . turn-on-auto-fill))

(use-package try)

(use-package which-key
  :config (which-key-mode))

(use-package evil
  :custom
  (evil-undo-system 'undo-redo)
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-initial-state 'erc-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'rfc-mode 'emacs)
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-mode))

(use-package evil-escape
  :after evil
  :config

  (setq evil-escape-key-sequence "jj"
        evil-escape-delay 0.2
        evil-escape-inhibit-functions '(evil-visual-state-p)
	evil-escape-excluded-major-modes '(pdf-view-mode rfc-mode))

  (evil-escape-mode))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-surround
  :config (global-evil-surround-mode))

(use-package vertico
  :init (vertico-mode))

(use-package savehist
  :init (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind
  ("C-x b" . consult-buffer)
  ("C-x p b" . consult-project-buffer)
  ("C-x r b" . consult-bookmark))

(use-package magit)

(use-package erc-hl-nicks)
(use-package erc
  :config
  (erc-update-modules))

(use-package rfc-mode
  :init
  (setq rfc-mode-directory "~/.rfc")
  :bind
  (:map rfc-mode-map
	("j" . next-line)
	("k" . previous-line)))

(use-package pdf-tools
  :pin manual
  :init
  (setq pdf-view-continuous nil
	pdf-view-themed-minor-mode t)
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :bind
  (:map pdf-view-mode-map
	("j" . (lambda () (interactive) (pdf-view-next-line-or-next-page 10)))
	("k" . (lambda () (interactive) (pdf-view-previous-line-or-previous-page 10))))
  :config
  (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
  (pdf-tools-install t))
