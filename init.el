;;; package.el
(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives
      '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;; use-package
(use-package use-package
  :custom
  (use-package-always-ensure t)
  (use-package-enable-imenu-support t))

;;; emacs
(use-package emacs
  :custom
  (inhibit-startup-screen t)
  (initial-buffer-choice "~/org/fhv.org")

  (fill-column 80)
  (tab-width 4)
  (frame-resize-pixelwise t)
  (delete-by-moving-to-trash t)
  (help-window-select t)
  (use-dialog-box nil)
  (vc-follow-symlinks t)

  ;; autosave and backup
  (bookmark-save-flag 1)
  (version-control t)
  (delete-old-versions -1)
  (auto-save-list-file-prefix "~/.emacs.d/autosave/")
  (auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
  (lock-file-name-transforms '((".*" "~/.emacs.d/lock/" t)))
  (backup-directory-alist '(("." . "~/.emacs.d/backup")))

  ;; completion
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (tab-always-indent 'complete)

  ;; ibuffer
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-display-summary nil)
  (ibuffer-formats
   '((mark modified read-only " " (name 16 32 :left :elide) " " (mode 16 16 :left :elide) " " filename-and-process)))
  (ibuffer-saved-filter-groups (quote (("default"
                                        ("Dired" (mode . dired-mode))
                                        ("Org mode" (mode . org-mode))
                                        ("IRC" (mode . rcirc-mode))
                                        ("Magit" (name . "^magit"))
                                        ("Emacs" (or
                                                  (name . "^\\*.*\\*$")
                                                  (name . "^\\*scratch\\*$")
                                                  (name . "^\\*Messages\\*$")))))))
  :config
  (blink-cursor-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (tooltip-mode 0)
  (column-number-mode)
  (show-paren-mode)
  (global-auto-revert-mode)
  (global-hl-line-mode)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (advice-add #'indent-for-tab-command :after #'hippie-expand)

  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (let ((font-height (if (string= system-name "M720t") 130 130)))
                (with-selected-frame frame
                  (set-face-attribute 'default nil :font "Iosevka" :height font-height)))))

  (add-hook 'emacs-lisp-mode-hook
			(lambda ()
			  (when (string-equal (buffer-name) "init.el")
				(setq-local outline-regexp ";;;\\(;* [^ \t\n]\\)")
				(outline-minor-mode 1)
				(evil-local-set-key 'normal (kbd "TAB") 'outline-toggle-children)
				(outline-hide-sublevels 1))))

  (load-theme 'modus-vivendi-deuteranopia)

  :bind*
  ("C-x C-b" . ibuffer)
  ("M-o" . other-window)
  ("M-j" . next-buffer)
  ("M-k" . previous-buffer)
  ("C-c a" . org-agenda)
  ("C-c i" . (lambda () (interactive) (find-file user-init-file)))
  ("C-c g" . (lambda () (interactive) (find-file "~/org/gptel.org")))
  ("C-c e" . (lambda () (interactive) (elfeed-update) (elfeed)))
  ("C-c r" . rcirc)

  :hook
  (prog-mode . display-line-numbers-mode)
  (prog-mode . electric-pair-mode)
  ;; (jupyter-python-mode . python-mode)
  (ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "default"))))

;;; tabs
(use-package tab-bar
  :after org
  :custom
  (tab-bar-show 1)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  ;; (tab-bar-new-tab-choice (lambda () (org-agenda nil "a")))
  (tab-bar-new-tab-choice 'ibuffer)
  :config
  (tab-bar-mode)
  :bind*
  ("M-w" . tab-close)
  ("M-u" . tab-undo)
  ("M-RET" . tab-new)
  ("M-h" . tab-previous)
  ("M-l" . tab-next))

;;; dired
(use-package dired
  :ensure nil
  :custom
  (insert-directory-program "ls")
  (dired-free-space nil)
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-lh --group-directories-first"))

;;; orgmode
(use-package org
  :custom
  (org-startup-indented t)
  (org-agenda-window-setup 'only-window)
  (org-src-window-setup 'current-window)
  (org-list-allow-alphabetical t)

  (org-agenda-custom-commands
   '(("f" "Filtered Agenda"
      agenda "" 
      ((org-agenda-skip-function
        (lambda ()
          (let ((heading (org-get-heading t t t t)))
            (if (string-match-p "business processes" (downcase heading))
                (point)  ;; Skip this entry by returning the point
              nil))))))))

  (org-archive-location "~/org/archive.org::* ARCHIVE")
  (org-agenda-files (append (directory-files-recursively "~/org" "\.org$")))
  (org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-outline-path-complete-in-steps nil)

  (org-list-allow-alphabetical t)
  (org-confirm-babel-evaluate nil)
  (org-id-link-to-org-use-id t)
  ;; (org-latex-hyperref-template "\\usepackage[colorlinks=false]{hyperref}")
  (org-latex-hyperref-template nil)
  (org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  :config
  (advice-add 'org-latex-compile :after #'delete-file)
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (java . t)
                               (shell . t)
                               (sql . t)
                               (sqlite . t)))
  :bind
  ("C-M-<return>" . org-meta-return)
  :hook
  (org-mode . turn-on-auto-fill)
  (org-babel-after-execute . org-redisplay-inline-images))

(use-package org-download)

;;; evil
(use-package evil
  :custom
  (evil-undo-system 'undo-redo)
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-initial-state 'rcirc-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  ;; (evil-set-initial-state 'gptel-mode 'emacs)
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-mode))

(use-package evil-escape
  :after evil
  :custom
  (evil-escape-key-sequence "jj")
  (evil-escape-delay 0.2)
  (evil-escape-inhibit-functions '(evil-visual-state-p))
  :config
  (evil-escape-mode))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-surround
  :config (global-evil-surround-mode))

;;; in-buffer completion
(use-package corfu
  :custom
  (corfu-auto nil)
  (corfu-quit-no-match t)
  :init (global-corfu-mode))

;;; minibuffer completion
(use-package savehist
  :init (savehist-mode))

(use-package vertico
  :init (vertico-mode))

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
  ("C-x r b" . consult-bookmark))

;;; git
(use-package magit)
;;; misc
(use-package try)
(use-package which-key
  :config (which-key-mode))
(use-package notmuch)
(use-package ol-notmuch)
(use-package hungry-delete
  :config (global-hungry-delete-mode))
(use-package spacious-padding
  :config(spacious-padding-mode))
(use-package ef-themes)

;;; gpt
(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist '((org-mode . "")))
  (gptel-response-prefix-alist '((org-mode . "")))

  :hook
  (org-mode . (lambda ()
				(when (equal (buffer-file-name) (expand-file-name "~/org/gptel.org"))
				  (gptel-mode 1)))))
;;; custom
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))
