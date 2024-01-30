(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
  :init
  ;;(setq evil-want-integration t) ;; this is optional, evil-collection is t
  (setq evil-want-keybinding nil) ;; needs to be set after evil loads
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode 1)
  (with-eval-after-load 'evil 
      ;; (setq evil-want-keybinding t)
      (with-eval-after-load 'elpaca-ui (evil-make-intercept-map elpaca-ui-mode-map))
      (with-eval-after-load 'elpaca-info (evil-make-intercept-map elpaca-info-mode-map))))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  ;; NOTE: If you don’t like surprises but still want to use evil-collection-init, setting
  ;; evil-collection-mode-list to nil and adding each mode manually might be a better option. 
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init)
  (with-eval-after-load 'evil-collection
      (with-eval-after-load 'org (evil-collection-org-setup))
      (with-eval-after-load 'info (evil-collection-info-setup))))

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;Useful for configuring built-in emacs features.
(use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))

(use-package general
  :config
  (general-evil-setup)
  ;; set 'SPC' as the global leader key
  (general-create-definer jah/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; sets leader
    :global-prefix "M-SPC")

  (jah/leader-keys
    "x" '(execute-extended-command :wk "M-x: Execute Extended Command"))

  (jah/leader-keys
    "." '(find-file :wk "Find file")
    "f" '(:ignore :wk "File Commands")
    "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
    "f C" '(jah/reload-init-file :wk "Reload emacs config")
    "TAB TAB" '(comment-line :wk "Comment lines"))

  (jah/leader-keys
    "b" '(:ignore t :wk "buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b n" '(next-buffer :wk "Goto next buffer")
    "b p" '(previous-buffer :wk "Goto previous buffer")
    "b r" '(revert-buffer :wk "Revert buffer")
    "b l" '(list-buffers :wk "Open buffer list"))

  (jah/leader-keys
    "e" '(:ignore t :wk "Eval")
    "e b" '(eval-buffer :wk "Eval current buffer")
    "e d" '(eval-defun :wk "Eval defun at or after point")
    "e l" '(eval-last-sexp :wk "Eval expression before point")
    "e e" '(eval-expression :wk "Eval expression input")
    "e r" '(eval-region :wk "Eval active region"))

  (jah/leader-keys
    "h" '(:ignore :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h o" '(describe-symbol :wk "Describe function or variable")
    "h k" '(describe-key :wk "Describe keybinding as input")
    "h w" '(where-is :wk "Describe keybindings for input")
    "h m" '(describe-mode :wk "Describe current major/minor modes")
    "h i" '(info :wk "Enter Info, the documentation browser"))

    (jah/leader-keys
      "t" '(:ignore t :wk "Toggle")
      "t l" '(display-line-numbers-mode t :wk "Toggle line numbers")
      "t t" '(visual-line-mode t :wk "Toggle truncated lines"))
  )
  (elpaca-wait)

;; modus-vivendi theme customizations
(setq modus-themes-mode-line
      '(borderless
	accented
	padded
	))

(setq modus-themes-region
      '(bg-only
	))

;; Check the manual for tweaking ‘bold’ and ‘italic’ faces: Info
;; node ‘(modus-themes) Configure bold and italic faces’.
(setq modus-themes-completions
	(quote ((matches . (extrabold background intense)) ;; matched user input
		(selection . (semibold accented intense)) ;; current line or matched candidate
		(popup . (accented)) ;; anciliary popups
		)))

;; Load a color theme
(load-theme 'modus-vivendi t)

;; Set fonts
(set-face-attribute 'default nil :font "Berkeley Mono" :height 110 :width 'regular)
(set-face-attribute 'variable-pitch nil :font "Berkeley Mono Variable" :height 120 :width 'regular)
(set-face-attribute 'fixed-pitch nil :font "Berkeley Mono" :height 110 :width 'regular)

;; Italicize comments
(set-face-attribute 'font-lock-comment-face nil :slant 'italic) ;; Italicize keywords
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

;; Set font on graphical frames
(add-to-list 'default-frame-alist '(font . "Berkeley Mono 12"))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-nerd-fonts
  :after all-the-icons
  :ensure t
  :config
  (all-the-icons-nerd-fonts-prefer))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)
(global-visual-line-mode t)

;; Turn off line number
(line-number-mode -1)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)

(require 'org-tempo)

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 0) ;; Different scroll margin
  (setq vertico-count 20) ;; Show more candidates
  (setq vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

(with-eval-after-load 'vertico
  (keymap-set vertico-map "TAB" #'minibuffer-complete)
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package emacs
  :elpaca nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
			(replace-regexp-in-string
			 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
			 crm-separator)
			(car args))
		(cdr args)))
      (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

	;; Do not allow the cursor in the minibuffer prompt
	(setq minibuffer-prompt-properties
	      '(read-only t cursor-intangible t face minibuffer-prompt))
	(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package vertico-prescient
  :after vertico
  :init
  (vertico-prescient-mode))

(use-package sudo-edit
  :after general
  :config
    (jah/leader-keys
      "f u" '(sudo-edit-find-file :wk "Sudo find file")
      "f U" '(sudo-edit :wk "Sudo edit file"))
)

(use-package which-key
  :init
    (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 40
        which-key-allow-imprecise-window-fit t
        which-key-separator " -> "))

(defun jah/reload-init-file ()
  (interactive)
  (load-file user-init-file))

(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
