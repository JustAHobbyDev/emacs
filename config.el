(defvar elpaca-installer-version 0.7)
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
  ;; Enable :ensure use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq use-package-always-ensure t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)

(use-package no-littering
  :config
  (no-littering-theme-backups))

(with-eval-after-load 'recentf 
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory)))

(with-eval-after-load 'no-littering
      (no-littering-theme-backups))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(add-hook 'Info-mode-hook
  (lambda ()
    (local-set-key "z" #'Info-scroll-up)
    (setq Info-isearch-search 1)
))

;; Remap C-x C-b from list-buffers to ibuffer
(global-set-key [remap list-buffers] 'ibuffer)

(global-set-key (kbd "C-x C-.") #'find-file-at-point)

(setq help-window-select t)

;; Set fonts
(set-face-attribute 'default nil :font "Berkeley Mono" :height 105 :width 'regular)
(set-face-attribute 'variable-pitch nil :font "Berkeley Mono Variable" :height 120 :width 'regular)
(set-face-attribute 'fixed-pitch nil :font "Berkeley Mono" :height 105 :width 'regular)

;; Italicize comments
(set-face-attribute 'font-lock-comment-face nil :slant 'italic) ;; Italicize keywords
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

;; Set font on graphical frames
(add-to-list 'default-frame-alist '(font . "Berkeley Mono 11"))

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

(make-symbol "use-naysayer-theme")
(if (boundp 'use-naysayer-theme)
  (use-package naysayer-theme	  
    :config			  
    (load-theme 'naysayer t)))

;; Load a color theme
; (load-theme 'modus-vivendi t)

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

(global-hl-line-mode)

;; Turn off line number
(line-number-mode -1)

(use-package helpful
  :init
  ;; If you want to replace the default Emacs /help/ keybindings:
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)

  ;; Recommended keybindings to get the most out of *helpful*:
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c d") #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function))

(keymap-global-set "C-c l" 'org-store-link)
(keymap-global-set "C-c a" 'org-agenda)
(keymap-global-set "C-c c" 'org-capture)

;; Put captured notes in their own directory
(setq org-default-notes-files (concat org-directory "/notes.org"))

(setq org-agenda-files '(
  "~/.config/emacs"
  "~/Documents/org"
))

;; Store state change notes into drawer LOGBOOK
(setq org-log-into-drawer t)

;; Custom keywords
(setq org-todo-keywords '((sequence "TODO(t)" "DONE(d!)")))

(font-lock-add-keywords 'org-mode
  '(("^ *\\([-]\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "·"))))))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)

(require 'org-tempo)

(setq org-hide-emphasis-markers t)

(use-package org-roam)

(use-package vertico
  :defer t
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 0) ;; Different scroll margin
  (setq vertico-count 20) ;; Show more candidates
  (setq vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)

  :bind
  (("TAB" . minibuffer-complete)
   ("DEL" . vertico-directory-delete-char))

  :config
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (savehist-mode))

(use-package emacs
  :ensure nil
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

(defvar +vertico-current-arrow t)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                 (not (bound-and-true-p vertico-flat-mode)))
                                            (eql t)))
  (setq cand (cl-call-next-method cand prefix suffix index start))
  (if (bound-and-true-p vertico-grid-mode)
      (if (= vertico--index index)
          (concat #("=>" 0 1 (face vertico-current)) cand)
        (concat #("_" 0 1 (display " ")) cand))
    (if (= vertico--index index)
        (concat #(" " 0 1 (display (left-fringe right-triangle vertico-current))) cand)
      cand)))

(setq vertico-buffer-mode 1)

(use-package orderless
  :defer t
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-hkoverrides '((file (styles basic partial-completion))))
  :bind (( "S-SPC" . +vertico-restrict-to-matches)))

(defun +vertico-restrict-to-matches ()
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert " ")
    (add-text-properties (minibuffer-prompt-ned) (point-max)
                         '(invisible t read-only t cursor-intangible t rear-nonsticky t))))

(use-package marginalia
:defer t
:bind (:map minibuffer-local-map
("M-A" . marginalia-cycle))
:init
(marginalia-mode))

(use-package consult
  :defer t
  :bind (;; C-c bindings in `mode-specific-map'
       ("C-c M-x" . consult-mode-command)
       ("C-c h" . consult-history)
       ("C-c k" . consult-kmacro)
       ("C-c m" . consult-man)
       ("C-c i" . consult-info)
       ([remap Info-search] . consult-info)
       ;; C-x bindings in `ctl-x-map'
       ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
       ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
       ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
       ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
       ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
       ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
       ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
       ;; Custom M-# bindings for fast register access
       ("M-#" . consult-register-load)
       ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
       ("C-M-#" . consult-register)
       ;; Other custom bindings
       ("M-y" . consult-yank-pop)                ;; orig. yank-pop
       ;; M-g bindings in `goto-map'
       ("M-g e" . consult-compile-error)
       ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
       ("M-g g" . consult-goto-line)             ;; orig. goto-line
       ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
       ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
       ("M-g m" . consult-mark)
       ("M-g k" . consult-global-mark)
       ("M-g i" . consult-imenu)
       ("M-g I" . consult-imenu-multi)
       ;; M-s bindings in `search-map'
       ("M-s d" . consult-fd)                  ;; Alternative: consult-find
       ("M-s c" . consult-locate)
       ("M-s g" . consult-grep)
       ("M-s G" . consult-git-grep)
       ("M-s r" . consult-ripgrep)
       ("M-s l" . consult-line)
       ("M-s L" . consult-line-multi)
       ("M-s k" . consult-keep-lines)
       ("M-s u" . consult-focus-lines)
       ;; Isearch integration
       ("M-s e" . consult-isearch-history)
       :map isearch-mode-map
       ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
       ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
       ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
       ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
       ;; Minibuffer history
       :map minibuffer-local-map
       ("M-s" . consult-history)                 ;; orig. next-matching-history-element
       ("M-r" . consult-history))                ;; orig. previous-matching-history-element

;; Enable automatic preview at point in the *Completions* buffer. This is
;; relevant when you use the default completion UI.
:hook (completion-list-mode . consult-preview-at-point-mode)

;; The :init configuration is always executed (Not lazy)
:init

;; Optionally configure the register formatting. This improves the register
;; preview for `consult-register', `consult-register-load',
;; `consult-register-store' and the Emacs built-ins.
(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)

;; Optionally tweak the register preview window.
;; This adds thin lines, sorting and hides the mode line of the window.
(advice-add #'register-preview :override #'consult-register-window)

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; Configure other variables and modes in the :config section,
;; after lazily loading the package.
:config

;; Optionally configure preview. The default value
;; is 'any, such that any key triggers the preview.
;; (setq consult-preview-key 'any)
;; (setq consult-preview-key "M-.")
;; (setq consult-preview-key '("S-<down>" "S-<up>"))
;; For some commands and buffer sources it is useful to configure the
;; :preview-key on a per-command basis using the `consult-customize' macro.
(consult-customize
 consult-theme :preview-key '(:debounce 0.2 any)
 consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 consult--source-bookmark consult--source-file-register
 consult--source-recent-file consult--source-project-recent-file
 ;; :preview-key "M-."
 :preview-key '(:debounce 0.4 any))

;; Optionally configure the narrowing key.
(setq consult-narrow-key "<") ;; "C-+"

;; Optionally make narrowing help available in the minibuffer.
;; You may want to use `embark-prefix-help-command' or which-key instead.
;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;; By default `consult-project-function' uses `project-root' from project.el.
;; Optionally configure a different project root function.
;;;; 1. project.el (the default)
;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. vc.el (vc-root-dir)
;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 3. locate-dominating-file
;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;;; 4. projectile.el (projectile-project-root)
;; (autoload 'projectile-project-root "projectile")
;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 5. No project support
;; (setq consult-project-function nil)
)

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Add extensions
(use-package cape
  :defer t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package magit 
:ensure t
:defer t)

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

(use-package pdf-tools
:init
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
:config
(pdf-tools-install))

(setq dired-clean-confirm-killing-deleted-buffers nil) ;; don't ask to kill buffers visiting deleted files
(setq dired-listing-switches "-alt")' ;; show hidden; long listing; sort by date
(setq dired-dwim-target t) ;; guess target destination
(setq dired-recursive-copies 'always) ;; copy recursively without asking
(setq dired-recursive-deletes 'always) ;; delete recursively without 
(setq dired-omit-files "\\.\\(#\\|\\.*$\\)")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

(defun jah/reload-init-file ()
  (interactive)
  (load-file user-init-file))

(use-package treesit-auto
:custom
(treesit-auto-install 'prompt)
:config
(treesit-auto-add-to-auto-mode-alist 'all)
(global-treesit-auto-mode))

(use-package deadgrep
:bind (("<f5>" . #'deadgrep)))

(use-package wgrep-deadgrep
:hook (deadgrep-finished-hook wgrep-deadgrep-setup))

(use-package rg
:init
(rg-enable-default-bindings)
:config
(setq rg-finish-functions
  (lambda (buf fin)
    (switch-to-buffer-other-window buf))))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(use-package eat
:init
(add-hook 'eshell-load-hook #'eat-eshell-mode)
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(defun dispatch-region ()
  (interactive)
  (thread-first
    (mapcar (pcase-lambda (`(,beg . ,end))
              (cons (create-marker beg)
                    (create-marker end)))
            (region-bounds))
    (region-iterator)                   ;; produces sequential (beg . end) pairs
    (dispatch-single-buffer)            ;; undo amalgamation, can handle multiple buffers
    (dispatch-with-state current-state) ;; modal keybinding mode state
    (pulse-on-record)                   ;; pulse region when recording the macro
    (macro-dispatch)))

;; Might clash with C-' org-cycle-agenda-files command
(use-package surround
  :ensure t
  :bind-keymap ("C-'" . surround-keymap))

;; User level paths I want added to my Emacs environment PATH
(setq jah/paths '(
  "/home/jah/.pyenv/plugins/pyenv-virtualenv/shims"
  "/home/jah/.pyenv/bin"
  "/home/jah/.pyenv/shims"
  "/home/jah/.cargo/bin"
  "/home/jah/.local/bin"
))

(setq exec-path (delete-dups (append jah/paths exec-path)))
