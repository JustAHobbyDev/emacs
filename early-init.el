;; Disable packagel.el
;; You'll also want to disable package.el in your early-init file.


;; [[file:config.org::*Disable packagel.el][Disable packagel.el:1]]
(setq package-enable-at-startup nil)
;; Disable packagel.el:1 ends here

;; Native compilation cache

;; When using Emacs 29, the location of the native compilation cache can
;; be changed using a function, preferably in ~early-init.el~:


;; [[file:config.org::*Native compilation cache][Native compilation cache:1]]
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))
;; Native compilation cache:1 ends here
