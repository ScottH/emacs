;;; init.el --- emacs configuration


;; Emacs config

;;; Code:

(message "Start of init.el")
;; Do this here to avoid issues with conf.org
(setq vc-follow-symlinks t) ;; dont bug me about symlinks

;; Gotta load org here since org is built in and will clash with loaded org later

;; dont use straight for emacs 29.1

;; ;; Install straight.el
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))
  
;; ; Install org early before builtin version gets loaded
;; (straight-use-package  'org)





(message "Running org-babel-load-file")
(org-babel-load-file (expand-file-name "conf.org" user-emacs-directory))



;; What is this for?
(let (
      (path (shell-command-to-string "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
  (setenv "PATH" path)
  (setq exec-path (split-string path path-separator)))



;; See: https://superuser.com/questions/364575/rebinding-s-mouse-1-to-mouse-2-in-emacs-on-os-x/1236645#1236645
;;(define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))


(message "End of init.el")
