;;; init.el --- emacs configuration


;; Emacs config

;;; Code:

(message "Start of init.el")
;; Do this here to avoid issues with conf.org
(setq vc-follow-symlinks t) ;; dont bug me about symlinks

;; Gotta load org here since org is built in and will clash with loaded org later

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
  
; Install org early before builtin version gets loaded
(straight-use-package  'org)





(message "Running org-babel-load-file")
(org-babel-load-file (expand-file-name "conf.org" user-emacs-directory))



;; What is this for?
(let ((path (shell-command-to-string "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
  (setenv "PATH" path)
  (setq exec-path (split-string path path-separator)))



;; See: https://superuser.com/questions/364575/rebinding-s-mouse-1-to-mouse-2-in-emacs-on-os-x/1236645#1236645
(define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))








;; lsp mode


;; pyight
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred


;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (XXX-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)


;; eglot

;; treemacs - copied from the repo docs

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          ;;treemacs-header-scroll-indicators        '(nil . "^^^^^^")'
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))


    ;;(treemacs-hide-gitignored-files-mode nil)
    )
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))



;; Match parens
;;(show-paren-mode 1)

;; Let's try ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ;;("C-j" . ivy-next-line)
         ;;("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
    :init
  (ivy-rich-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config(setq which-key-idle-delay 0.5))


(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;; magit
(use-package magit)


;; Try projectile
(use-package projectile
  :ensure t
  ;;:pin melpa-stable
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Elpy

(use-package elpy
  :ensure t
  :init
  (elpy-enable))


(setq elpy-rpc-python-command "python3")
(setq elpy-rpc-virtualenv-path 'current)

;; flycheck
(use-package flycheck
	     :ensure t
	     :init (global-flycheck-mode))


;; anaconda-mode
;;(use-package anaconda-mode
;;  :ensure t
;;  )


;; company-mode
;;(use-package company-mode
;;  :ensure t)

(global-company-mode)
;;(global-set-key (kbd "<tab>") #'company-indent-or-complete-common)



;; ;; yasnippets
;; (use-package yasnippet                  ; Snippets
;;   :ensure t
;;   :config
;;   (validate-setq
;;    yas-verbosity 1                      ; No need to be so verbose
;;    yas-wrap-around-region t)

;;   (with-eval-after-load 'yasnippet
;;     (validate-setq yas-snippet-dirs '(yasnippet-snippets-dir)))

;;   (yas-reload-all)
;;   (yas-global-mode))

(use-package yasnippet-snippets         ; Collection of snippets
  :ensure t)


;; fci mode
(use-package fill-column-indicator
  :init (fci-mode 1))

(set-fill-column 80)

;;
(defvar my-packages
  '(ein
    py-autopep8
    auto-complete
    clang-format
    sublimity
    slime))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      my-packages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

;; sublimity
;;(require 'sublimity)
;;(require 'sublimity-scroll)
;;(require 'sublimity-map) ;; experimental
;;(require 'sublimity-attractive)



;;clang-format
;; (add-hook 'c-mode-common-hook
;; 	  (function (lambda ()
;; 		      (add-hook 'before-save-hook
;; 				'clang-format-buffer) )))


;; ;;autocomplete
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)

;; ;;yasnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; ;;iedit

;; ;;remap key based on b yuksel tutorial
;; (define-key global-map (kbd "C-c ;") 'iedit-mode)

;; ;;flymake google cpp
;; (defun my:flymake-google-init ()
;;   (require 'flymake-google-cpplint)
;;   (custom-set-variables
;;    '(flymake-google-cpplint-command "/home/scott/miniconda3/bin/cpplint")
;;    )
;;   (flymake-google-cpplint-load)
;;   )

;; (add-hook 'c-mode-hook 'my:flymake-google-init)
;; (add-hook 'c++-mode-hook 'my:flymake-google-init)


;;;;emacs-flymake-cursor to use flymake cursor w/ emacs >26
;;(use-package flymake-cursor
;;  :load-path "~/.emacs.d/repos/emacs-flymake-cursor" ;; cloned repo path
;;  :config
;;  (flymake-cursor-mode))


;;Platform IO
;;(require 'platformio-mode)

;; Add the required company backend.
;;(with-eval-after-load 'company
;;  (add-to-list 'company-backends 'company-irony)
;; Enable irony for all c++ files, and platformio-mode only
;; when needed (platformio.ini present in project root).
;;(add-hook 'c++-mode-hook (lambda ()
;;                           (irony-mode)
;;                          (irony-eldoc)
;;                         (platformio-conditionally-enable)))

;; Use irony's completion functions.
;;(add-hook 'irony-mode-hook
;;          (lambda ()
;;            (define-key irony-mode-map [remap completion-at-point]
;;              'irony-completion-at-point-async)
;;
;;            (define-key irony-mode-map [remap complete-symbol]
;;              'irony-completion-at-point-async)
;;
 ;;           (irony-cdb-autosetup-compile-options)))
            
;; Setup irony for flycheck.
;;(add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
;;




;; PYTHON CONFIGURATION
;; --------------------------------------

;;(elpy-enable)

;; (elpy-use-ipython)
;; (setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt --pprint")
;; ;; use flycheck not flymake with elpy
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; ;; enable autopep8 formatting on save
;;(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; ;; Arduino mode
;; (setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
;; (autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)
;; ;;


;; ;;
;; ;; C++ stuff
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)

;; (require 'yasnippet)
;; (yas-global-mode 1)

;; (require 'iedit)

;; (defun my:flymake-init ()
;;   (require 'flymake-google-cpplint)
;;   (flymake-google-cpplint-load)
;;   )

;; (add-hook 'c-mode-hook 'my:flymake-init)
;; (add-hook 'c++-mode-hook 'my:flymake-init)


;; init.el ends here

;;
;; SLIME
;;
;;(require 'sime)
;;(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; Optionally, specify the lisp program you are using. Default is "lisp"
;;(setq inferior-lisp-program "/usr/local/bin/clisp")
;;; .emacs ends here

(message "End of init.el")
