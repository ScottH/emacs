;; init.el --- Emacs configuration
;;Apple-mouse for mouse-2 OSX only
;; ____________________________________________________________________________
;; Aquamacs custom-file warning:
;; Warning: After loading this .emacs file, Aquamacs will also load
;; customizations from `custom-file' (customizations.el). Any settings there
;; will override those made here.
;; Consider moving your startup settings to the Preferences.el file, which
;; is loaded after `custom-file':
;; ~/Library/Preferences/Aquamacs Emacs/Preferences
;; _____________________________________________________________________________
(define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
;;

;; use-package
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))
;;

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(better-defaults
    magit
    ein
    elpy
    flycheck
    py-autopep8
    fill-column-indicator
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


;; flycheck

(use-package flycheck
	     :ensure t
	     :init (global-flycheck-mode))

;;clang-format
(add-hook 'c-mode-common-hook
	  (function (lambda ()
		      (add-hook 'before-save-hook
				'clang-format-buffer) )))


;;autocomplete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;;yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;iedit

;;remap key based on b yuksel tutorial
(define-key global-map (kbd "C-c ;") 'iedit-mode)

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

(setq inhibit-startup-message t) ;; hide the startup message
;;(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally

;; raibow delimeters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)

;; (elpy-use-ipython)
;; (setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt --pprint")
;; ;; use flycheck not flymake with elpy
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; ;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

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
(setq inferior-lisp-program "/usr/local/bin/clisp")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"))
 '(custom-enabled-themes (quote (atom-dark)))
 '(custom-safe-themes
   (quote
    ("5b7c31eb904d50c470ce264318f41b3bbc85545e4359e6b7d48ee88a892b1915" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "d1af5ef9b24d25f50f00d455bd51c1d586ede1949c5d2863bef763c60ddf703a" "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "2642a1b7f53b9bb34c7f1e032d2098c852811ec2881eec2dc8cc07be004e45a0" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" default)))
 '(fci-rule-color "#073642")
 '(flymake-google-cpplint-command "/Users/scott/anaconda3/bin/cpplint")
 '(hl-sexp-background-color "#1c1f26")
 '(linum-format " %7i ")
 '(minimap-mode t)
 '(org-agenda-files
   (quote
    ("~/Documents/org/1.org" "~/Documents/org/ThingsToLearn.org")))
 '(package-selected-packages
   (quote
    (slime gnuplot gnuplot-mode blacken rainbow-mode minimap clang-format+ atom-one-dark-theme abyss-theme flymake-cursor atom-dark-theme auctex platformio-mode rainbow-delimiters arduino-mode flymake-google-cpplint iedit yasnippet yasnippet-snippets auto-complete company-c-headersac-dcd ## color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow sublimity sublime-themes py-autopep8 molokai-theme material-theme flycheck fill-column-indicator elpy ein color-theme better-defaults)))
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
