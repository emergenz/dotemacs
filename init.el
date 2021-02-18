(setq gc-cons-threshold (* 50 1000 1000))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;(setq coding-system-for-read 'utf-8)
;(setq coding-system-for-write 'utf-8)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))




(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

(use-package evil
  :ensure t
  :defer .1 ;; don't block emacs when starting, load evil immediately after startup
  :init
  (setq evil-want-integration nil) ;; required by evil-collection
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t) ;; like vim's 'splitright'
  (setq evil-split-window-below t) ;; like vim's 'splitbelow'
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode))

;(use-package ido
  ;:ensure t
  ;:defer .1 ;; don't block emacs when starting, load evil immediately after startup
  ;:config
  ;(ido-mode t))

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(blink-cursor-mode -1)

(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)

(setq inhibit-startup-screen t)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
       (abbreviate-file-name (buffer-file-name))
       "%b"))))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(set-frame-font "Hack 16" nil t)

(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)

(setq-default tab-width 4
              indent-tabs-mode nil)

(use-package smartparens
  :ensure t
  :defer 2
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package magit
  :ensure t
  :defer 5
  :bind (("C-M-g" . magit-status)))

;loeschen?
;(global-undo-tree-mode)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;(use-package ido-vertical-mode
  ;:ensure t
  ;:defer 1
  ;:config
  ;(ido-mode 1)
  ;(ido-vertical-mode 1)
  ;(setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package smex
  :ensure t
  :defer 1
  :config
  (smex-initialize))

; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq gc-cons-threshold (* 2 1000 1000))

(global-auto-revert-mode nil)

(use-package org
  :ensure t
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)
  (setq org-agenda-files '("~/Documents/gtd/inbox.org"
                           "~/Documents/gtd/gtd.org"
                           "~/Documents/gtd/tickler.org")))

(global-set-key (kbd "C-c c") 'org-capture)

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package tex-mik
  :config
  (load "auctex.el" nil t t))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq TeX-PDF-mode t)

(setq-default TeX-master "document")

(setq-default mode-line-format
              '("%e" ; print error message about full memory.
                mode-line-front-space
                ; mode-line-mule-info
                ; mode-line-client
                ; mode-line-modified
                ; mode-line-remote
                ; mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                 mode-line-position
                ; (vc-mode vc-mode)
                ; "  "
                ;mode-line-modes
                "   "
                ; mode-line-misc-info
                display-time-string
                "   "
                battery-mode-line-string
                mode-line-end-spaces))

(display-time-mode 1)
(setq display-time-format "| %a %d. %m. | %t%R |")
(display-battery-mode 1)
;(setq battery-mode-line-format "%p%%") ; Default: "[%b%p%%]"

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t))

(add-hook 'after-init-hook 'global-company-mode)

(use-package company-flx
  :ensure t
  :config)

(with-eval-after-load 'company
  (company-flx-mode +1))

(use-package vhdl-capf
  :ensure t)

(when (require 'vhdl-capf)
  (vhdl-capf-enable))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (prog-mode . lsp)
         (emacs-lisp-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optional if you want which-key integration
(use-package which-key
    :ensure t
    :config
    (which-key-mode))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

;; jump to definition with 'gd'
(use-package dumb-jump
  :ensure t)

(use-package js2-mode
  :ensure t)
  ;:defer 20

(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t)
  (modus-themes-load-themes)
  :config
  (load-theme 'modus-operandi t)
  (global-set-key (kbd "<f5>") #'modus-themes-toggle))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'ace-window))

; This goes into your emacs config file
(use-package pdf-tools
  :ensure t
  :config
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(setq pdf-view-use-scaling t)

;; this only has to be executed for the installation and can be removed/commented afterwards
;; I recommend commenting it out so that it can be found easily when reinstalling
;(setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
;(pdf-tools-install)

; set C-i as universal-argugment in order to use C-u in evil-mode
(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
(global-set-key (kbd "C-i") 'universal-argument)

(use-package org-noter
  :ensure t
  :config
  (org-noter-set-auto-save-last-location t))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-org gruvbox-theme org which-key vhdl-capf use-package smex smartparens pdf-tools org-noter modus-themes lsp-ui lsp-treemacs lsp-ivy js2-mode flycheck dumb-jump dashboard counsel company-quickhelp company-flx auctex-latexmk)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
