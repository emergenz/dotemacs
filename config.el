;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Hack" :size 14 :weight 'semi-light))
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-vivendi)
;(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


; Modus-Themes
(setq modus-themes-slanted-constructs t
      odus-themes-bold-constructs t)
(global-set-key [f5] 'modus-themes-toggle)

; disable snipe
(after! evil-snipe
  (evil-snipe-mode -1))

; Modified delay in showing suggestions. (company)
(setq company-idle-delay 0.1)

; set new ace-window key
(global-set-key (kbd "C-;") 'ace-window)

; disable evil in info-mode
; edited in /.emacs.d/modules/editor/evil/init.el

; org-mode
;(define-key global-map "\C-ca" 'org-agenda)
;(setq org-log-done t)
;(setq org-agenda-files '("~/Documents/gtd/todo.org"
                        ;"~/Documents/gtd/gtd.org"
                        ;"~/Documents/gtd/tickler.org"))
;(global-set-key (kbd "C-c c") 'org-capture)

(after! org
 (setq org-capture-templates
      '(("t" "Personal todo" entry
        (file "~/Dropbox/org/todo.org")
        "* TODO %?
        %i
        %a" :prepend t)
        ("n" "Personal notes" entry
        (file+headline +org-capture-notes-file "Inbox")
        "* %u %?
        %i
        %a" :prepend t)
        ("j" "Journal" entry
        (file+olp+datetree +org-capture-journal-file)
        "* %U %?
        %i
        %a" :prepend t)
        ("p" "Templates for projects")
        ("pt" "Project-local todo" entry
        (file+headline +org-capture-project-todo-file "Inbox")
        "* TODO %?
        %i
        %a" :prepend t)
        ("pn" "Project-local notes" entry
        (file+headline +org-capture-project-notes-file "Inbox")
        "* %U %?
        %i
        %a" :prepend t)
        ("pc" "Project-local changelog" entry
        (file+headline +org-capture-project-changelog-file "Unreleased")
        "* %U %?
        %i
        %a" :prepend t)
        ("o" "Centralized templates for projects")
        ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?
        %i
        %a" :heading "Tasks" :prepend nil)
        ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?
        %i
        %a" :heading "Notes" :prepend t)
        ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?
        %i
        %a" :heading "Changelog" :prepend t)))
 (plist-put org-format-latex-options :scale 1.0)
 )


; assembly
(defun my-asm-mode-hook ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; (local-unset-key "<return>") ; doesn't work. "RET" in a terminal.  http://emacs.stackexchange.com/questions/13286/how-can-i-stop-the-enter-key-from-triggering-a-completion-in-company-mode
  (electric-indent-local-mode)  ; toggle off
;  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  ;; (setq tab-always-indent (default-value 'tab-always-indent))

  (defun asm-calculate-indentation ()
  (or
   ;; Flush labels to the left margin.
;   (and (looking-at "\\(\\.\\|\\sw\\|\\s_\\)+:") 0)
   (and (looking-at "[.@_[:word:]]+:") 0)
   ;; Same thing for `;;;' comments.
   (and (looking-at "\\s<\\s<\\s<") 0)
   ;; %if nasm macro stuff goes to the left margin
   (and (looking-at "%") 0)
   (and (looking-at "c?global\\|section\\|default\\|align\\|INIT_..X") 0)
   ;; Simple `;' comments go to the comment-column
   ;(and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
   ;; The rest goes at column 4
   (or 4)))
  )

(add-hook 'asm-mode-hook #'my-asm-mode-hook)

; controlling macOS music from inside emacs
;(load-file "~/.emacs.d/modules/tools/musica/musica.el")

;(global-set-key (kbd "C-c m SPC") #'musica-play-pause)
;(global-set-key (kbd "C-c m i") #'musica-info)
;(global-set-key (kbd "C-c m n") #'musica-play-next)
;(global-set-key (kbd "C-c m p") #'musica-play-previous)
;(global-set-key (kbd "C-c m r") #'musica-play-next-random)
;(global-set-key (kbd "C-c m s") #'musica-search)

; bind gud-run
;(global-set-key(kbd "C-x C-a r") #'gud-run)

; tls workaround for eww
;(setq network-security-level 'low)

;; ; w3m
;; (setq w3m-search-default-engine "duckduckgo")
;; (with-eval-after-load 'w3m
;;  (evil-define-key 'normal w3m-mode-map
;;   "s" 'w3m-search
;;   "S" 'w3m-search-new-session
;;   "d" 'evil-scroll-down
;;   "u" 'evil-scroll-up
;;   "x" 'w3m-delete-buffer
;;   "J" 'w3m-previous-buffer
;;   "K" 'w3m-next-buffer
;;   "E" 'w3m-view-url-with-browse-url))


;; ; w3m link-hinting
;; (defvar w3m-isearch-links-do-wrap nil
;; "Used internally for fast search wrapping.")

;; (defun w3m-isearch-links (&optional regexp)
;; (interactive "P")
;; (let ((isearch-wrap-function
;; #'(lambda ()
;;         (setq w3m-isearch-links-do-wrap nil)
;;         (if isearch-forward
;; (goto-char (window-start))
;; (goto-char (window-end)))))
;; (isearch-search-fun-function
;; #'(lambda () 'w3m-isearch-links-search-fun))
;; post-command-hook   ;inhibit link echoing
;; do-follow-link
;; (isearch-mode-end-hook
;; (list  #'(lambda nil
;;         (when (and (not isearch-mode-end-hook-quit)
;;         (w3m-anchor))
;; (setq do-follow-link t))))))
;; (setq w3m-isearch-links-do-wrap t)
;; (isearch-mode t
;; regexp
;; ;; fast wrap
;; #'(lambda nil
;; (if isearch-success
;;         (setq w3m-isearch-links-do-wrap t)
;; (when w3m-isearch-links-do-wrap
;;         (setq w3m-isearch-links-do-wrap nil)
;;         (setq isearch-forward
;;         (not isearch-forward))
;;         (isearch-repeat isearch-forward))))
;; t)
;; (when do-follow-link
;; (w3m-view-this-url))))

;; (defun w3m-isearch-links-search-fun (string &optional bound no-error)
;; (let* (isearch-search-fun-function
;; (search-fun  (isearch-search-fun))
;; error
;; (bound  (if isearch-forward
;;         (max (or bound 0)
;;         (window-end))
;;         (min (or bound (window-start))
;; (window-start)))))
;; (condition-case err
;; (while (and (apply search-fun (list string bound))
;;         (not (w3m-anchor (point)))))
;; (error (setq error err)))
;; (if error
;; (if (not no-error)
;; (signal (car error) (cadr error)))
;; (point))))

;; (require 'w3m)
;; (evil-define-key 'normal w3m-mode-map "f" 'w3m-isearch-links)

;; ; signal-msg
;; (add-to-list 'load-path (expand-file-name "~/signal-msg"))
;; (require 'signal-msg)
;; (setq signal-msg-username "+4369917182739")

;; ; pdf-tools custom keybindings
;; (evil-define-key 'normal pdf-view-mode-map
;;   "d" 'pdf-view-scroll-up-or-next-page
;;   "u" 'pdf-view-scroll-down-or-previous-page)

;; (setq-hook! 'pdf-view-mode-hook
;;   pdf-view-midnight-colors '( "#ffffff" . "#000000"))

;; ; exec-path-from-shell
;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

;; ; w3m hack for not hanging?
;; (add-to-list 'w3m-command-environment '("GC_NPROCS" ."1"))

; change font only for info mode and emacs-w3m
 ;; Use variable width font faces in current buffer
(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Verdana" :height 150))
  (buffer-face-mode))

;; Use monospaced font faces in current buffer
(defun my-buffer-face-mode-fixed ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Consolas" :height 100))
  (buffer-face-mode))

;; Set default font faces for Info and ERC modes
;(add-hook 'w3m-mode-hook 'my-buffer-face-mode-variable)
(add-hook 'Info-mode-hook 'my-buffer-face-mode-variable)

; open links in eww per default
; (setq browse-url-browser-function 'eww-browse-url)


; eww
(with-eval-after-load 'eww
 (evil-define-key 'normal eww-mode-map
  "d" 'evil-scroll-down
  "u" 'evil-scroll-up))

; xwidget
;(with-eval-after-load 'xwidget-webkit
  ;(define-key xwidget-webkit-mode-map
    ;(kbd "f") 'xwwp-follow-link))
    ;"u" 'xwidget-webkti-scroll-down
    ;"f" 'xwwp-follow-link))

; emacs eww as browse-url-browser
; (setq browse-url-browser-function 'eww)

;; (defun xdg-open (filename)
;;   (interactive "fFilename: ")
;;   (let ((process-connection-type))
;;     (start-process "" nil "xdg-open" (expand-file-name filename))))

(defun xdg-open (file)
  (interactive "f")
  (let ((process-connection-type nil))
    (start-process
     "" nil shell-file-name
     shell-command-switch
     (format "nohup 1>/dev/null 2>/dev/null xdg-open %s"
             (expand-file-name file)))))

(defun find-file-auto (orig-fun &rest args)
  (let ((filename (car args)))
    (if (cl-find-if
         (lambda (regexp) (string-match regexp filename))
         '("\\.pdf\\'" "\\.docx?\\'"))
        (xdg-open filename)
      (apply orig-fun args))))

(advice-add 'find-file :around 'find-file-auto)

;; ccls
(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom
