;;
;; init.el start
;; --------------------------------------------------------------------------
;;
;; Packages
;; --------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(color-theme-sanityinc-tomorrow
    rjsx-mode
    exec-path-from-shell
    org
    magit
    eshell-git-prompt
    ox-twbs))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;
;; Packages Settings
;; --------------------------------------------------------------------------

;; exec-path-from-shell
(exec-path-from-shell-initialize)

;; org
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-hide-leading-stars t)
(setq org-log-done t)
(setq org-startup-truncated nil)

(defun epi-org-list (files)
  (let ((epi-dir "~/workspace/epi_org/"))
    (mapcar (lambda (f) (concat epi-dir f)) files)))
(setq org-agenda-files (epi-org-list '("test.org" "cookbook.org" "reviews.org")))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; eshell-git-prompt
(eshell-git-prompt-use-theme 'powerline)

;; rjsx-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook 'electric-pair-mode)
(setq js-indent-level 2)

;;
;; Custom Settings
;; --------------------------------------------------------------------------

;; UI Settings
;; ------------------------------
(defvar my-font
  (cond ((eq system-type 'darwin)
         "-*-D2Coding-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
        ((and (eq system-type 'gnu/linux)
              (string= system-name "archlinux.local"))
         "-RIXF-D2Coding-normal-normal-normal-*-32-*-*-*-d-0-iso10646-1")
        ((and (eq system-type 'gnu/linux)
              (string-prefix-p "fedora" system-name))
         "-RIXF-D2Coding-normal-normal-normal-*-16-*-*-*-d-0-iso10646-1")))
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist `(font . ,my-font))
  (set-fontset-font "fontset-default" '(#xAC00 . #xD7AF) "D2Coding")
  (set-fontset-font "fontset-default" '(#x3131 . #x319E) "D2Coding"))


(setq default-korean-keyboard "3")
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)

(global-linum-mode t)
(unless (display-graphic-p)
  (setq linum-format "%d "))
(add-hook 'eshell-mode-hook
          (lambda () (linum-mode -1)))
(add-hook 'term-mode-hook
          (lambda () (linum-mode -1)))

(setq whitespace-style
      '(face
        tabs
        trailing
        newline
        empty))
(global-whitespace-mode 1)

(setq inhibit-startup-message t)

(load-theme 'sanityinc-tomorrow-bright t)


;; Language specific Settings
;; ------------------------------
(setq css-indent-offset 2)
(setq python-shell-interpreter "python3")

;; Functional Settings
;; ------------------------------
(defun eshell-new ()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(setq tramp-default-method "ssh")

(setq custom-file (make-temp-file "emacs-custom"))

;; Better Defaults
;; ------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq-default indent-tabs-mode nil)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; init.el end
;; --------------------------------------------------------------------------
