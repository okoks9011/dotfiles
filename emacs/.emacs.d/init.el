;;
;; init.el start
;; --------------------------------------------------------------------------
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;
;; Auxiliary functions
;; --------------------------------------------------------------------------
(defun read-lines (filePath)
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun indent-marked-files ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (replace-string "\t" "    " nil (point-min) (point-max))
    (delete-trailing-whitespace (point-min) (point-max))
    (indent-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))

;;
;; Packages
;; --------------------------------------------------------------------------
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-bright t))

(use-package org
  :ensure t
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :init
  (setq org-hide-leading-stars t)
  (setq org-log-done t)
  (setq org-startup-truncated nil)
  (let ((agenda-list-file "~/.agenda"))
    (when (file-exists-p agenda-list-file)
      (setq org-agenda-files (read-lines agenda-list-file))))
  :config
  (require 'ox-md))

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)))

(use-package eshell-git-prompt
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . 'er/expand-region)))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  (add-hook 'web-mode-hook 'electric-pair-mode)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq flycheck-python-flake8-executable "python3")
  (setq flycheck-python-pycompile-executable "python3")
  (setq flycheck-python-pylint-executable "python3")
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-global-modes '(python-mode web-mode js-mode css-mode))
  (setq-default flycheck-disabled-checkers '(javascript-jshint)))

(use-package intero
  :ensure t
  :requires flycheck
  :init
  (add-hook 'haskell-mode-hook 'intero-mode)
  :config
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(use-package jedi
  :ensure t
  :bind
  (:map ac-mode-map
        ("M-/" . auto-complete))
  :init
  (setq ac-auto-start nil)
  (setq jedi:use-shortcuts t)
  (add-hook 'python-mode-hook 'jedi:setup))

(use-package peep-dired
  :ensure t
  :bind
  (:map dired-mode-map
        ("P" . peep-dired)))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'org-mode-hook 'yas-minor-mode)
  :config
  (yas-reload-all))

(use-package ggtags
  :ensure t
  :init
  (add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1)))))

(use-package google-c-style
  :ensure t
  :init
  (add-hook 'c-mode-common-hook (lambda () (setq c-basic-offset 4)))
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode))

(use-package markdown-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode)))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))

(use-package smex
  :ensure t
  :config
  (smex-initialize))

(use-package js2-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package rust-mode
  :ensure t)


;;
;; Custom Settings
;; --------------------------------------------------------------------------

;; UI Settings
;; ------------------------------
(defvar my-font
  (cond ((eq system-type 'darwin)
         "-*-D2Coding-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
        ((and (eq system-type 'gnu/linux)
              (string= (system-name) "archlinux.local"))
         "-RIXF-D2Coding-normal-normal-normal-*-32-*-*-*-d-0-iso10646-1")
        ((eq system-type 'gnu/linux)
         "-RIXF-D2Coding-normal-normal-normal-*-16-*-*-*-d-0-iso10646-1")))
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (set-frame-font my-font nil t)
  (set-fontset-font "fontset-default" '(#xAC00 . #xD7AF) "D2Coding")
  (set-fontset-font "fontset-default" '(#x3131 . #x319E) "D2Coding")
  (let* ((half-screen-width (/ (x-display-pixel-width) 2))
         (my-char-cnt 160)
         (my-left (max 50
                       (- half-screen-width (* my-char-cnt (frame-char-width)) 50))))
    (setq initial-frame-alist
          `((top . 10)
            (left . ,my-left)
            (width . ,my-char-cnt)
            (height . 110)))))

(setq default-korean-keyboard "3")
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

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


;; Language specific Settings
;; ------------------------------
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq python-shell-interpreter "python3")
(add-hook 'python-mode-hook 'electric-pair-mode)

(defun python-pipenv-interpreter-toggle ()
  "Toggle default python-shell-interpreter value and pipenv run python"
  (interactive)
  (if-let ((saved (get 'python-pipenv-interpreter-toggle 'state)))
      (progn
        (setq python-shell-interpreter saved
              python-shell-interpreter-args nil)
        (put 'python-pipenv-interpreter-toggle 'state nil)
        (message "python-pipenv-interpreter disabled!"))
    (progn
      (put 'python-pipenv-interpreter-toggle 'state python-shell-interpreter)
      (setq python-shell-interpreter "pipenv"
            python-shell-interpreter-args "run python")
      (message "python-pipenv-interpreter enabled!"))))
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-e") 'python-pipenv-interpreter-toggle))


;; Functional Settings
;; ------------------------------
(defun eshell-new ()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(setq tramp-default-method "ssh")

(setq custom-file (make-temp-file "emacs-custom"))

(setenv "LANG" "ko_KR.UTF-8")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; Better Defaults
;; ------------------------------
(setq-default indent-tabs-mode nil)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)

;; init.el end
;; --------------------------------------------------------------------------
