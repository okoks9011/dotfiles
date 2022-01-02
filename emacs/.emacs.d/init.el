;;
;; init.el start
;; --------------------------------------------------------------------------
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

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
    (delete-trailing-whitespace)
    (unless (s-suffix? ".py" file)
      (replace-string "\t" "    " nil (point-min) (point-max))
      (indent-region (point-min) (point-max)))
    (save-buffer)
    (kill-buffer nil)))

(defun pyvenv-autoload ()
  "Automatically activates pyvenv version if .venv directory exists."
  (f-traverse-upwards
   (lambda (path)
     (let ((venv-path (f-expand ".venv" path)))
       (if (f-exists? venv-path)
           (progn
             (pyvenv-activate venv-path)
             (setq flycheck-python-flake8-executable (concat venv-path "/bin/flake8"))
             t))))))

(defun kickstart-get-candiname (f-attrib)
  (car (split-string (car f-attrib) "-")))

(defun kickstart-rename-files ()
  (interactive)
  (let ((all-files (directory-files-and-attributes "." nil "-.*\.txt$"))
        )
    (dolist (candi (seq-group-by 'kickstart-get-candiname all-files))
      (let* ((candi-name (car candi))
             (candi-files (cdr candi))
             (sorted-files (cl-sort candi-files
                                    'time-less-p
                                    :key (lambda (x) (nth 5 x))))
             (numbered-files (cl-mapcar 'cons
                                        (number-sequence 0 (length candi-files))
                                        sorted-files)))
        (dolist (numbered-file numbered-files)
          (let ((old-name (nth 1 numbered-file))
                (new-name (concat
                           (string (+ ?A (car numbered-file)) ?.)
                           candi-name
                           ".cc")))
            (rename-file old-name new-name)))))))

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
  :ensure org-plus-contrib
  :after ox-gfm
  :defer 7
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :init
  (setq org-hide-leading-stars t)
  (setq org-log-done t)
  (setq org-startup-truncated nil)
  (setq org-export-with-sub-superscripts nil)
  (let ((agenda-list-file "~/.agenda"))
    (when (file-exists-p agenda-list-file)
      (setq org-agenda-files (read-lines agenda-list-file))))
  :config
  (add-to-list 'org-file-apps '(directory . emacs)))

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
  (setq flycheck-checkers
        (cons 'python-pylint (remove 'python-pylint flycheck-checkers)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-global-modes '(python-mode web-mode js-mode css-mode))
  (setq-default flycheck-disabled-checkers '(javascript-jshint)))

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-modules
        (remove 'elpy-module-flymake elpy-modules)))

(use-package f
  :ensure t)

(use-package peep-dired
  :ensure t
  :after (dired)
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

(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char)))

(use-package go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'go-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        (remove 'tabs whitespace-style))))
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package dash-at-point
  :ensure t
  :bind
  (("C-c d" . dash-at-point))
  :init
  (add-hook 'go-mode-hook (lambda () (setq dash-at-point-docset "go"))))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c p")
  (setq lsp-file-watch-threshold 4000)
  :hook
  ((go-mode . lsp))
  :commands
  lsp)

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

(use-package  lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-erros-list)

(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package fsharp-mode
  :defer t
  :ensure t)

(use-package eglot-fsharp
  :ensure t)

(use-package ox-gfm
  :ensure t)

(use-package sudo-edit
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package protobuf-mode
  :ensure t)

(use-package gotest
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
  (set-face-attribute 'fixed-pitch nil :family "D2Coding")
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
(setq default-buffer-file-coding-system 'utf-8-unix)
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")

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
(add-hook 'python-mode-hook 'pyvenv-autoload)

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
(setq tramp-default-method "ssh")

(setq custom-file (make-temp-file "emacs-custom"))

(setq explicit-shell-file-name "/bin/bash")

(setenv "LANG" "ko_KR.UTF-8")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key [remap dabbrev-expand] 'hippie-expand)


;; Specific
;; ------------------------------
(setq custom-el-path "~/.emacs.d/specific")
(add-to-list 'load-path custom-el-path)
(if (file-directory-p custom-el-path)
    (dolist (custom-el (directory-files custom-el-path))
      (if (string-suffix-p ".el" custom-el)
          (load custom-el))))


;; Better Defaults
;; ------------------------------
(setq-default indent-tabs-mode nil)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M-m") 'shell)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; init.el end
;; --------------------------------------------------------------------------
