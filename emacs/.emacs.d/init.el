;;
;; init.el start
;; --------------------------------------------------------------------------
;;
;; Initialize straight.el
;; --------------------------------------------------------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

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
    :straight t
    :init
    (setq shell-file-name "/bin/zsh")
    :config
    (exec-path-from-shell-initialize)))

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :config
  (load-theme 'sanityinc-tomorrow-bright t))

(use-package org
  :straight t
  :after ox-gfm
  :defer 7
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-switchb))
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
  :straight t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package magit
  :straight t
  :bind
  (("C-x g" . magit-status)))

(use-package eshell-git-prompt
  :straight t
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package expand-region
  :straight t
  :bind
  (("C-=" . 'er/expand-region)))

(use-package web-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  (add-hook 'web-mode-hook 'electric-pair-mode)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package flycheck
  :straight t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq flycheck-checkers
        (cons 'python-pylint (remove 'python-pylint flycheck-checkers)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-global-modes '(python-mode web-mode js-mode css-mode))
  (setq-default flycheck-disabled-checkers '(javascript-jshint)))

(use-package elpy
  :straight t
  :init
  (elpy-enable)
  :config
  (setq elpy-modules
        (remove 'elpy-module-flymake elpy-modules))
  (setq elpy-rpc-python-command "/usr/bin/python3"))

(use-package f
  :straight t)

(use-package peep-dired
  :straight t
  :after (dired)
  :bind
  (:map dired-mode-map
        ("P" . peep-dired)))

(use-package yasnippet
  :straight t
  :init
  (add-hook 'org-mode-hook 'yas-minor-mode)
  :config
  (yas-reload-all))

(use-package ggtags
  :straight t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

(use-package google-c-style
  :straight t
  :init
  (add-hook 'c-mode-common-hook (lambda () (setq c-basic-offset 4)))
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(use-package rainbow-delimiters
  :straight t
  :init
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :straight t
  :init
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode)
  :bind
  (:map paredit-mode-map
        ("RET" . nil)
        ("C-j" . paredit-new-line)))

(use-package markdown-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode)))

(use-package ivy
  :straight t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (ivy-mode 1))

(use-package swiper
  :straight t
  :bind
  (("C-s" . swiper)))

(use-package counsel
  :straight t
  :config
  (counsel-mode 1)
  ;; Using :bind breaks counsel key map.
  (bind-key "C-c g" 'counsel-git)
  (bind-key "C-c j" 'counsel-git-grep))

(use-package smex
  :straight t
  :config
  (smex-initialize))

(use-package avy
  :straight t
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-g f" . avy-goto-line)))

(use-package go-mode
  :straight t
  :bind
  (("C-c m" . go-test-current-file)
   ("C-c ." . go-test-current-test))
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion)
        ("TAB" . copilot-accept-completion)
        ("C-<tab>" . copilot-accept-completion-by-word)
        ("C-TAB" . copilot-accept-completion-by-word))
  :init
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'go-mode-hook (lambda () (setq lsp-go-env '((GOFLAGS . "-tags=integration")))))
  (add-hook 'go-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        (remove 'tabs whitespace-style))))
  (add-hook 'go-mode-hook 'copilot-mode)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package dash-at-point
  :straight t
  :bind
  (("C-c d" . dash-at-point))
  :init
  (add-hook 'go-mode-hook (lambda () (setq dash-at-point-docset "go"))))

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c p")
  (setq lsp-file-watch-threshold 9000)
  ;; (setq lsp-eldoc-enable-hover nil)
  :hook
  ((go-mode . lsp)
   (haskell-mode . lsp)
   (fsharp-mode . lsp)
   (lsp-mode . yas-minor-mode))
  :commands
  lsp)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package  lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-erros-list)

(use-package dumb-jump
  :straight t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package flycheck-golangci-lint
  :straight t
  :hook
  (go-mode . flycheck-golangci-lint-setup))

(use-package proof-general
  :straight t
  :init
  (setq proof-splash-enable nil)
  (setq proof-electric-terminator-enable t))

(use-package which-key
  :straight t
  :config
  (which-key-mode +1))

;; (use-package ace-window
;;   :straight t
;;   :bind
;;   ([remap other-window] . ace-window))

(use-package dap-mode
  :straight t
  :init
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  (require 'dap-dlv-go)
  (dap-register-debug-template
   "Go Dlv Test Current Function Configuration With Integration Tag"
   (list :type "go"
         :request "launch"
         :name "Test function with integration tag"
         :mode "test"
         :program nil
         :args nil
         :env '((GOFLAGS . "-tags=integration"))))
  (dap-register-debug-template
  "Launch managefnt with test env"
  (list :type "go"
        :request "launch"
        :name "Launch Executable"
        :mode "exec"
        :program "../../bin/managefnt"
        :args "-config=config/config.test.us"
        :env '(("GOENV" . "test")))))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el")))

(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (concat my/workspace "/wiki_logs/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; (use-package org-excalidraw
;;   :straight (:type git :host github :repo "wdavew/org-excalidraw")
;;   :config
;;   (org-excalidraw-directory (concat my/workspace "/wiki_logs/excalidraw")))

(use-package fsharp-mode
  :defer t
  :straight t)

(use-package ellama
  :straight t
  :init
  (setopt ellama-keymap-prefix "C-c e")
  (setopt ellama-lanuage "Korean")
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           ;; this model should be pulled to use it
           ;; value should be the same as you print in terminal during pull
           :chat-model "llama3-instruct-8b:latest"
           :embedding-model "nomic-embed-text"
           ;; :default-chat-non-standard-params '(("num_ctx" . 8192))
)))

(use-package yafolding
  :straight t
  :bind (("C-c <C-return>" . yafolding-toggle-element)))

(use-package pdf-tools
  :straight t)

(use-package lsp-haskell
  :straight t)

(use-package ox-gfm
  :straight t)

(use-package sudo-edit
  :straight t)

(use-package js2-mode
  :straight t)

(use-package json-mode
  :straight t)

(use-package yaml-mode
  :straight t)

(use-package dockerfile-mode
  :straight t)

(use-package protobuf-mode
  :straight t)

(use-package gotest
  :straight t)

(use-package restclient
  :straight t)

(use-package terraform-mode
  :straight t)


;;
;; Custom Settings
;; --------------------------------------------------------------------------

;; `image-type: Invalid image type ‘svg’` hotfix
;; -----------------------------
(add-to-list 'image-types 'svg)

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
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")

(global-display-line-numbers-mode t)
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
(setq split-height-threshold 120)
(setq split-width-threshold 240)


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

;; LSP linter chaining
;; ------------------------------
(defvar-local my/flycheck-local-cache nil)
(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))
(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'go-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (golangci-lint)))))))))

;; Functional Settings
;; ------------------------------
(setq tramp-default-method "ssh")

(setq custom-file (make-temp-file "emacs-custom"))

(setq explicit-shell-file-name "/bin/bash")

(setenv "LANG" "en_US.UTF-8")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M-m") 'shell)


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

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; init.el end
;; --------------------------------------------------------------------------
