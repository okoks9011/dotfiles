;;; -*- lexical-binding: t -*-
;;
;; init.el start
;; --------------------------------------------------------------------------
;;
;; Initialize straight.el
;; --------------------------------------------------------------------------
;; Shallow clone: protobuf-mode/dune pull entire monorepos otherwise
(setq straight-vc-git-default-clone-depth 1)
;; Skip `find' over every repo at startup; edits made outside Emacs
;; need M-x straight-rebuild-package
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-use-package-by-default t)

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
(setq use-package-compute-statistics t)

;; Emacs 29 ships these; straight copies clash with built-in eglot
(dolist (pkg '(project xref flymake eldoc jsonrpc eglot external-completion))
  (add-to-list 'straight-built-in-pseudo-packages pkg))


;;
;; Auxiliary functions
;; --------------------------------------------------------------------------
(defvar my/workspace (expand-file-name "~/workspace"))

(defun my/read-lines (filePath)
  (with-temp-buffer
    (insert-file-contents filePath)
    (string-lines (buffer-string) t)))

(defun my/indent-marked-files ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (delete-trailing-whitespace)
    (unless (string-suffix-p ".py" file)
      (let ((tab-width 4))
        (untabify (point-min) (point-max)))
      (indent-region (point-min) (point-max)))
    (save-buffer)
    (kill-buffer nil)))

(defun my/copy-file-path-relative-to-git-root (&optional arg)
  "Copy the current file path with line number relative to the git repository root."
  (interactive "P")
  (if-let* ((file (buffer-file-name))
             (git-root (string-trim
                        (shell-command-to-string "git rev-parse --show-toplevel")))
             (_ (not (string-match-p "^fatal:" git-root)))
             (relative-path (file-relative-name file git-root))
             (path-with-line (format "%s:L%d" relative-path (line-number-at-pos)))
             (result (if arg
                         (format "%s `%s`" path-with-line (string-trim (thing-at-point 'line t)))
                       path-with-line)))
      (progn
        (kill-new result)
        (message "Copied: %s" result))
    (message "Not in a git repository or buffer has no file")))

(defun my/go-mod-root ()
  "Return the nearest directory contains go.mod if the current buffer is a Go file."
  (when (and buffer-file-name
             (derived-mode-p 'go-mode 'go-ts-mode))
    (when-let* ((dir (locate-dominating-file default-directory "go.mod")))
      (expand-file-name dir))))

(defun my/magit-worktree-remember (directory &rest _)
  "Add the newly created worktree DIRECTORY to the project.el project list."
  (let ((dir (file-name-as-directory (expand-file-name directory))))
    (when (file-directory-p dir)
      (if-let* ((pr (project-current nil dir)))
          (let ((root (expand-file-name (project-root pr))))
            (project-remember-project pr)
            (unless (file-equal-p root dir)
              (message "Warning: Root resolved to %s (worktree %s)" root dir)))
        (message "project.el failed to recognize %s as a project" dir)))))

(defun my/wslp ()
  "Return t if Emacs is running in WSL2, nil otherwise."
  (and
   ;; Check if the system is Linux
   (eq system-type 'gnu/linux)
   ;; Check for WSL-specific file
   (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop")
   ;; Check for WSL2-specific file
   (file-exists-p "/run/WSL")
   ;; Additional check: look for Windows-style root directory
   (file-directory-p "/mnt/c")))

(when (my/wslp)
  ; WSLg breaks copy-paste from Emacs into Windows
  ; see: https://www.lukas-barth.net/blog/emacs-wsl-copy-clipboard/
  (setq select-active-regions nil
        select-enable-clipboard 't
        select-enable-primary nil
        interprogram-cut-function #'gui-select-text))

;; To mitigate broken encoding while using default kill-region
(defun my/copy-region-to-clipboard (beg end)
  "Copy region to Wayland clipboard using wl-copy."
  (interactive "r")
  (let ((coding-system-for-write 'utf-8-unix))
    (call-process-region
     beg end
     "wl-copy"
     nil nil nil
     "--type" "text/plain;charset=utf-8"))
  (message "Copied to clipboard!"))

(keymap-global-set "C-c y" #'my/copy-region-to-clipboard)

(defmacro my/use-package-deferred (&rest forms)
  "Add `:defer t' to each `use-package' form in FORMS."
  `(progn ,@(mapcar (lambda (f) `(,@f :defer t)) forms)))

;;
;; Packages
;; --------------------------------------------------------------------------
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :init
    (setq shell-file-name "/bin/zsh")
    :config
    (exec-path-from-shell-initialize)))

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-bright t))

(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-switchb))
  :init
  (setq org-log-done t)
  (setq org-startup-truncated nil)
  (setq org-export-with-sub-superscripts nil)
  (let ((agenda-list-file "~/.agenda"))
    (when (file-exists-p agenda-list-file)
      (setq org-agenda-files (my/read-lines agenda-list-file)))))

(use-package magit
  :bind
  (("C-x g" . magit-status))
  :config
  (advice-add #'magit-worktree-checkout :after #'my/magit-worktree-remember)
  (advice-add #'magit-worktree-branch   :after #'my/magit-worktree-remember))

;; Considered switching to `expreg', but it doesn't support contracting the
;; region with a negative prefix (`C-- C-='), so keep `expand-region'.
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-global-modes
                '(web-mode js-mode js-ts-mode css-mode css-ts-mode))
  (setq-default flycheck-disabled-checkers '(javascript-jshint)))

(use-package dired-preview
  :after (dired)
  :bind
  (:map dired-mode-map
        ("P" . dired-preview-mode)))

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'racket-mode-hook #'paredit-mode)
  (add-hook 'racket-repl-mode-hook #'paredit-mode)
  :bind
  (:map paredit-mode-map
        ("RET" . nil)
        ("C-j" . paredit-new-line)))

(use-package savehist
  :straight nil
  :config
  (savehist-mode 1))

(use-package recentf
  :straight nil
  :config
  (recentf-mode 1))

(use-package vertico
  :config
  (vertico-mode 1))

(use-package vertico-directory
  :straight nil
  :after vertico
  :bind
  (:map vertico-map
        ("RET"   . vertico-directory-enter)
        ("DEL"   . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides
        '((file (styles basic partial-completion))))
  (setq completion-category-defaults nil))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  :bind
  (("C-s"   . consult-line)
   ("C-x b" . consult-buffer)
   ("M-y"   . consult-yank-pop)
   ("M-g g" . consult-goto-line)
   ("M-g i" . consult-imenu)
   ;; ("C-c g" . project-find-file)
   ;; ("C-c j" . consult-git-grep)
   ;; ("C-c r" . consult-ripgrep)
   )
  :config
  (consult-customize consult-source-recent-file :preview-key "M-."))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-lsp
  :commands consult-lsp-symbols)

;; (use-package ivy
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "%d/%d ")
;;   (ivy-mode 1))

;; (use-package swiper
;;   :bind
;;   (("C-s" . swiper)))

;; (use-package counsel
;;   :config
;;   (counsel-mode 1)
;;   ;; Using :bind breaks counsel key map.
;;   (bind-key "C-c g" 'counsel-git)
;;   (bind-key "C-c j" 'counsel-git-grep))

(use-package avy
  :bind
  (("C-:" . avy-goto-char-timer)
   ("M-g f" . avy-goto-line)
   :map isearch-mode-map
   ("C-'" . avy-isearch)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c p")
  (setq lsp-file-watch-threshold 10000)
  ;; (setq lsp-eldoc-enable-hover nil)
  :hook
  ((go-mode . lsp)
   (fsharp-mode . lsp)
   (lsp-mode . yas-minor-mode)
   (rustic-mode . lsp))
  :config
  (advice-add #'lsp--suggest-project-root :before-until #'my/go-mod-root)
  :commands
  lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; (use-package  lsp-ivy
;;   :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package flycheck-golangci-lint
  :hook
  (go-mode . flycheck-golangci-lint-setup))

(use-package which-key
  :straight nil
  :config
  (which-key-mode +1))

(use-package org-roam
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

(use-package hideshow
  :straight nil
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-c <C-return>" . hs-toggle-hiding)))

(use-package ghostel
  :init
  (add-hook 'ghostel-mode-hook #'ghostel-ime-mode)
  :bind (("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ;; ("C-s"  . swiper)
         ("C-k"  . ghostel-send-C-k-and-kill)
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :config
  (defun ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))
  :hook
  (ghostel-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (ghostel-shell (executable-find "fish")))

(use-package ghostel-eshell
  :straight nil
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs
        (seq-difference treesit-auto-langs '(go gomod ocaml)))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  ;; auto-mode-alist 'all already picks ts-modes by extension; the
  ;; per-file remap rebuild only probes all 60 grammars on every visit.
  (advice-remove #'set-auto-mode-0 #'treesit-auto--set-major-remap))

(use-package flymake-ruff
  :hook
  (eglot-managed-mode . flymake-ruff-load))

(use-package eglot
  :straight nil
  :hook
  (python-base-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) "basedpyright-langserver" "--stdio")))

(use-package ocaml-eglot
  :hook
  (tuareg-mode . ocaml-eglot-mode)
  (ocaml-eglot-mode . eglot-ensure))

(use-package ox-gfm
  :after org)

;; Explicit deferred packages
;; ------------------------------
(my/use-package-deferred
 (use-package org-modern
   :hook
   ((org-mode . org-modern-mode)
    (org-agenda-finalize . org-modern-agenda))
   :custom
   (org-modern-star 'replace))

 (use-package web-mode
   :init
   (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
   (add-hook 'web-mode-hook #'electric-pair-local-mode)
   :config
   (setq web-mode-markup-indent-offset 2)
   (setq web-mode-css-indent-offset 2)
   (setq web-mode-code-indent-offset 2))

 (use-package markdown-mode
   :init
   (add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode)))

 (use-package restclient
   :init
   (add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode)))

 (use-package yasnippet
   :init
   (add-hook 'org-mode-hook #'yas-minor-mode)
   :config
   (yas-reload-all))

 (use-package rainbow-delimiters
   :init
   (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
   (add-hook 'racket-mode-hook #'rainbow-delimiters-mode))

 (use-package dumb-jump
   :init
   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 90))

 (use-package dap-mode
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

 (use-package python
   :straight nil
   :custom
   (python-shell-interpreter "uv")
   (python-shell-interpreter-args "run python -i")
   (python-shell-dedicated 'project)
   ;; Detection runs `uv -i', which fails; the default prompt regexps already match CPython
   (python-shell-prompt-detect-enabled nil))

 (use-package go-mode
   :init
   (setq lsp-go-env '((GOFLAGS . "-tags=integration")))
   (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
   (add-hook 'go-mode-hook (lambda ()
                             (setq-local whitespace-style
                                         (remove 'tabs whitespace-style))))
   (setq gofmt-command "goimports")
   :config
   (add-hook 'before-save-hook #'gofmt-before-save))

 (use-package gotest
   :after go-mode
   :bind
   (:map go-mode-map
         ("C-c m" . go-test-current-file)
         ("C-c ." . go-test-current-test)))

 (use-package racket-mode
   :hook (racket-mode . racket-xp-mode))

 (use-package wgrep)

 (use-package rustic)

 (use-package fsharp-mode)

 (use-package tuareg)

 (use-package dune)

 (use-package protobuf-mode)

 (use-package terraform-mode)

 (use-package fish-mode)

 (use-package lua-mode)

 (use-package sudo-edit)

 (use-package open-junk-file))



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
        ((and (eq system-type 'gnu/linux)
              (string-prefix-p "cachyos" (system-name)))
         "D2Coding-12"
         ;; "-RIXF-D2Coding-regular-normal-normal-*-29-*-*-*-d-0-iso10646-1"
         ;; "-RIXF-D2Coding-regular-normal-normal-*-20-*-*-*-d-0-iso10646-1"
         )
        ((eq system-type 'gnu/linux)
         "-RIXF-D2Coding-normal-normal-normal-*-16-*-*-*-d-0-iso10646-1")))

(defun my/setup-fonts ()
  (set-frame-font my-font nil t)
  (set-face-attribute 'fixed-pitch nil :family "D2Coding")
  (set-fontset-font t 'hangul "D2Coding" nil)
  ;; Nerd font
  (setq use-default-font-for-symbols nil)
  (dolist (range '((#x23fb . #x23fe) (#x2665 . #x2665) (#x26a1 . #x26a1)
                   (#x2b58 . #x2b58)
                   (#xe000 . #xf8ff)
                   (#xf0000 . #xffffd)))
    (set-fontset-font t range "Symbols Nerd Font Mono" nil)))

(defun my/setup-frame ()
  (let* ((half-screen-width (/ (x-display-pixel-width) 2))
         (my-char-cnt 160)
         (my-left (max 50
                       (- half-screen-width (* my-char-cnt (frame-char-width)) 50))))
    ;; Setting initial-frame-alist here might not work as the initial frame is already created.
    ;; Consider moving this into early-init.el
    (setq initial-frame-alist
          `((top . 10)
            (left . ,my-left)
            (width . ,my-char-cnt)
            (height . 110)))))

(when (display-graphic-p)
  (my/setup-fonts)
  (my/setup-frame))

(setq default-korean-keyboard "3")
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")

(global-display-line-numbers-mode t)

(setq whitespace-style
      '(face
        tabs
        trailing
        newline
        empty))
(global-whitespace-mode 1)

(setq inhibit-startup-screen t)
(setq split-height-threshold 120)
(setq split-width-threshold 240)


;; Language specific Settings
;; ------------------------------
(setq css-indent-offset 2)
(setq js-indent-level 2)


;; LSP linter chaining
;; ------------------------------
(defvar-local my/flycheck-local-cache nil)
(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))
(advice-add #'flycheck-checker-get :around #'my/flycheck-checker-get)
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'go-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (golangci-lint)))))))))


;; Functional Settings
;; ------------------------------
(setq tramp-default-method "ssh")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(setenv "LANG" "en_US.UTF-8")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(keymap-global-set "<remap> <dabbrev-expand>" #'hippie-expand)
(keymap-global-set "C-c C-k" #'my/copy-file-path-relative-to-git-root)
;; (keymap-global-set "C-x m" #'eshell)
;; (keymap-global-set "C-x M-m" #'shell)

(setq ring-bell-function 'ignore)

;; Specific
;; ------------------------------
(setq custom-el-path (expand-file-name "specific" user-emacs-directory))
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
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "M-z" #'zap-up-to-char)

;; init.el end
;; --------------------------------------------------------------------------
