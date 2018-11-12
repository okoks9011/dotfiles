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
    exec-path-from-shell
    org
    magit
    eshell-git-prompt
    ox-twbs
    expand-region
    flycheck
    js2-mode
    json-mode
    web-mode
    intero
    yaml-mode
    dockerfile-mode
    jedi
    ag
    peep-dired
    yasnippet
    smex
    ggtags
    google-c-style
    clojure-mode
    rainbow-delimiters
    paredit))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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
    (indent-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))

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
(with-eval-after-load 'org
  (require 'ox-md nil t))

(let ((agenda-list-file "~/.agenda"))
  (when (file-exists-p agenda-list-file)
    (setq org-agenda-files (read-lines agenda-list-file))))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; eshell-git-prompt
(eshell-git-prompt-use-theme 'powerline)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-hook 'web-mode-hook 'electric-pair-mode)

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(setq flycheck-python-flake8-executable "python3")
(setq flycheck-python-pycompile-executable "python3")
(setq flycheck-python-pylint-executable "python3")
(with-eval-after-load 'flycheck
  (setq flycheck-global-modes '(python-mode web-mode js-mode css-mode))
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; expand-region
(require' expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; intero
(add-hook 'haskell-mode-hook 'intero-mode)
(with-eval-after-load 'intero
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

;; jedi
(setq ac-auto-start nil)
(with-eval-after-load 'auto-complete
  (define-key ac-mode-map (kbd "M-/") 'auto-complete))
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)

;; peep-dired
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "P") 'peep-dired))

;; yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'org-mode-hook #'yas-minor-mode)

;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; ggtags
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; google-c-style
(add-hook 'c-mode-common-hook
          (lambda () (setq c-basic-offset 4)))
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; rainbow-delimiters
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;; paredit
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'ielm-mode-hook #'paredit-mode)

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
              (string-prefix-p "fedora" (system-name)))
         "-RIXF-D2Coding-normal-normal-normal-*-16-*-*-*-d-0-iso10646-1")))
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist `(font . ,my-font))
  (set-fontset-font "fontset-default" '(#xAC00 . #xD7AF) "D2Coding")
  (set-fontset-font "fontset-default" '(#x3131 . #x319E) "D2Coding")
  (setq initial-frame-alist
        '((width . 160) (height . 110))))


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

(load-theme 'sanityinc-tomorrow-bright t)


;; Language specific Settings
;; ------------------------------
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq python-shell-interpreter "python3")

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

(setq ido-everywhere t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)

;; Better Defaults
;; ------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(setq-default indent-tabs-mode nil)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)

(ido-mode t)
(setq ido-enable-flex-matching t)

;; init.el end
;; --------------------------------------------------------------------------
