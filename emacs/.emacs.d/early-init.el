;;; -*- lexical-binding: t -*-
;; To prevent package.el loading packages prior to their init-file loading.
;; It is from straight.el installation guide.
(setq package-enable-at-startup nil)

(push '(tool-bar-lines . 0) default-frame-alist)         ;;=> (tool-bar-mode -1)
(push '(menu-bar-lines . 0) default-frame-alist)         ;;=> (menu-bar-mode -1)
(push '(vertical-scroll-bars . nil) default-frame-alist) ;;=> (scroll-bar-mode -1)

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)))) ;; 100MB

;; Bundled libgccjit derives the macOS version from the Darwin kernel
;; (27 -> "18.0"), which clang rejects. Pin it explicitly.
;; Set it eagerly (not after `comp') so async native-comp workers inherit it.
(when (eq system-type 'darwin)
  (setq native-comp-driver-options '("-Wl,-w" "-mmacosx-version-min=26.0")))
