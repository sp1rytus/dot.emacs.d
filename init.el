;; パッケージのインストーラーの設定
(require 'package)
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/")          t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/")   t)
(add-to-list 'package-archives '("marmalade"    . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; common-lisp 系の関数設定
(require 'cl)

;; 必要パッケージのリスト
(defvar installing-package-list
  '(
    init-loader
    ;;-- 10
    ;; folding
    ;; auto-complete
    ;; auto-highlight-symbol
    ;; color-theme
    ;; foreign-regexp
    ;; --20
    bazel-mode
    scss-mode
    scala-mode
    web-mode
    ;; apache-mode
    ;; google-c-style
    ;; jade-mode
    ;; js2-mode
    ;; json-mode
    ;; markdown-mode
    ;; php-mode
    ;; popwin
    ;; rainbow-mode
    ;; swift-mode
    ;; smarty-mode
    ;; ssh-config-mode
    ;; yaml-mode
    ;; web-mode
    ;; coffee-mode
    ;;-- 30
    company
    ;; flyspell
    ;; flycheck
    ;; flycheck-pos-tip
    ;; google-translate
    ;;-- 40
    ;; ensime
    ;; sbt-mode
    ;;-- others
    ;; magit
    ;; web-beautify
    ))

;; 自動インストール設定
(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))

;; 各々の設定ファイルのロード
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages (quote (auto-install)))
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(vc-handled-backends nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "white")))))
