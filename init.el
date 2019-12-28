;; ~/.emacs.d/site-lisp 以下全部読み込み
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;; パッケージのインストーラーの設定
(require 'package)
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/")        t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
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
    ;; apache-mode
    ;; google-c-style
    ;; jade-mode
    ;; js2-mode
    json-mode
    ;; markdown-mode
    ;; php-mode
    ;; popwin
    ;; rainbow-mode
    scss-mode
    scala-mode
    ;; swift-mode
    ;; smarty-mode
    ;; ssh-config-mode
    ;; yaml-mode
    web-mode
    ;; coffee-mode
    ;;-- 30
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
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja")
 '(js-indent-level 2)
 '(markdown-command "mdown")
 '(menu-bar-mode nil)
 '(package-selected-packages (quote (auto-install)))
 ;; '(package-selected-packages
 ;;   (quote
 ;;    (web-beautify magit sbt-mode ensime google-translate flycheck-pos-tip flycheck coffee-mode web-mode yaml-mode ssh-config-mode smarty-mode scss-mode scala-mode rainbow-mode popwin php-mode markdown-mode json-mode js2-mode jade-mode google-c-style apache-mode foreign-regexp color-theme auto-highlight-symbol auto-complete folding init-loader)))
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(vc-handled-backends nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "white")))))
