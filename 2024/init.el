;; パッケージ管理のセットアップ
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; use-packageのインストールと初期設定
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; 自動的にパッケージをインストール
(setq use-package-always-ensure t)

;;--------------------------------------------------------------------------
;; System
;;--------------------------------------------------------------------------
;; 文字エンコーディング
(set-language-environment   "Japanese")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; バックアップファイル設定
(setq make-backup-files t)

;; バックアップファイルと自動保存ファイルのディレクトリを指定
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup/")))

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/backup/" t)))

;; バックアップと自動保存に関するその他の設定
(setq backup-by-copying t)      ;; 元のファイルを変更せずにコピーでバックアップ
(setq delete-old-versions t)    ;; 古いバックアップファイルを削除
(setq kept-new-versions 6)      ;; 最新の6つのバックアップを保持
(setq kept-old-versions 2)      ;; 古いバージョンのうち2つを保持
(setq version-control t)        ;; バックアップファイルのバージョン管理を有効にする

;; コメントアウトの形式変更
(setq comment-style 'multi-line)

;; 改行コードを表示
(setq eol-mnemonic-dos  "(CRLF)")
(setq eol-mnemonic-mac  "(CR)")
(setq eol-mnemonic-unix "(LF)")

;;複数ウィンドウを開かないようにする
(setq ns-pop-up-frames nil)

;; TAB
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; ミニバッファ:単語単位での削除操作
(define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)

;; 通常のウィンドウで行を折り返さない
(setq-default truncate-lines t)

;; ウィンドウを左右に分割したときに行を折り返さない
(setq truncate-partial-width-windows t)

;; スクロール時の移動量を1に
(setq scroll-conservatively 101)
(setq scroll-step 1)

;; モードラインに列番号表示
(column-number-mode)

;; カーソルの位置が何行目かを表示する
(line-number-mode t)

;; 対応する括弧をハイライト
(setq show-paren-delay 0)
(show-paren-mode t)

;; 編集時のバッファ再読み込み
(global-auto-revert-mode 1)

;; Emacs の質問を y/n に
(fset 'yes-or-no-p 'y-or-n-p)

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; シンボリックリンクを開くときの質問省略
(setq vc-follow-symlinks t)

;; メニューバーを表示しない
(custom-set-variables
 '(display-time-mode t)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces)

;; スクロールバーを消す
(toggle-scroll-bar nil)

;;--------------------------------------------------------------------------
;; Color Theme
;;--------------------------------------------------------------------------
(use-package doom-themes
  :ensure t
  :config
  ;; カラーテーマを doom-dark+ に設定
  (load-theme 'doom-dark+ t)

  ;; 各種テーマの設定（オプション）
  (doom-themes-visual-bell-config) ;; 目に優しいビジュアルベルを有効化
  (doom-themes-org-config)         ;; org-modeのスタイルをカスタマイズ

  ;; カスタム色を設定
  (custom-set-faces
   '(default ((t (:background "#030303"))))))

;;--------------------------------------------------------------------------
;; Key binding
;;--------------------------------------------------------------------------
(global-set-key (kbd "<tab>") 'indent-for-tab-command)
(global-set-key (kbd "<f7>") 'enlarge-window-horizontally)
(global-set-key (kbd "<f8>") 'enlarge-window)
(global-set-key (kbd "<f11>") 'ibuffer)
(global-set-key (kbd "<f12>") 'undo)
(global-set-key (kbd "M-z") 'lsp)

(global-set-key (kbd "<help> c") 'helpful-command)
(global-set-key (kbd "<help> w") 'helm-man-woman)

(global-set-key (kbd "C-x <RET> u") 'revert-buffer-with-coding-system-utf-8-unix)
(global-set-key (kbd "C-x <RET> s") 'revert-buffer-with-coding-system-japanese-cp932-dos)
