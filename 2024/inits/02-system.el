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

