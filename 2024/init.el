;;--------------------------------------------------------------------------
;; Package Manager
;;--------------------------------------------------------------------------
;; パッケージ管理のセットアップ
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; straight.elのインストール
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-packageのインストールと初期設定
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; 自動的にパッケージをインストール
(setq use-package-always-ensure t)

;; use-packageとstraight.elの統合
(setq straight-use-package-by-default t)

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

;; dired でディレクトリを先に表示する
(setq dired-listing-switches "-al --group-directories-first")

;; コマンドサポート
(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.5) ;; ポップアップが表示されるまでの遅延時間を設定
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)) ;; ポップアップの表示位置を設定

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


;;--------------------------------------------------------------------------
;; Proguraming Tools
;;--------------------------------------------------------------------------
;; 末尾に改行が必ずあるように
(setq require-final-newline t)

;; 行末の空白を表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "brightwhite")

;; インデントをスペースに(タブ幅4)
(defun my-c-mode-hook ()
  (c-set-style "linux")
  (setq c-basic-offset tab-width))
(add-hook 'c-mode-hook 'my-c-mode-hook)
(setq-default tab-width 2 indent-tabs-mode nil)

;; インデントして次の行に移動する
(defun indent-and-next-line ()
  (interactive)
  (indent-according-to-mode)
  (forward-line 1))

;; タブと全角スペースに色を付けて目立たせる
(defface my-face-b-1 '((t (:background "brightwhite"))) "Face for highlighting full-width spaces.")
(defface my-face-b-2 '((t (:background "brightblack"))) "Face for highlighting tab characters.")
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defun my-highlight-tabs-and-fullwidth-spaces ()
  "Highlight tab characters and full-width spaces."
  (font-lock-add-keywords nil
                          '(("　" 0 my-face-b-1 append)
                            ("\t" 0 my-face-b-2 append))))
(add-hook 'font-lock-mode-hook 'my-highlight-tabs-and-fullwidth-spaces)

;; Github Copilot
(use-package copilot
  :straight (copilot :type git :host github :repo "zerolfx/copilot.el" :files ("*.el" "dist"))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  :config
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))


;; OpenAI ChatGPT
(use-package openai
  :straight (openai :type git :host github :repo "emacs-openai/openai" :files ("*.el"))
  :config
  ;; OpenAI APIキーを設定
  (setq openai-key (getenv "OPENAI_API_KEY"))

  ;; 選択範囲を質問としてChatGPTに送り、回答を挿入する関数
  (defun ask-chatgpt-and-insert-response (start end)
    "Send the selected region as a prompt to ChatGPT and insert the response."
    (interactive "r")
    (let ((selected-text (buffer-substring-no-properties start end)))
      (openai-completion
       selected-text
       (lambda (response)
         (insert (cdr (assq 'text (aref (cdr (assq 'choices response)) 0)))))
       :model "gpt-3.5-turbo-instruct"
       :max-tokens 1000
       )))
  )

;; Magit
(use-package magit
  :ensure t
  :config
  (use-package copilot
    :ensure t
    :hook (magit-commit-mode . copilot-suggest-commit-message)))

