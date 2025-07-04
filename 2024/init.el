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
(setq auto-save-default nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t)))

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

;; グローバルに行番号を表示する
(global-display-line-numbers-mode t)

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

;; GUI利用時のシェルの環境変数対応
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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
(global-set-key (kbd "<f7>") 'enlarge-window-horizontally)
(global-set-key (kbd "<f8>") 'enlarge-window)
(global-set-key (kbd "<f10>") 'my-openai-menu)
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
  (setq copilot-idle-delay 1) ;; 補完トリガーを遅らせる
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

;; LSP
(use-package lsp-mode
  :hook
  (lsp-mode . lsp-lens-mode)
  (php-mode . lsp-deferred)
  (scala-ts-mode . lsp)
  :config
  (setq lsp-prefer-flymake nil)                ;; Flycheck を使用するために Flymake を無効化
  (setq lsp-keep-workspace-alive nil)          ;; ワークスペースを維持しない
  (setq read-process-output-max (* 1024 1024)) ;; LSPの出力を増やす
  (setq lsp-idle-delay 1)                      ;; アイドル状態0.5秒後にLSPがトリガー
  (setq lsp-enable-file-watchers nil)          ;; ファイルウォッチャーを無効化
  (setq lsp-auto-guess-root nil)               ;; 自動で親のプロジェクトを探さない
  (setq lsp-headerline-breadcrumb-enable nil)  ;; パンクズ表示を無効化
  (setq lsp-use-plists nil)
  (setq lsp-clients-php-server-command '("intelephense" "--stdio")) ;; PHP設定
  )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  ;; FlyCheckの更新頻度を調整
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  (setq flycheck-idle-change-delay 0.5)  ;; テキスト変更後0.5秒でチェックを開始

  ;; FlyCheckがESLintを使うように設定
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; エラー行のハイライト
  (custom-set-faces
   '(flycheck-error
     ((t (:background "#FFCCCC" :foreground "#990000" :underline t :bold t))))
   '(flycheck-warning
     ((t (:background "#FFFFCC" :foreground "#996600" :underline t :bold t))))
   '(flycheck-info
     ((t (:background "#CCFFCC" :foreground "#006600" :underline t :bold t)))))
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-flycheck-live-reporting t)
)


;; OpenAI ChatGPT
(use-package openai
  :straight (openai :type git :host github :repo "emacs-openai/openai" :files ("*.el"))
  :config
  ;; OpenAI APIキーを設定
  (setq openai-key (getenv "OPENAI_API_KEY"))

  ;; 選択範囲を質問としてChatGPTに送り、回答を挿入する関数
  (defun ask-chatgpt-and-insert-response (prompt)
    "Send the selected region as a prompt to ChatGPT and insert the response."
    (interactive "r")
    (let ((text-buffer (concat (buffer-substring (region-beginning) (region-end)) "\n" prompt)))
      (openai-completion
       text-buffer
       (lambda (response)
         (insert (cdr (assq 'text (aref (cdr (assq 'choices response)) 0)))))
       :model "gpt-3.5-turbo-instruct"
       :max-tokens 1000
       )))
  )

;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))


;;--------------------------------------------------------------------------
;; Custom Menu
;;--------------------------------------------------------------------------
(use-package transient
  :ensure t
  :config
  (transient-define-prefix my-openai-menu ()
    "My custom menu."
    [
     ["Transalate"
      ("<f12>" "English"  (lambda () (interactive) (ask-chatgpt-and-insert-response "上の文章を英語に翻訳してください")))
      ("<f10>" "Japanese" (lambda () (interactive) (ask-chatgpt-and-insert-response "上の文章を日本語に翻訳してください")))
      ]
     ["Doc"
      ("<f11>" "English"  (lambda () (interactive) (ask-chatgpt-and-insert-response "上のコードを機能概要を言語に合わせたdocblock形式で、簡単にサマライズして英語で提案してください")))
      ("<f9>"  "Japanese" (lambda () (interactive) (ask-chatgpt-and-insert-response "上のコードを機能概要を言語に合わせたdocblock形式で、簡単にサマライズして日本語で提案してください")))
      ]]
    )
  )

;;--------------------------------------------------------------------------
;; Proguraming: Web (html, css, js, etc...)
;;--------------------------------------------------------------------------
;; JavaScript/JSX editing
(use-package js
  :mode ("\\.js\\'" "\\.jsx\\'")  ;; 自動的に js-mode を使うファイル拡張子を指定
  :hook (js-mode . lsp-deferred)  ;; js-mode の時に LSP サーバーを遅延起動
  :init
  (setq js-indent-level 2))       ;; JavaScript のインデントレベルを2に設定

(use-package rjsx-mode
  :ensure t
  :mode ("\\.js\\'" "\\.jsx\\'")  ;; 自動的に js-mode を使うファイル拡張子を指定
  :config
  (setq js2-basic-offset 2)
  (add-hook 'rjsx-mode-hook 'lsp-deferred))

;; TypeScript support
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Svelte support
(use-package svelte-mode
  :ensure t
  :mode "\\.svelte\\'"
  :hook (svelte-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Web development
(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.css\\'" "\\.php\\'" "\\.erb\\'" "\\.vue\\'"  "\\.tsx\\'")
  :config
  (setq web-mode-markup-indent-offset 2)  ;; HTMLのインデント設定
  (setq web-mode-css-indent-offset 2)     ;; CSSのインデント設定
  (setq web-mode-code-indent-offset 2)    ;; JSやPHPなどコード部分のインデント設定
  (setq web-mode-enable-auto-closing t)   ;; タグの自動閉じを有効化
  (setq web-mode-enable-auto-quoting t)   ;; 属性の自動クォートを有効化

  ;; PHPのみインデントを4スペースに設定
  (setq web-mode-script-padding 4)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "php" (file-name-extension buffer-file-name))
                (setq web-mode-code-indent-offset 4))))


  ;; NodeModuleのESLintを使うように設定
  (defun my/find-npm-command (command)
    (let* ((dirname "node_modules")
           (root (locate-dominating-file default-directory dirname)))
      (if root (concat
                (file-name-as-directory root)
                (file-name-as-directory dirname)
                (file-name-as-directory ".bin")
                command))))
  (defun my/executable-find (command)
    (let* ((file-path (my/find-npm-command command)))
      (if (and file-path (file-executable-p file-path))
          file-path (executable-find command))))

  (defun my-web-mode-flycheck-setup ()
    "Custom Flycheck setup for web-mode."
    (setq-local flycheck-executable-find #'my/executable-find))

  (add-hook 'web-mode-hook #'my-web-mode-flycheck-setup)
  )

;;--------------------------------------------------------------------------
;; Proguraming: PHP
;;--------------------------------------------------------------------------
(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode)
  :hook (php-mode . lsp))

;;--------------------------------------------------------------------------
;; Proguraming: Scala
;;--------------------------------------------------------------------------
;; Install and configure lsp-metals
(use-package lsp-metals
  :config
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-metals-coursier-bootstrap-server t)
  )

(setq lsp-diagnostics-provider :flycheck)

;; Install and configure scala-mode
;; (use-package scala-mode
;;   :ensure t
;;   :mode "\\.s\\(cala\\|bt\\)$"
;;   :hook (scala-mode . lsp-deferred)
;;   :config
;;   (setq scala-indent:use-javadoc-style t))

(straight-use-package 'scala-ts-mode)
(setq treesit-font-lock-level 4)
(with-eval-after-load 'scala-ts-mode
  (let ((offset scala-ts-indent-offset))
    (let ((existing (alist-get 'scala scala-ts--indent-rules)))
      (setq scala-ts--indent-rules
            `((scala
               ;; 元のルールを残して
               ,@(cl-remove-if
                  (lambda (rule)
                    (equal (car rule) '(parent-is "^tuple_expression$")))
                  existing)
               ;; tuple_expression の新しいルールを追加
               ((parent-is "^tuple_expression$") parent-bol ,offset)
               ))))))

;; Install and configure sbt-mode
(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  ;; SBT popup windows configuration
  (setq sbt:display-command-buffer nil))

(use-package company
  :hook (scala-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf))

(use-package eglot
  :pin melpa-stable
  ;; (optional) Automatically start metals for Scala files.
  :hook (scala-mode . eglot-ensure))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode)
  :hook (yaml-mode . (lambda ()
                       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package terraform-mode
  :ensure t
  :mode ("\\.tf\\'" . terraform-mode)
  :hook (terraform-mode . terraform-format-on-save-mode))

;; init.elの終端
(provide 'init)

;;; init.el ends here
