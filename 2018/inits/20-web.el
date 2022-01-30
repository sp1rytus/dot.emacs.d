(require 'web-mode)

;; インデント関係
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset           2)
  (setq web-mode-css-offset            2)
  (setq web-mode-script-offset         2)
  (setq web-mode-php-offset            2)
  (setq web-mode-java-offset           2)
  (setq web-mode-asp-offset            2)
  (setq web-mode-code-indent-offset    2)
  (setq web-mode-markup-indent-offset  2)
  (setq indent-tabs-mode               nil))
(add-to-list 'web-mode-indentation-params '("lineup-args"    . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls"   . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

(add-hook 'web-mode-hook 'web-mode-hook)

;; 拡張子の設定
(add-to-list 'auto-mode-alist '("\\.html?$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.scala.html?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"         . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"         . web-mode))

;; 色の設定
(custom-set-faces
 '(default ((t (:background "black" :foreground "white"))))
)
