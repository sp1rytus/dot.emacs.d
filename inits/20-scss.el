;; インデント幅2
;; 自動コンパイルをオフにする
(defun scss-custom ()
  "scss-mode-hook"
  (and
   (set (make-local-variable 'css-indent-offset)      2)
   (set (make-local-variable 'scss-compile-at-save) nil)
   )
  )

(add-hook 'scss-mode-hook
  '(lambda() (scss-custom)))
