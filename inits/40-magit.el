(require 'magit)

;; 文字色変更
(set-face-foreground 'magit-diff-added             "#00FF00") ; 追加行
(set-face-foreground 'magit-diff-added-highlight   "#00FF00") ; 追加行
(set-face-foreground 'magit-diff-removed           "#FF0000") ; 削除行
(set-face-foreground 'magit-diff-removed-highlight "#FF0000") ; 削除行

;; 背景色変更
(set-face-background 'magit-diff-added             "#000000") ; 追加行
(set-face-background 'magit-diff-added-highlight   "#000000") ; 追加行
(set-face-background 'magit-diff-removed           "#000000") ; 削除行
(set-face-background 'magit-diff-removed-highlight "#000000") ; 削除行

;; (setq magit-diff-refine-hunk 'all)
(set-face-background 'magit-diff-context-highlight nil)
(set-face-background 'magit-section-highlight      nil)
