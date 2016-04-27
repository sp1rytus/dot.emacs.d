(require 'popwin)

(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)
(setq popwin:close-popup-window-timer-interval 0.5)

;; ポップアップ表示設定
(push '("\*sbt\*.*" :regexp t) popwin:special-display-config)
