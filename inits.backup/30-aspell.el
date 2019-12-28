(if (file-executable-p "/usr/bin/aspell")
    (setq-default ispell-program-name "aspell")
    (setq-default ispell-program-name "ispell"))

(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(defun ispell-get-word (following)
  (when following (camelCase-forward-word 1))
  (let* ((start (progn (camelCase-backward-word 1)
                       (point)))
         (end   (progn (camelCase-forward-word 1)
                       (point))))
    (list (buffer-substring-no-properties start end)
          start end)))
