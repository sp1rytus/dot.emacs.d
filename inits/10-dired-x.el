(require 'dired-x)
(add-hook 'dired-load-hook
   (function (lambda () (load "dired-x"))))
(setq dired-listing-switches "-al --g")

;; 一度に複数のファイルcoding systemを変換する
(require 'dired-aux)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key (current-local-map) "T"
              'dired-do-convert-coding-system)))
(defvar dired-default-file-coding-system nil
  "*Default coding system for converting file (s).")
(defvar dired-file-coding-system 'no-conversion)

(defun dired-convert-coding-system ()
  (let ((file (dired-get-filename))
        (coding-system-for-write dired-file-coding-system)
        failure)
    (condition-case err
        (with-temp-buffer
          (insert-file file)
          (write-region (point-min) (point-max) file))
      (error (setq failure err)))
    (if (not failure)
        nil
      (dired-log "convert coding system error for %s:\n%s\n" file failure)
      (dired-make-relative file))))

(defun dired-do-convert-coding-system (coding-system &optional arg)
  "Convert file (s) in specified coding system."
  (interactive
   (list (let ((default (or dired-default-file-coding-system
                            buffer-file-coding-system)))
           (read-coding-system
            (format "Coding system for converting file (s) (default, %s): "
                    default)
            default))
         current-prefix-arg))
  (check-coding-system coding-system)
  (setq dired-file-coding-system coding-system)
  (dired-map-over-marks-check
   (function dired-convert-coding-system) arg 'convert-coding-system t))
