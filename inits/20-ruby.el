(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))

(add-hook 'ruby-mode-hook
  (lambda ()
    (setq ruby-indent-level 2)
  ))
