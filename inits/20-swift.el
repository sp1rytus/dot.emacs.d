(add-hook 'swift-mode-hook
  (lambda ()
    (defvar swift-indent-offset)
    (setq-local swift-mode:basic-offset 2)))
