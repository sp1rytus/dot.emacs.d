(require 'terraform-mode)

(add-hook 'typescript-mode-hook
  (lambda ()
    (setq terraform-indent-level 2)
  ))
