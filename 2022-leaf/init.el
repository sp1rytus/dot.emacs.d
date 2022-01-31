;; -*- lexical-binding: t -*-

;;--------------------------------------------------------------------------
;; My Custom Settings
;;--------------------------------------------------------------------------
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra    :ensure t)
    (leaf el-get   :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

;;--------------------------------------------------------------------------
;; System
;;--------------------------------------------------------------------------
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((user-full-name                  . "Yoshinobu Kinugasa")
            (user-mail-address               . "yoshinobu.kinugasa@ixias.net")
            (user-login-name                 . "sp1rytus")
            (use-dialog-box                  . nil)
            (use-file-dialog                 . nil)
            (menu-bar-mode                   . nil)
            (tool-bar-mode                   . nil)
            (scroll-bar-mode                 . nil)
            (indent-tabs-mode                . nil)
            (create-lockfiles                . nil)
            (debug-on-error                  . t)
            (init-file-debug                 . t)
            (frame-resize-pixelwise          . t)
            (enable-recursive-minibuffers    . t)
            (truncate-lines                  . t)
            (history-length                  . 1000)
            (history-delete-duplicates       . t)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively           . 100)
            (mouse-wheel-scroll-amount       . '(1 ((control) . 5)))
            (ring-bell-function              . 'ignore)
            (text-quoting-style              . 'straight))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

(leaf dired
  :custom
  (dired-isearch-filenames . t)
  (dired-listing-switches  . "-al --group-directories-first"))

;;--------------------------------------------------------------------------
;; Color Themes
;;--------------------------------------------------------------------------
(leaf themes
  :custom-face
  (mode-line          . '((t (:box "dark olive green"))))
  (mode-line-inactive . '((t (:box "#445"))))
  :config
  (leaf color-theme-sanityinc-solarized
    :disabled t
    :doc "A version of Ethan Schoonover's Solarized themes"
    :req "emacs-24.1" "cl-lib-0.6"
    :tag "themes" "faces" "emacs>=24.1"
    :added "2021-03-19"
    :url "https://github.com/purcell/color-theme-sanityinc-solarized"
    :emacs>= 24.1
    :ensure t
    :config (load-theme 'sanityinc-solarized-dark t))
  (leaf doom-themes
    :doc "an opinionated pack of modern color-themes"
    :req "emacs-25.1" "cl-lib-0.5"
    :tag "nova" "faces" "icons" "neotree" "theme" "one" "atom" "blue" "light" "dark" "emacs>=25.1"
    :added "2021-04-09"
    :url "https://github.com/hlissner/emacs-doom-theme"
    :emacs>= 25.1
    :ensure t
    :config
    (load-theme 'doom-dark+ t)
    (custom-set-faces '(default ((t (:background "#030303")))))
    ))

;;--------------------------------------------------------------------------
;; Key Bindings
;;--------------------------------------------------------------------------
(leaf basic-key-bindings
  :init
  (global-unset-key (kbd "C-q"))
  (global-unset-key (kbd "C-x DEL"))
  (leaf basic-key-bindings-bind
    :init
    (defun switch-to-used-buffer ()
      (interactive)
      (switch-to-buffer nil))
    :bind (("<tab>" . indent-for-tab-command)
           ("C-x <RET> u" . revert-buffer-with-coding-system-utf-8-unix)
           ("C-x <RET> s" . revert-buffer-with-coding-system-japanese-cp932-dos))))

;;--------------------------------------------------------------------------
;; Common Behavior
;;--------------------------------------------------------------------------

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
