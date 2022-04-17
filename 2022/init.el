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
(leaf locale
    :setq-default ((buffer-file-coding-system quote utf-8))
    :config
    (set-language-environment "Japanese")
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8))

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
  :custom '(
            (ring-bell-function  . 'ignore)
            (use-dialog-box      . nil)
            (use-file-dialog     . nil)
            (menu-bar-mode       . nil)
            (tool-bar-mode       . nil)
            (scroll-bar-mode     . nil)
            (create-lockfiles    . nil)
            (column-number-mode  . t)
            (column-number-mode  . t)
            (line-number-mode    . t)
            ;; 編集
            (scroll-step                           . 1)
            (next-screen-context-lines             . 10)
            (tab-width                             . 2)
            (indent-tabs-mode                      . nil)
            (fill-column                           . 72)
            (truncate-lines                        . nil)
            (truncate-partial-width-windows        . t)
            (paragraph-start                       . '"^\\([ 　・○<\t\n\f]\\|(?[0-9a-zA-Z]+)\\)")
            (auto-fill-mode                        . nil)
            (next-line-add-newlines                . nil)
            (read-file-name-completion-ignore-case . nil)
            (save-abbrevs                          . 'silent)
            ;; backup
            (debug-on-error                  . t)
            (init-file-debug                 . t)
            (frame-resize-pixelwise          . t)
            (enable-recursive-minibuffers    . t)
            (history-length                  . 1000)
            (history-delete-duplicates       . t)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively           . 100)
            (mouse-wheel-scroll-amount       . '(1 ((control) . 5)))
            (text-quoting-style              . 'straight))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

(leaf files
  :doc "file input and output commands for Emacs"
  :tag "builtin"
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)))

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

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
(leaf *global-set-key
  :leaf-autoload nil
  :bind
  ("<tab>"    . indent-for-tab-command)
  ("<f7>"     . enlarge-window-horizontally)
  ("<f8>"     . enlarge-window)
  ("<f11>"    . ibuffer)
  ("<f12>"    . undo)
  ("M-z"      . lsp)

  ("<help> c" . helpful-command)
  ("<help> w" . helm-man-woman)

  ("C-x <RET> u" . revert-buffer-with-coding-system-utf-8-unix)
  ("C-x <RET> s" . revert-buffer-with-coding-system-japanese-cp932-dos))

;;--------------------------------------------------------------------------
;; Common Behavior
;;--------------------------------------------------------------------------
(leaf dired
  :custom
  (dired-isearch-filenames . t)
  (dired-listing-switches  . "-al --group-directories-first"))

(leaf minibuffer
  :bind
  ((minibuffer-local-completion-map
    ("C-w" . backward-kill-word)
    )))

(leaf ispell
  :if (file-executable-p "aspell")
  :custom
  (ispell-program-name . "aspell")
  :config
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  )

(leaf flyspell
  :ensure t
  :blackout (flyspell-mode . "F")
  :defun
  flyspell-emacs-popup-textual
  :preface
  (defun my:flyspell-popup-choose (orig event poss word)
    (if (window-system)
        (funcall orig event poss word)
      (flyspell-emacs-popup-textual event poss word)))
  :advice (:around flyspell-emacs-popup
                   my:flyspell-popup-choose)
  :hook
  ((scala-mode-hook . flyspell-prog-mode))
  )

(leaf *trailing-white-space
  :preface
  (defvar my:delete-trailing-whitespace-exclude-suffix
    (list "\\.rd$" "\\.md$" "\\.rbt$" "\\.rab$"))
  (defun my:delete-trailing-whitespace ()
    (interactive)
    (eval-when-compile (require 'cl-lib))
    (cond
     ((equal nil
             (cl-loop for pattern in my:delete-trailing-whitespace-exclude-suffix
                      thereis (string-match pattern buffer-file-name)))
      (delete-trailing-whitespace))))
  :hook
  (before-save-hook . my:delete-trailing-whitespace)
  )

;;--------------------------------------------------------------------------
;; IDE Enviroment
;;--------------------------------------------------------------------------
;; (leaf lsp-mode
;;   :ensure t
;;   :after  t
;;   :defun  lsp-enable-which-key-integration
;;   :defvar lsp-command-map lsp-signature-mode-map
;;   :custom
;;   (lsp-auto-guess-root              . nil)
;;   (lsp-enable-snippet               . nil)
;;   (lsp-keymap-prefix                . "M-z")
;;   (lsp-lens-mode                    . t)
;;   (lsp-prefer-flymake               . nil)
;;   (lsp-headerline-breadcrumb-enable . nil)
;;   (read-process-output-max          . 1048576)
;;   (lsp-completion-provider          . :capf)
;;   :bind (:lsp-mode-map
;;          ("C-S-SPC" . nil)
;;          ("C-c C-a" . lsp-execute-code-action)
;;          ("C-c C-i" . lsp-format-region)
;;          ("C-c C-n" . lsp-rename)
;;          ("C-c C-r" . lsp-workspace-restart)
;;          ("C-c C-t" . lsp-describe-thing-at-point))
;;   :config
;;   (define-key lsp-mode-map (kbd "M-z") lsp-command-map)
;;   (leaf lsp-metals)
;;   (leaf lsp-ui
;;     :ensure t
;;     :defvar lsp-ui-peek-mode-map
;;     :custom
;;     (lsp-ui-doc-header            . t)
;;     (lsp-ui-doc-include-signature . t)
;;     (lsp-ui-doc-position          . 'bottom)
;;     (lsp-ui-sideline-enable       . nil)
;;     :bind (:lsp-ui-mode-map
;;            ("C-c C-d" . lsp-ui-doc-show)))
;;   (leaf dap-mode
;;     :ensure t
;;     :hook
;;     (lsp-mode-hook . dap-mode)
;;     (lsp-mode-hook . dap-ui-mode)))

(leaf yasnippet
  :ensure  t
  :require t
  :bind (:yas-minor-mode-map
         ("<tab>"   . nil)
         ("TAB"     . nil)
         ("C-c C-y" . company-yasnippet))
  :config
  (yas-global-mode)
  (leaf yasnippet-snippets :ensure t))

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n"   . nil)
          ("M-p"   . nil)
          ("C-s"   . company-filter-candidates)
          ("C-n"   . company-select-next)
          ("C-p"   . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n"  . company-select-next)
          ("C-p"  . company-select-previous)))
  :custom ((company-idle-delay   . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

(leaf whitespace
  :ensure t
  :bind ("C-c C-c" . my:cleanup-for-spaces)
  :hook (prog-mode-hook . my:enable-trailing-mode)
  :config
  (setq require-final-newline t)
  (setq show-trailing-whitespace t)
  (set-face-background 'trailing-whitespace "brightwhite")
  :init
  (defun my:enable-trailing-mode ()
    "Show tail whitespace."
    (setq show-trailing-whitespace t))

  (defun my-c-mode-hook ()
    (c-set-style "linux")
    (setq c-basic-offset tab-width))
  (add-hook 'c-mode-hook 'my-c-mode-hook)
  (setq-default tab-width 2 indent-tabs-mode nil)

  (defun indent-and-next-line ()
    (interactive)
    (indent-according-to-mode)
    (next-line 1))

  (defun my:cleanup-for-spaces ()
    "Remove contiguous line breaks at end of line + end of file."
    (interactive)
    (delete-trailing-whitespace)
    (save-excursion
      (save-restriction
    (widen)
    (goto-char (point-max))
    (delete-blank-lines)))))

  (defface my-face-b-1 '((t (:background "brightwhite"))) nil)
  (defface my-face-b-2 '((t (:background "brightblack"))) nil)
  (defvar  my-face-b-1 'my-face-b-1)
  (defvar  my-face-b-2 'my-face-b-2)
  (defadvice font-lock-mode (before my-font-lock-mode ())
    (font-lock-add-keywords
     major-mode
     '(("　" 0 my-face-b-1 append)
       ("\t" 0 my-face-b-2 append)
       )))
  (ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
  (ad-activate 'font-lock-mode)

;;--------------------------------------------------------------------------
;; Language Dev Enviroment
;;--------------------------------------------------------------------------
(leaf cc-mode
  :doc "major mode for editing C and similar languages"
  :tag "builtin"
  :defvar (c-basic-offset)
  :bind   (c-mode-base-map
           ("C-c c" . compile))
  :mode-hook
  (c-mode-hook   . ((c-set-style "bsd") (setq c-basic-offset 2)))
  (c++-mode-hook . ((c-set-style "bsd") (setq c-basic-offset 2))))

;; (leaf scala-mode
;;   :ensure t
;;   :hook
;;   (scala-mode-hook . lsp)
;;   :init
;;   (setq
;;    scala-indent:use-javadoc-style t
;;   )
;;   :config
;;   (leaf lsp-metals :ensure t :require t))

(leaf scala-mode
  :ensure t
  :init
  (setq
   scala-indent:use-javadoc-style t
  ))

(leaf js
  :custom
  (js-indent-level . 2)
  )

(leaf typescript-mode
  :ensure t
  :custom
  (typescript-indent-level . 2)
  )

(leaf mhtml-mode
  :ensure t
  :leaf-defer t
  :mode ("\\.html\\'" . mhtml-mode))

(leaf css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

(leaf scss-mode
  :if (executable-find "sass")
  :ensure t
  :mode "\\.scss\\'"
  :init
  ((setq scss-compile-at-save nil))
  :custom
  `((scss-sass-command . ,(executable-find "sass")))
  )

(leaf json-mode
  :ensure t
  :mode ("\\.json$")
  :hook (json-mode-hook . (lambda ()))
  :custom
  ((js-indent-level . 2)
   (json-reformat:indent-width . 2)
   (tab-width . 2)))

(leaf yaml-mode
  :ensure t
  :leaf-defer t
  :mode ("\\.yaml\\'" . yaml-mode))


(leaf dockerfile-mode :ensure t)

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
