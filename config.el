(package-initialize)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips

(require 'use-package)

(use-package evil
 :ensure
 :init
 (setq evil-want-keybinding nil)
 :config
 (evil-mode t))

(use-package evil-collection
  :ensure
  :init
  (evil-collection-init))

(use-package helpful)

(use-package which-key
  :ensure
  :config
  (which-key-mode)

  )
(use-package magit :ensure)
(use-package org :ensure)
(use-package haskell-mode :ensure)
(use-package general :ensure)

(use-package vertico
  :init
  (vertico-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(general-define-key :states '(normal visual)
 :prefix "SPC"
 ":" 'execute-extended-command
 "h b b" 'embark-bindings
 "h f" 'helpful-callable
 "h v" 'helpful-variable
 "h k" 'helpful-key
 "f f" 'find-file
 "b b" 'ido-switch-buffer
 "w m m" 'maximize-window
 "w j" 'evil-window-down
 "w k" 'evil-window-up
 "w h" 'evil-window-left
 "w l" 'evil-window-right
 "w v" 'evil-window-vsplit
 "w s" 'evil-window-split
 "w u" 'winner-undo
 "g g" 'magit-status
 "SPC" 'projectile-find-file
 "p" '(:ignore t :which-key "project")
 "p p" 'projectile-switch-project
 "." '(:which-key "Find file" find-file)
 )
