(package-initialize)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips

(require 'use-package)

(use-package evil
  :ensure
  :config
  (evil-mode t))

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
  (vertico-mode)
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
 )
