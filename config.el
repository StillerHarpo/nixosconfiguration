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
 "w d" 'maximize-delete
 "w u" 'winner-undo
 "w +" 'evil-window-increase-height
 "w -" 'evil-window-decrease-height
 "w <" 'evil-window-decrease-width
 "w =" 'balance-windows
 "w >" 'evil-window-increase-width
 "w H" 'evil-window-move-far-left
 "w J" 'evil-window-move-very-bottom
 "w K" 'evil-window-move-very-top
 "w L" 'evil-window-move-far-right
 "w R" 'evil-window-rotate-upwards
 "w S" 'evil-window-split
 "w W" 'evil-window-prev
 "w _" 'evil-window-set-height
 "w b" 'evil-window-bottom-right
 "w c" 'evil-window-delete
 "w f" 'ffap-other-window
 "w h" 'evil-window-left
 "w j" 'evil-window-down
 "w k" 'evil-window-up
 "w l" 'evil-window-right
 "w n" 'evil-window-new
 "w o" 'delete-other-windows
 "w p" 'evil-window-mru
 "w q" 'evil-quit
 "w r" 'evil-window-rotate-downwards
 "w s" 'evil-window-split
 "w t" 'evil-window-top-left
 "w v" 'evil-window-vsplit
 "w w" 'evil-window-next
 "w x" 'evil-window-exchange
 "w |" 'evil-window-set-width
 "g g" 'magit-status
 "SPC" 'project-find-file
 "p" '(:ignore t :which-key "project")
 "p p" 'project-switch-project
 "." '(:which-key "Find file" 'find-file)
)

(general-evil-define-key '(normal visual) 'emacs-lisp-mode
  :prefix "SPC m"
  "e e" 'eval-last-sexp)

(setq ytel-invidious-api-url "https://yewtu.be")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix format all not using direnv environmet
(after! format-all (advice-add 'format-all-buffer :around #'envrc-propagate-environment))
(advice-add 'save-buffer :around #'envrc-propagate-environment)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun peertube-mpv-open-video ()
  "Open the video under the cursor using `browse-url'."
  (interactive)
  (let ((url (peertube-video-url (peertube--get-current-video))))
    (browse-url-mpv url)))

(general-evil-define-key '(normal visual) 'peertube-mode-map
  "enter" 'peertube-mpv-open-video
  "o" 'peertube-mpv-open-video
  "c" 'peertube-goto-channel
  "i" 'peertube-show-video-info
  "d" 'peertube-download-video
  "q" 'peertube-quit
  "s" 'peertube-search
  "m" 'peertube-change-sort-method
  "t" 'peertube-preview-thumbnail)
(setq ytel-invidious-api-url "https://yewtu.be")
