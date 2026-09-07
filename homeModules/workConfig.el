(use-package claude-code-ide
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(general-define-key :states '(normal visual motion emacs)
 :keymaps '(my-keys-minor-mode-map)
 :prefix "SPC"
 "o a" 'claude-code-ide-menu
)
