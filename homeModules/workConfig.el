(use-package claude-code-ide
  :custom
  (claude-code-ide-terminal-backend 'ghostel)
  :config
  (claude-code-ide-emacs-tools-setup)
  (defun claude-code-ide-mcp-buffer-list ()
    (claude-code-ide-mcp-server-with-session-context
     nil
     (buffer-list)))
  (claude-code-ide-make-tool
   :function #'claude-code-ide-mcp-buffer-list
   :name "claude-code-ide-mcp-buffer-list"
   :description "List the names of all currently open (live, non-internal) Emacs buffers."
   :args '())
  (defun claude-code-ide-mcp-buffer-contents (buffer-name)
    (claude-code-ide-mcp-server-with-session-context
     nil
     (let ((buf (get-buffer buffer-name)))
       (if (not buf)
	   (format "No buffer named %s" buffer-name)
	 (with-current-buffer buf
	   (save-restriction
	     (widen)
	     (buffer-substring-no-properties (point-min) (point-max))))))))
  (claude-code-ide-make-tool
   :function #'claude-code-ide-mcp-buffer-contents
   :name "claude-code-ide-mcp-buffer-contents"
   :description "Get the content of the given buffer"
   :args '((
	    :name "buffer"
	    :type string
	    :description "The buffer to get the content from"
	    )))
  (defun claude-code-ide-mcp-meqo--buffer-name-for-mode (mode)
    "Return the buffer name `compile' uses for a `define-compilation-mode' MODE.
Mirrors `compilation-start's default naming: \"*\" + the mode's symbol name
with a trailing \"-mode\" stripped, downcased, + \"*\"."
    (concat "*" (downcase (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))) "*"))
  (defun claude-code-ide-mcp-meqo--running-process (mode)
    "Return the live process of a meqo compilation already running under MODE.
Returns nil if none is running."
    (let* ((buf (get-buffer (claude-code-ide-mcp-meqo--buffer-name-for-mode mode)))
	   (proc (and buf (get-buffer-process buf))))
      (and proc (process-live-p proc) proc)))
  (defun claude-code-ide-mcp-meqo-compile (target &optional kill_running)
    "Run `./shake TARGET' for the meqo backend, as an MCP tool.
If KILL_RUNNING is non-nil, kill an already-running meqo compile first."
    (if (not target)
	(error "target parameter is required")
      (claude-code-ide-mcp-server-with-session-context nil
						       (condition-case err
							   (let ((running (claude-code-ide-mcp-meqo--running-process 'meqo-compilation-mode)))
							     (if (and running (not kill_running))
								 (format "A meqo compile is already running in buffer %s. Pass kill_running=true to kill it and start `./shake %s` instead."
									 (claude-code-ide-mcp-meqo--buffer-name-for-mode 'meqo-compilation-mode)
									 target)
							       (let ((compilation-ask-about-save nil)
								     (compilation-always-kill t)
								     (buf (my/meqo-compile target)))
								 (format "Started `./shake %s`%s." target
									 (if (bufferp buf) (format " in buffer %s" (buffer-name buf)) "")))))
							 (error (format "Error starting meqo compile of '%s': %s"
									target (error-message-string err)))))))
  (claude-code-ide-make-tool
   :function #'claude-code-ide-mcp-meqo-compile
   :name "claude-code-ide-mcp-meqo-compile"
   :description "Compile a meqo backend shake target (e.g. \"all-objs\", \"UiGen\", a *Tests target, ...). Starts an async `./shake TARGET` compilation in Emacs and returns immediately; watch the returned compilation buffer for results. If a meqo compile is already running, reports that instead of starting a new one unless kill_running is set. Saves modified buffers automatically."
   :args '((:name "target"
		  :type string
		  :description "The shake target to build, e.g. \"all-objs\" or \"UiGenTests\"")
	   (:name "kill_running"
		  :type boolean
		  :description "If true, kill an already-running meqo compile and start this one instead of reporting that one is running"
		  :optional t)))
  (defun claude-code-ide-mcp-meqo-webpack (entry-point &optional kill_running)
    "Run webpack --watch ENTRY-POINT for the meqo TypeScript code, as an MCP tool.
If KILL_RUNNING is non-nil, kill an already-running meqo webpack watch first."
    (if (not entry-point)
	(error "entry_point parameter is required")
      (claude-code-ide-mcp-server-with-session-context nil
						       (condition-case err
							   (let ((running (claude-code-ide-mcp-meqo--running-process 'meqo-webpack-compilation-mode)))
							     (if (and running (not kill_running))
								 (format "A meqo webpack watch is already running in buffer %s. Pass kill_running=true to kill it and start watching '%s' instead."
									 (claude-code-ide-mcp-meqo--buffer-name-for-mode 'meqo-webpack-compilation-mode)
									 entry-point)
							       (let ((compilation-ask-about-save nil)
								     (compilation-always-kill t)
								     (buf (my/meqo-webpack entry-point)))
								 (format "Started webpack --watch for entry point '%s'%s." entry-point
									 (if (bufferp buf) (format " in buffer %s" (buffer-name buf)) "")))))
							 (error (format "Error starting meqo webpack for '%s': %s"
									entry-point (error-message-string err)))))))
  (claude-code-ide-make-tool
   :function #'claude-code-ide-mcp-meqo-webpack
   :name "claude-code-ide-mcp-meqo-webpack"
   :description "Start `./js/scripts/webpack --watch ENTRY_POINT` for the meqo TypeScript code. Starts an async watch compilation in Emacs and returns immediately; watch the returned compilation buffer for results. If a meqo webpack watch is already running, reports that instead of starting a new one unless kill_running is set. Saves modified buffers automatically."
   :args '((:name "entry_point"
		  :type string
		  :description "The webpack entry point to build, e.g. \"workflow\" or \"web\"")
	   (:name "kill_running"
		  :type boolean
		  :description "If true, kill an already-running meqo webpack watch and start this one instead of reporting that one is running"
		  :optional t)))
  (defun claude-code-ide-mcp-meqo-run-ts-tests (filter &optional kill_running)
    "Run the meqo TypeScript tests matching FILTER, as an MCP tool.
If KILL_RUNNING is non-nil, kill an already-running meqo test run first."
    (claude-code-ide-mcp-server-with-session-context nil
						     (condition-case err
							 (let ((running (claude-code-ide-mcp-meqo--running-process 'meqo-ts-test-compilation-mode)))
							   (if (and running (not kill_running))
							       (format "A meqo TypeScript test run is already running in buffer %s. Pass kill_running=true to kill it and start filter '%s' instead."
								       (claude-code-ide-mcp-meqo--buffer-name-for-mode 'meqo-ts-test-compilation-mode)
								       (or filter ""))
							     (let ((compilation-ask-about-save nil)
								   (compilation-always-kill t)
								   (buf (my/meqo-run-ts-tests (or filter ""))))
							       (format "Started meqo TypeScript tests (filter: '%s')%s." (or filter "")
								       (if (bufferp buf) (format " in buffer %s" (buffer-name buf)) "")))))
						       (error (format "Error starting meqo TypeScript tests with filter '%s': %s"
								      filter (error-message-string err))))))
  (claude-code-ide-make-tool
   :function #'claude-code-ide-mcp-meqo-run-ts-tests
   :name "claude-code-ide-mcp-meqo-run-ts-tests"
   :description "Run meqo's TypeScript test suite (`./js/scripts/runTests`), optionally filtered. Starts an async compilation in Emacs and returns immediately; watch the returned compilation buffer for results. If a meqo test run is already running, reports that instead of starting a new one unless kill_running is set. Saves modified buffers automatically."
   :args '((:name "filter"
		  :type string
		  :description "Test name filter passed as -f FILTER to runTests; empty string runs all tests"
		  :optional t)
	   (:name "kill_running"
		  :type boolean
		  :description "If true, kill an already-running meqo test run and start this one instead of reporting that one is running"
		  :optional t)))
  )

(use-package ghostel)

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode)
  :config
  (defun my/ghostel-pad-to-cursor (&rest _)
    "Pad the cursor line so point reaches the terminal cursor column.
  The renderer trims trailing blanks, so a just-typed space would
  otherwise not move point."
    (when-let* ((term ghostel--term)
		(pos (ghostel--cursor-position term))
		(col (car pos)))
      (when (and (eolp) (< (current-column) col))
	(let ((inhibit-read-only t)
	      (inhibit-modification-hooks t))
	  (insert (make-string (- col (current-column)) ?\s))))))
  (advice-add 'ghostel--redraw :after #'my/ghostel-pad-to-cursor)
  (defun my/ghostel-paste-and-insert ()
    "Paste kill ring into the ghostel terminal and enter insert state."
    (interactive)
    (ghostel-yank)
    (evil-insert-state))
  (evil-define-key* 'normal evil-ghostel-mode-map
    "p" #'my/ghostel-paste-and-insert
    "P" #'my/ghostel-paste-and-insert))

(general-define-key
 :states '(normal visual motion emacs)
 :keymaps '(my-keys-minor-mode-map)
 :prefix "SPC"
 "o a" 'claude-code-ide-menu
 )
