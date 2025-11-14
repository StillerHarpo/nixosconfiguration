(package-initialize)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)           ; Disable menubar
(setq ring-bell-function 'ignore)
(setq create-lockfiles nil)
(setq make-backup-files nil)

(setq emacsAutoSaveDir "~/Dokumente/.emacs-saves/")
(unless (file-directory-p emacsAutoSaveDir)
  (make-directory emacsAutoSaveDir))
(setq auto-save-file-name-transforms
  `((".*" ,emacsAutoSaveDir t)))
(defun my/diff-with-autosave-file ()
  "Diff file with its auto-save"
  (interactive)
  (ediff (bookmark-buffer-file-name) (make-auto-save-file-name)))

(defun my/recover-some-files ()
  "ask to recover files which have autosaves"
  (interactive)
  (map-y-or-n-p
   (lambda (recover-and-file) (format "Recover file %s? " (cadr recover-and-file)))
   (lambda (recover-and-file) (apply 'rename-file `(,@recover-and-file t)))
   (-map
    (lambda (auto-save-file)
      `(,(concat emacsAutoSaveDir auto-save-file)
        ,(replace-regexp-in-string "!" "/" (replace-regexp-in-string "#" "" auto-save-file))))
    (directory-files emacsAutoSaveDir nil (rx (seq "#" (one-or-more anychar) "#"))))
   nil
   `((?d ,(lambda (recover-and-file)
            (require 'diff)            ;for diff-no-select.
            (let ((diffbuf (diff-no-select (cadr recover-and-file) (car recover-and-file)
                                           nil 'noasync)))
              (if (not enable-recursive-minibuffers)
                  (progn (display-buffer diffbuf)
                         (setq other-window-scroll-buffer diffbuf))
                (view-buffer diffbuf (lambda (_) (exit-recursive-edit)))
                (recursive-edit)))
            ;; Return nil to ask about BUF again.
            nil)
         ,(purecopy "view changes of auto-save"))
     (?D ,(lambda (recover-and-file)
            (delete-file (car recover-and-file)))
         ,(purecopy "view changes of auto-save")))))

(require 'use-package)

(use-package my-nix-variables
  :config
  (defun browse-url-mpv (url &rest args) (browse `(,nix-mpv-path "--save-position-on-quit") url))
  (defun browse-url-mpv-audio (url &rest args) (browse `(,nix-mpv-path "--no-video --save-position-on-quit") url))
  (defun browse-url-linkopen (url &rest args) (browse `(,nix-linkopen-path) url))
  (setq vterm-shell nix-zsh-path)
  (setq org-latex-pdf-process (list (concat nix-latexmk-path " -shell-escape -bibtex -f -pdfxe %f")))
  (setq langtool-java-classpath (concat nix-languagetool-path "/share/")
        langtool-java-user-arguments `("-Dfile.encoding=UTF-8"
                                       "-cp" ,(concat nix-languagetool-path "/share/"))
        langtool-java-bin nix-jdk-path
        langtool-language-tool-jar (concat nix-languagetool-path "/share/languagetool-commandline.jar")
        langtool-language-tool-server-jar (concat nix-languagetool-path "/share/languagetool-server.jar")))

(use-package general :ensure)

(use-package hydra
  :ensure t)

(use-package evil
 :ensure t
 :after hydra
 :init
 (setq evil-want-keybinding nil)
 (setq evil-undo-system 'undo-redo)
 :config
 (evil-mode t)
 (define-key evil-motion-state-map " " nil)
 (defhydra hydra-zoom (evil-normal-state-map "SPC z")
   "zoom"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" text-scale-mode "no scale"))
 )

(use-package evil-collection
  :ensure
  :init
  (evil-collection-init))

(use-package evil-embrace
  :after evil
  :ensure t
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-surround
  :ensure
  :config
  (global-evil-surround-mode 1))

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-nerd-commenter
  :ensure)

(use-package helpful)

(use-package which-key
  :ensure
  :config
  (which-key-mode)
  )

(use-package vertico
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode 1))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  :config
  (defun my-org-sparse-tree ()
    "Create an org sparse tree showing only point"
    (interactive)
    (org-overview)      ;; Hide everything
    (org-show-context)) ;; Show context around point
  )

(use-package lean4-mode
  ;; to defer loading the package until required
  :commands (lean4-mode))

(use-package company
  :config
  (global-company-mode))

(use-package tempel
  :general
  (:states '(normal visual motion emacs)
   :keymaps '(my-keys-minor-mode-map)
   :prefix "SPC"
   "c i" 'tempel-insert))

(use-package tempel-collection
  :ensure t
  :after tempel)

(use-package consult
  :ensure t
  :custom
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref)
  (completion-in-region-function 'consult-completion-in-region))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package avy :ensure)

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package
  envrc
  :ensure
  :config
  (envrc-global-mode))

(use-package
  format-all
  :ensure
  :config
  ;; Fix format all not using direnv environmet
  (advice-add 'format-all-buffer :around #'envrc-propagate-environment))
  (advice-add 'save-buffer :around #'envrc-propagate-environment)

(use-package org
  :ensure
  :custom
  ;; https://github.com/doomemacs/doomemacs/issues/2672
  (org-agenda-inhibit-startup t)
  (org-agenda-start-on-weekday nil)
  (org-src-window-setup 'current-window)
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)))
  (org-highlight-latex-and-related '(latex script entities))
  (org-ref-bibliography-notes "~/Dokumente/bibliography/notes.org")
  (org-ref-default-bibliography reftex-default-bibliography)
  (org-ref-pdf-directory "~/Dokumente/bibliography/bibtex-pdfs/")
  (org-default-notes-file "~/Dokumente/notes.org")
  (org-read-date-force-compatible-dates nil)
  (org-modules '(ol-bibtex org-habit))
  (org-log-into-drawer "LOGBOOK")
  (org-agenda-window-setup 'current-window)
  (org-agenda-todo-ignore-scheduled 'future)
  (org-todo-keywords
   '((sequence
      "TODO(t)"  ; A task that needs doing & is ready to do
      "|"
      "DONE(d)"  ; Task successfully completed
      "KILL(k)"))) ; Task was cancelled, aborted or is no longer applicable
  (org-startup-folded 'fold)
  (org-capture-templates
   '(("c" "capture to notes.org" entry (file "~/Dokumente/notes.org") "* %i %?\n%U\n- %a")))
  :config
  ;; https://github.com/doomemacs/doomemacs/issues/2672
  ;; Prevent temporarily opened agenda buffers from polluting recentf.
  (advice-add #'org-get-agenda-file-buffer
              :around (lambda (orig-fn file)
                        (let ((recentf-exclude (list (lambda (_file) t)))
                              find-file-hook
                              org-mode-hook)
                          (funcall orig-fn file))))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook #'auto-revert-mode)

  (defun my/org-insert-auto-schedule (heading)
    "Schedules an heading to the current date. Inserts the heading if doesn't exist"
    (with-current-buffer (find-file-noselect "~/Dokumente/auto.org")
      (let ((heading-name heading))
        (if-let (pos (org-find-exact-headline-in-buffer heading-name nil t))
            (goto-char pos)
          (progn (goto-char (point-max)) (org-insert-heading) (insert heading-name)))
        (org-todo "TODO")
        (org-schedule 1 (calendar-date-string (calendar-current-date)))
        (save-buffer))))

  (defun my/org-find-headline-in-buffer (regex &optional todo-keyword buffer pos-only)
    "Find node wihtch matches regexHEADING in BUFFER.
Return a marker to the heading if it was found, or nil if not.
If POS-ONLY is set, return just the position instead of a marker.

The heading text must match exact, but it may have a TODO keyword,
a priority cookie and tags in the standard locations."
    (with-current-buffer (or buffer (current-buffer))
      (org-with-wide-buffer
       (goto-char (point-min))
       (let (case-fold-search)
         (when (re-search-forward
                (rx (seq line-start "* " (literal (if todo-keyword todo-keyword "")) (zero-or-more not-newline) (regexp regex))) nil t)
           (if pos-only
               (match-beginning 0)
             (move-marker (make-marker) (match-beginning 0))))))))

  (defun my/org-set-auto-schedule-done-rx (heading)
    "Mark every heading as done which matches heading regex"
    (with-current-buffer (find-file-noselect "~/Dokumente/auto.org")
      (let ((heading-name heading))
        (while-let ((pos (my/org-find-headline-in-buffer heading-name "TODO" nil t)))
          (goto-char pos)
          (org-todo "DONE")
          (save-buffer)))))

  (defun my/org-set-auto-schedule-done (heading)
    "Mark every HEADING as done which matches heading regex"
    (with-current-buffer (find-file-noselect "~/Dokumente/auto.org")
      (let ((heading-name heading))
        (if-let ((pos (org-find-exact-headline-in-buffer heading-name nil t)))
            (progn (goto-char pos)
                   (org-todo "DONE")
                   (save-buffer))))))

  (defun my/auto-update-agenda-if (p msg)
    "sets org entry with msg to done if p is true existing and schedules otherwise"
    (if p
        (my/org-set-auto-schedule-done msg)
      (my/org-insert-auto-schedule msg)))

  (my/org-set-auto-schedule-done "auto saves aufräumen")
  (defun my/auto-update-agenda-if-file-empty (file msg)
    "sets org entry with msg to done if file is empty/not existing
     and schedules otherwise"
    (my/auto-update-agenda-if
     (or
      (not (file-exists-p file))
      (string-equal (org-file-contents file) ""))
     msg))

  (my/auto-update-agenda-if
   (directory-empty-p emacsAutoSaveDir)
   "auto saves aufräumen")

  (defun my/org-agenda ()
    "org-agenda with auto todos"
    (interactive)
    (my/auto-update-agenda-if
     (process-lines
      "journalctl"
      "-u"
      "restic-backups-florian.service"
      "JOB_RESULT=done"
      "--since=-7d"
      "--output=cat")
     "restic fixen")
    (my/auto-update-agenda-if
     (directory-empty-p "~/Dokumente/org-roam/journals")
     "journals aufräumen")
    (my/auto-update-agenda-if-file-empty
     "~/Dokumente/calendar.org"
     "calendar aufräumen")
    (my/auto-update-agenda-if-file-empty
     "~/Dokumente/notes.org"
     "notes aufräumen")
    (my/auto-update-agenda-if
     (not (directory-files "~/Dokumente/org-roam/" nil (rx ".sync-conflict-")))
     "sync conflict files entfernen")
    (org-agenda))

  :general
  (:states
   '(normal visual motion emacs)
   :keymaps '(my-keys-minor-mode-map)
   :prefix "SPC"
   "n a" 'my/org-agenda
   "n c" 'org-capture
   "n l" 'org-store-link)
  :general
  (:states
   '(normal visual motion)
   :keymaps '(org-mode-map)
   :prefix "SPC"
   "m #" 'org-update-statistics-cookies
   "m '" 'org-edit-special
   "m *" 'org-ctrl-c-star
   "m +" 'org-ctrl-c-minus
   "m ," 'org-switchb
   "m ." 'org-goto
   "m @" 'org-cite-insert
   "m ." 'consult-org-heading
   "m /" 'consult-org-agenda
   "m A" 'org-archive-subtree
   "m e" 'org-export-dispatch
   "m f" 'org-footnote-action
   "m h" 'org-toggle-heading
   "m i" '(:ignore t :which-key "insert")
   "m i t" 'org-insert-structure-template
   "m i d" 'org-insert-drawer
   "m i c" 'org-insert-comment
   "m i h" 'org-insert-heading
   "m i i" 'org-insert-item
   "m i l" 'org-insert-link
   "m i p" 'org-insert-property-drawer
   "m i s" 'org-insert-subheading
   "m k" 'org-babel-remove-result
   "m n" 'org-store-link
   "m o" 'org-set-property
   "m q" 'org-set-tags-command
   "m t" 'org-todo
   "m T" 'org-todo-list
   "m x" 'org-toggle-checkbox
   "m a" '(:ignore t :which-key "attachments")
   "m a a" 'org-attach
   "m a d" 'org-attach-delete-one
   "m a d" 'org-attach-delete-all
   "m a n" 'org-attach-new
   "m a o" 'org-attach-open
   "m a o" 'org-attach-open-in-emacs
   "m a r" 'org-attach-reveal
   "m a r" 'org-attach-reveal-in-emacs
   "m a u" 'org-attach-url
   "m a s" 'org-attach-set-directory
   "m a s" 'org-attach-sync
   "m b" '(:ignore t :which-key "tables")
   "m b -" 'org-table-insert-hline
   "m b a" 'org-table-align
   "m b b" 'org-table-blank-field
   "m b c" 'org-table-create-or-convert-from-region
   "m b e" 'org-table-edit-field
   "m b f" 'org-table-edit-formulas
   "m b h" 'org-table-field-info
   "m b s" 'org-table-sort-lines
   "m b r" 'org-table-recalculate
   "m b R" 'org-table-recalculate-buffer-tables
   "m b d" '(:ignore t :which-key "delete")
   "m b d c" 'org-table-delete-column
   "m b d r" 'org-table-kill-row
   "m b i" '(:ignore t :which-key "insert")
   "m b i c" 'org-table-insert-column
   "m b i h" 'org-table-insert-hline
   "m b i r" 'org-table-insert-row
   "m b i H" 'org-table-hline-and-move
   "m b t" '(:ignore t :which-key "toggle")
   "m b t f" 'org-table-toggle-formula-debugger
   "m b t o" 'org-table-toggle-coordinate-overlays
   "m c" '(:ignore t :which-key "clock")
   "m c c" 'org-clock-cancel
   "m c d" 'org-clock-mark-default-task
   "m c e" 'org-clock-modify-effort-estimate
   "m c E" 'org-set-effort
   "m c g" 'org-clock-goto
   "m c i" 'org-clock-in
   "m c I" 'org-clock-in-last
   "m c o" 'org-clock-out
   "m c r" 'org-resolve-clocks
   "m c R" 'org-clock-report
   "m c t" 'org-evaluate-time-range
   "m c =" 'org-clock-timestamps-up
   "m c -" 'org-clock-timestamps-down
   "m d" '(:ignore t :which-key "date/dateline")
   "m d d" 'org-deadline
   "m d s" 'org-schedule
   "m d t" 'org-time-stamp
   "m d i" 'org-time-stamp-inactive
   "m g" '(:ignore t :which-key "goto")
   "m g g" 'org-goto
   "m g h" 'consult-org-heading
   "m g a" 'consult-org-agenda
   "m g c" 'org-clock-goto
   "m g i" 'org-id-goto
   "m g r" 'org-refile-goto-last-stored
   "m g x" 'org-capture-goto-last-stored
   "m l" '(:ignore t :which-key "links")
   "m l c" 'org-cliplink
   "m l i" 'org-id-store-link
   "m l l" 'org-insert-link
   "m l a" 'org-insert-all-links
   "m l s" 'org-store-link
   "m l x" 'org-insert-last-stored-link
   "m l t" 'org-toggle-link-display
   "m P" '(:ignore t :which-key "puplish")
   "m P a" 'org-publish-all
   "m P f" 'org-publish-current-file
   "m P p" 'org-publish
   "m P P" 'org-publish-current-project
   "m P s" 'org-publish-sitemap
   "m r" '(:ignore t :which-key "refile")
   "m r r" 'org-refile
   "m r R" 'org-refile-reverse
   "m s" '(:ignore t :which-key "tree/subtree")
   "m s a" 'org-toggle-archive-tag
   "m s b" 'org-tree-to-indirect-buffer
   "m s c" 'org-clone-subtree-with-time-shift
   "m s d" 'org-cut-subtree
   "m s h" 'org-promote-subtree
   "m s j" 'org-move-subtree-down
   "m s k" 'org-move-subtree-up
   "m s l" 'org-demote-subtree
   "m s n" 'org-narrow-to-subtree
   "m s r" 'org-refile
   "m s s" 'org-sparse-tree
   "m s A" 'org-archive-subtree
   "m s N" 'widen
   "m s S" 'org-sort
   "m p" '(:ignore t :which-key "priority")
   "m p d" 'org-priority-down
   "m p p" 'org-priority
   "m p u" 'org-priority-up)
  :general
  (:states
   '(normal visual emacs motion)
   :keymaps '(org-agenda-mode-map)
   "q" 'org-agenda-exit))

(use-package ol-notmuch
  :ensure t)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "SPC") nil))
  )

(use-package org-roam
  :ensure
  :commands
  (org-roam-node-list org-roam-node-file)
  :init
  (defun my/org-roam-filter-by-regex (regex)
    (lambda (file)
      (string-match regex (org-file-contents file))))

  (defun my/org-roam-list-notes-by-regex (regex)
    (seq-filter
     (my/org-roam-filter-by-regex regex)
     (mapcar #'org-roam-node-file (org-roam-node-list))))
  (defun my/org-roam-refresh-agenda-list ()
    (interactive)
    (setopt org-agenda-files
            (delete-dups
             (cons "~/Dokumente/auto.org"
                   (cons "~/Dokumente/notes.org"
                         (my/org-roam-list-notes-by-regex
                          "<[0-9]\\{4\\}\\-[0-9]\\{2\\}\\-[0-9]\\{2\\}\\|<%%(diary-float\\|^\\*+ TODO"))))
            org-caldav-files org-agenda-files))
  (my/org-roam-refresh-agenda-list)
  :custom
  (org-roam-file-exclude-regexp '("data/" ".stversions/"))
  (org-roam-directory (file-truename "~/Dokumente/org-roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
    '(("d" "default" plain "%?" :target
        (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}")
        :unnarrowed t)
      ("l" "location" plain "* %A %?" :target
        (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}")
        :unnarrowed t)))
  (org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %U %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: ~ %<%Y-%m-%d>\n"))
        ("t" "traueme" plain "#+zeit: %^{von}-%^{bis}\n* Traum 1\n%?" :target
         (file+head "%<%Y-%m-%d>-Traueme.org" "#+title: ~ Träume vom %<%d.%m.%Y>\n")
         :unnarrowed t)))
  (org-roam-db-node-include-function
    (lambda () (not (org-get-heading))))
  :config
  (org-roam-db-autosync-mode)
  (defun my/org-roam-refile ()
    "Do an org roam refile and save files."
    (interactive)
    (org-roam-refile (org-roam-node-read nil nil nil 'require-match))
    ;; TODO save only affected buffers
    (org-save-all-org-buffers))
  :general
  (:states '(normal visual motion emacs)
   :keymaps '(my-keys-minor-mode-map)
           :prefix "SPC"
           "n" '(:ignore t :which-key "org")
           "n f" 'org-roam-node-find
           "n i" 'org-roam-node-insert
           "n r" 'my/org-roam-refile
           "n d" 'org-roam-dailies-capture-today))

(use-package vterm
  :ensure
  :general
  (:states '(normal visual)
  "p" 'vterm-yank
  :prefix ","
  "e" 'vterm-send-next-key
  "c" 'vterm-clear))

(use-package link-hint
  :ensure
  :general
  (:states '(normal visual motion emacs)
   :keymaps '(my-keys-minor-mode-map)
  :prefix "SPC"
  "s l" 'link-hint-open-link))

(use-package magit
  :ensure
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk 't)
  :hook (magit-status-mode-hook . so-long-minor-mode)
  :general
  (:states '(normal visual)
           :keymaps 'magit-status-mode-map
           "S-SPC" 'magit-diff-show-or-scroll-up)
  :general
  (:states '(normal visual motion emacs)
           :keymaps '(my-keys-minor-mode-map)
           :prefix "SPC"
           "g" '(:ignore t :which-key "magit")
           "g g" 'magit-status
           "g r" 'magit-reflog-head
           "g f" 'magit-find-file
           "g c" 'magit-clone
           "g b" 'magit-blame-addition
           "g i" 'magit-init)
  :config
  (with-eval-after-load 'magit-log
    (define-key magit-log-mode-map (kbd "SPC") nil))
  (with-eval-after-load 'magit-status
    (define-key magit-status-mode-map (kbd "SPC") nil))
  (with-eval-after-load 'magit-revision
    (define-key magit-revision-mode-map (kbd "SPC") nil))
  (with-eval-after-load 'magit-revision
    (define-key magit-stash-mode-map (kbd "SPC") nil))
  (with-eval-after-load 'magit-diff
    (define-key magit-diff-mode-map (kbd "SPC") nil))

  (defun th/magit--with-difftastic (buffer command)
    "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
    (let ((process-environment
           (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                         (number-to-string (frame-width)))
                 process-environment)))
      ;; Clear the result buffer (we might regenerate a diff, e.g., for
      ;; the current changes in our working directory).
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer))
      ;; Now spawn a process calling the git COMMAND.
      (make-process
       :name (buffer-name buffer)
       :buffer buffer
       :command command
       ;; Don't query for running processes when emacs is quit.
       :noquery t
       ;; Show the result buffer once the process has finished.
       :sentinel (lambda (proc event)
                   (when (eq (process-status proc) 'exit)
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-min))
                       (ansi-color-apply-on-region (point-min) (point-max))
                       (setq buffer-read-only t)
                       (view-mode)
                       (end-of-line)
                       ;; difftastic diffs are usually 2-column side-by-side,
                       ;; so ensure our window is wide enough.
                       (let ((width (current-column)))
                         (while (zerop (forward-line 1))
                           (end-of-line)
                           (setq width (max (current-column) width)))
                         ;; Add column size of fringes
                         (setq width (+ width
                                        (fringe-columns 'left)
                                        (fringe-columns 'right)))
                         (goto-char (point-min))
                         (pop-to-buffer
                          (current-buffer)
                          `(;; If the buffer is that wide that splitting the frame in
                            ;; two side-by-side windows would result in less than
                            ;; 80 columns left, ensure it's shown at the bottom.
                            ,(when (> 80 (- (frame-width) width))
                               #'display-buffer-at-bottom)
                            (window-width
                             . ,(min width (frame-width))))))))))))

  (defun th/magit-show-with-difftastic (rev)
    "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If REV is given, just use it.
            (when (boundp 'rev) rev)
            ;; If not invoked with prefix arg, try to guess the REV from
            ;; point's position.
            (and (not current-prefix-arg)
                 (or (magit-thing-at-point 'git-revision t)
                     (magit-branch-or-commit-at-point)))
            ;; Otherwise, query the user.
            (magit-read-branch-or-commit "Revision"))))
    (if (not rev)
        (error "No revision specified")
      (th/magit--with-difftastic
       (get-buffer-create (concat "*git show difftastic " rev "*"))
       (list "git" "--no-pager" "show" "--ext-diff" rev))))

  (defun th/magit-diff-with-difftastic (arg)
    "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If RANGE is given, just use it.
            (when (boundp 'range) range)
            ;; If prefix arg is given, query the user.
            (and current-prefix-arg
                 (magit-diff-read-range-or-commit "Range"))
            ;; Otherwise, auto-guess based on position of point, e.g., based on
            ;; if we are in the Staged or Unstaged section.
            (pcase (magit-diff--dwim)
              ('unmerged (error "unmerged is not yet implemented"))
              ('unstaged nil)
              ('staged "--cached")
              (`(stash . ,value) (error "stash is not yet implemented"))
              (`(commit . ,value) (format "%s^..%s" value value))
              ((and range (pred stringp)) range)
              (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
    (let ((name (concat "*git diff difftastic"
                        (if arg (concat " " arg) "")
                        "*")))
      (th/magit--with-difftastic
       (get-buffer-create name)
       `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

  (transient-define-prefix th/magit-aux-commands ()
    "My personal auxiliary magit commands."
    ["Auxiliary commands"
     ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
     ("s" "Difftastic Show" th/magit-show-with-difftastic)])
  (transient-append-suffix 'magit-dispatch "!"
    '("#" "My Magit Cmds" th/magit-aux-commands))

  (define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands))

(use-package orgit
  :ensure t)

(use-package git-timemachine
  :ensure t
  :after hydra
  :general
  (:states '(normal visual motion emacs)
 :keymaps '(my-keys-minor-mode-map)
  :prefix "SPC"
  "g t" 'git-timemachine)
  :config
  (defhydra hydra-git-timemachine (git-timemachine-mode-map "SPC m t")
    "timemachine transient state"
    ("n" git-timemachine-show-next-revision "next revision")
    ("p" git-timemachine-show-previous-revision "previous revision")
    ("j" evil-scroll-down "scroll down")
    ("k" evil-scroll-up "scroll up")
    ("s" consult-line "search")
    ("b" magit-blame-addition "blame")
    ))

(use-package forge
  :after magit
  :ensure t
  :general
  (:states
   '(normal visual motion)
   :keymaps '(forge-pullreq-mode-map)
   :prefix "SPC"
   "m r" '(forge-topic-set-review-requests :which-key "request reviews")
   "m k" 'forge-delete-comment
   "m n" 'forge-create-post
   "m RET" 'forge-topic-menu
   "m c" '(magit-dispatch :which-key "magit-dispatch")
   "m e" 'magit-edit-thing
   "m o" 'magit-browse-thing
   "m w" 'magit-copy-thing
   "m d" 'magit-topic-toggle-draft)
  :general
  (:states
   '(normal visual motion)
   :keymaps '(forge-post-mode-map)
   :prefix "SPC"
   "m '" 'markdown-edit-code-block
   "m -" 'markdown-insert-hr
   "m <" 'markdown-outdent-region
   "m <down>" 'markdown-move-down
   "m <left>" 'markdown-promote
   "m <right>" 'markdown-demote
   "m <up>" 'markdown-move-up
   "m >" 'markdown-indent-region
   "m /" 'markdown-promote
   "m =" 'markdown-demote
   "m h" 'markdown-mark-subtree
   "m ]" 'markdown-complete
   "m a" '(:ignore t :which-key "markdown link")
   "m a L" 'markdown-insert-link
   "m a f" 'markdown-insert-footnote
   "m a l" 'markdown-insert-link
   "m a r" 'markdown-insert-link
   "m a u" 'markdown-insert-uri
   "m a w" 'markdown-insert-wiki-link
   "m b" 'markdown-outline-previous-same-level
   "m c" '(forge-post-submit :which-key "forge-post-submit")
   "m d" 'markdown-do
   "m e" 'forge-post-dispatch
   "m f" 'markdown-outline-next-same-level
   "m j" 'markdown-insert-list-item
   "m k" 'forge-post-cancel
   "m l" 'markdown-insert-link
   "m n" 'markdown-outline-next
   "m o" 'markdown-follow-thing-at-point
   "m p" 'markdown-outline-previous
   "m i" '(:ignore t :which-key "markdown insert")
   "m i !" 'markdown-insert-header-setext-1
   "m i -" 'markdown-insert-hr
   "m i 1" 'markdown-insert-header-atx-1
   "m i 2" 'markdown-insert-header-atx-2
   "m i 3" 'markdown-insert-header-atx-3
   "m i 4" 'markdown-insert-header-atx-4
   "m i 5" 'markdown-insert-header-atx-5
   "m i 6" 'markdown-insert-header-atx-6
   "m i @" 'markdown-insert-header-setext-2
   "m i C" 'markdown-insert-gfm-code-block
   "m i F" 'markdown-insert-foldable-block
   "m i H" 'markdown-insert-header-setext-dwim
   "m i P" 'markdown-pre-region
   "m i Q" 'markdown-blockquote-region
   "m i [" 'markdown-insert-gfm-checkbox
   "m i b" 'markdown-insert-bold
   "m i c" 'markdown-insert-code
   "m i d" 'markdown-insert-strike-through
   "m i e" 'markdown-insert-italic
   "m i f" 'markdown-insert-footnote
   "m i h" 'markdown-insert-header-dwim
   "m i i" 'markdown-insert-italic
   "m i k" 'markdown-insert-kbd
   "m i l" 'markdown-insert-link
   "m i p" 'markdown-insert-pre
   "m i q" 'markdown-insert-blockquote
   "m i s" 'markdown-insert-strike-through
   "m i t" 'markdown-insert-table
   "m i w" 'markdown-insert-wiki-link
   "m t" '(:ignore t :which-key "markdown table")
   "m t <down>" 'markdown-table-insert-row
   "m t <left>" 'markdown-table-delete-column
   "m t <right>" 'markdown-table-insert-column
   "m t <up>" 'markdown-table-delete-row
   "m t !" 'markdown-insert-header-setext-1
   "m t 1" 'markdown-insert-header-atx-1
   "m t 2" 'markdown-insert-header-atx-2
   "m t 3" 'markdown-insert-header-atx-3
   "m t 4" 'markdown-insert-header-atx-4
   "m t 5" 'markdown-insert-header-atx-5
   "m t 6" 'markdown-insert-header-atx-6
   "m t @" 'markdown-insert-header-setext-2
   "m t H" 'markdown-insert-header-setext-dwim
   "m t h" 'markdown-insert-header-dwim
   "m t s" 'markdown-insert-header-setext-2
   "m t t" 'markdown-insert-header-setext-1
   "m u" 'markdown-outline-up
   "m x" '(:ignore t :which-key "mardown toogle")
   "m x e" 'markdown-toggle-math
   "m x f" 'markdown-toggle-fontify-code-blocks-natively
   "m x l" 'markdown-toggle-url-hiding
   "m x x" 'markdown-toggle-gfm-checkbox
   "m x RET" 'markdown-toggle-markup-hiding
   "m x TAB" 'markdown-toggle-inline-images
   "m x d" 'markdown-move-down
   "m x l" 'markdown-promote
   "m x m" 'markdown-insert-list-item
   "m x r" 'markdown-demote
   "m x u" 'markdown-move-up
   "m TAB" 'markdown-insert-image))

(use-package pr-review
  :after magit
  :ensure t
  :init
  (defun my/pr-review-at-point ()
    "opens the pull request at point in pr-review"
    (interactive)
    (if-let ((url (forge-get-url (or (forge-post-at-point)
                                     (forge-current-topic)))))
        (pr-review url)
      (user-error "Nothing at point with a URL")))
  (defun my/pr-review-in-buffer ()
    "opens the pull request which is nearest point"
    (interactive)

    (if-let ((txt (buffer-substring-no-properties 1 (buffer-end 1)))
             (_ (string-match (rx (seq "https://github.com" (repeat 2 2 (seq "/" (one-or-more (not "/")))) "/pull/"  (one-or-more num))) txt))
             (url (match-string 0 txt)))
        (pr-review url)))

  :general
  (:states
   '(normal visual motion)
   :keymaps '(pr-review-mode-map)
   :prefix "SPC"
   "m c" '(pr-review-context-comment :which-key "pr-review-context-comment")
   "m e" 'pr-review-context-edit
   "m f" 'pr-review-goto-file
   "m o" 'pr-review-open-in-default-browser
   "m q" 'pr-review-request-reviews
   "m r" '(pr-review-refresh :which-key "pr-review-refresh")
   "m a" '(pr-review-context-action :which-key "pr-review-context-action")
   "m v" 'pr-review-view-file
   "m p" '(pr-review-add-pending-review-thread :which-key "pr-review-add-pending-review-thread")
   "m s" '(pr-review-submit-review :which-key "pr-review-submit-review")))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :general
  (:states '(normal visual motion emacs)
 :keymaps '(my-keys-minor-mode-map)
  :prefix "SPC"
   "c x" 'flycheck-list-errors))

(use-package flycheck-eglot
  :ensure t
  :config
  (global-flycheck-eglot-mode 1))

(use-package eglot-booster
        :after eglot
        :config (eglot-booster-mode))
;; Maybe https://github.com/manateelazycat/lsp-bridge

(use-package lsp-mode
  :hook (;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package nix-mode
  :mode "\\.nix\\'")

(defun my/meqo-server-respository? ()
  (and (project-current)
       (string-match
        (rx (and "/meqo/" (zero-or-more anychar) (zero-or-one "server/") line-end))
        (project-root (project-current)))))

(use-package haskell-mode :ensure t)

(add-hook
 'find-file-hook
 (lambda ()
   (when (s-ends-with? ".agda" (buffer-file-name))
     (load-file (let ((coding-system-for-read 'utf-8))
                  (shell-command-to-string "agda-mode locate")))
     (envrc-propagate-environment 'agda2-mode)
     (defun my/agda2-restart-envrc ()
       "Tries to start or restart the Agda process in envrc."
       (interactive)
       (envrc-propagate-environment 'agda2-restart))
     (general-define-key
      :states '(normal visual motion)
      :keymaps '(agda2-mode-map)
      :prefix "SPC"
      "m l" '(agda2-load :which-key "Load")
      "m x l" 'agda2-load
      "m x c" '(agda2-compile :which-key "Compile")
      "m x q" '(agda2-quit :which-key "Quit")
      "m x r" '(my/agda2-restart-envrc :which-key "Kill and restart Agda")
      "m x a" '(agda2-abort :which-key "Abort a command")
      "m x d" '(agda2-remove-annotations :which-key "Remove goals and highlighting (\"deactivate\")")
      "m x h" '(agda2-display-implicit-arguments :which-key "Toggle display of hidden arguments")
      "m x i" '(agda2-display-irrelevant-arguments :which-key "Toggle display of irrelevant arguments")
      "m =" '(agda2-show-constraints :which-key "Show constraints")
      "m s" '(agda2-solve-maybe-all :which-key "Solve constraints")
      "m ?" '(agda2-show-goals :which-key "Show goals")
      "m f" '(agda2-next-goal :which-key "Next goal") ; Forward.
      "m b" '(agda2-previous-goal :which-key "Previous goal") ; Back.
      "m SPC" '(agda2-give :which-key "Give")
      "m m" '(agda2-elaborate-give :which-key "Elaborate and Give")
      "m r" '(agda2-refine :which-key "Refine")
      "m a" '(agda2-mimer-maybe-all :which-key "Auto")
      "m c" '(agda2-make-case :which-key "Case")
      "m t" '(agda2-goal-type :which-key "Goal type")
      "m e" '(agda2-show-context :which-key "Context (environment)")
      "m h" '(agda2-helper-function-type :which-key "Helper function type")
      "m d" '(agda2-infer-type-maybe-toplevel :which-key "Infer (deduce) type")
      "m w" '(agda2-why-in-scope-maybe-toplevel :which-key "Explain why a particular name is in scope")
      "m ," '(agda2-goal-and-context :which-key "Goal type and context")
      "m ." '(agda2-goal-and-context-and-inferred :which-key "Goal type, context and inferred type")
      "m ;" '(agda2-goal-and-context-and-checked :which-key "Goal type, context and checked type")
      "m z" '(agda2-search-about-toplevel :which-key "Search About")
      "m o" '(agda2-module-contents-maybe-toplevel :which-key "Module contents")
      "m n" '(agda2-compute-normalised-maybe-toplevel :which-key "Evaluate term to normal form")
      "m x ;" '(agda2-comment-dwim-rest-of-buffer :which-key "Comment/uncomment the rest of the buffer")
      "m x s" '(agda2-set-program-version :which-key "Switch to another version of Agda")))))

(use-package rustic :ensure
  :init
  (setq rustic-lsp-client 'eglot))

(use-package protobuf-mode :ensure t)

(use-package tide
  :ensure t
  :after (company flycheck envrc)
  :custom
  (tide-server-max-response-length 204800)
  (typescript-ts-mode-indent-offset 4)
  (tide-node-executable "node")
  (tide-disable-suggestions t)
  :init
  (defun my/set-tide-from-direnv ()
    "sets the right node executable"
     (interactive)
     (setq
      tide-node-executable
      (string-trim (shell-command-to-string "direnv exec . which node 2>/dev/null || echo node"))))
  (add-hook 'typescript-ts-mode-hook (lambda () (setq indent-tabs-mode nil)))
  (add-hook 'tide-mode-hook (lambda () (setq indent-tabs-mode nil)))
  (add-hook 'typescript-ts-mode-hook 'my/set-tide-from-direnv -90)
  (add-hook 'tsx-ts-mode-hook 'my/set-tide-from-direnv -90)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode))
  :general
  (:states '(normal visual motion)
   :keymaps '(tsx-ts-mode-map typescript-ts-mode-map)
  "g r" 'tide-references)
  (:states '(normal visual motion)
   :keymaps '(tsx-ts-mode-map typescript-ts-mode-map)
  :prefix "SPC"
  "m r" 'tide-rename-symbol
  "m f" 'tide-rename-file
  "m a" 'tide-fix)
  :config
  (evil-add-command-properties #'tide-goto-line-reference :jump t)
  (evil-add-command-properties #'tide-jump-to-definition :jump t))

(use-package csv-mode
  :ensure t
  :hook (csv-mode . csv-guess-set-separator))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package notmuch
  :ensure t
  :init
  (setq-default notmuch-search-oldest-first nil)
  :custom
  (mml-secure-openpgp-sign-with-sender 't)
  (notmuch-search-oldest-first 'f)
  (message-default-mail-headers "Cc: \nBcc: \n")
  ;; postponed message is put in the following draft directory
  (message-auto-save-directory "~/Maildir/draft")
  ;; change the directory to store the sent mail
  (message-directory "~/Maildir/")
  (send-mail-function 'sendmail-send-it)
  (sendmail-program "msmtp")
  (mail-specify-envelope-from t)
  (mail-envelope-from 'header)
  (message-sendmail-envelope-from 'header)
  (notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox and not tag:trash and date:3M..today" :key "i")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "must read" :query "not tag:rechnung and not tag:agb and not tag:anmeldung and not tag:trash and date:3M..today" :key "r")
          (:name "sent" :query "tag:sent" :key "s")
          (:name "drafts" :query "tag:draft" :key "d")
          (:name "all mail" :query "date:3M..today" :key "a")))
  :init
  (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
  (defun my/notmuch-update ()
    "Update mails and notmuch buffer"
    (interactive)
    (with-current-buffer (compile "mbsync -a && notmuch new")
    (add-hook
     'compilation-finish-functions
     (lambda (buf status)
       (if (equal status "finished\n")
           (progn
             (delete-windows-on buf)
             (bury-buffer buf)
             (notmuch-refresh-all-buffers)
             (message "Notmuch sync successful"))
         (user-error "Failed to sync notmuch data")))
     nil
     'local)))
  (defun my/notmuch-edit-draft ()
    "Resume editing draft"
    (interactive)
    (notmuch-draft-resume (notmuch-show-get-message-id)))
  (defun my/notmuch-search ()
    "Open notmuch in search mode"
    (interactive)
     (notmuch-search "tag:inbox and date:3M..today"))
   ;;;###autoload
  (defun +notmuch/compose ()
    "Compose new mail"
    (interactive)
    (let* ((from (completing-read "From: " (notmuch-user-emails)))
          (mail-signature (if (string= from nix-workmail) nix-workmail-signature "")))
      (notmuch-mua-mail
       nil
       nil
       (list (cons 'From from)))
      (mail-signature)))
   (defun my/notmuch-trash ()
     "Add trash label to mail"
     (interactive)
     (notmuch-search-add-tag '("+trash" "-inbox" "-flagged" "-unread")))
   (advice-mapc (lambda (a r) (advice-remove 'notmuch-show-reply-sender a)) 'notmuch-show-reply-sender)
   (advice-mapc (lambda (a r) (advice-remove 'notmuch-show-reply a)) 'notmuch-show-reply)
   (advice-add
    'notmuch-show-reply-sender
    :after
    (lambda
     (&rest args)
     (let* ((from (message-sendmail-envelope-from))
           (mail-signature (if (-any (lambda (address) (string=  from address)) `(,nix-workmail "engel@medilyse.org")) nix-workmail-signature "")))
       (mail-signature))))
  :general
  (:states '(normal visual)
   :prefix "SPC"
   :keymaps '(notmuch-search-mode-map)
   "m u" '(my/notmuch-update :which-key "update mails")
   "m c" '(+notmuch/compose :which-key "compose mail"))
  (:states '(normal visual)
   :prefix "SPC"
   :keymaps '(notmuch-message-mode-map)
   "m c" '(notmuch-mua-send-and-exit :which-key "send mail")
   "m a" '(mml-attach-file :which-key "attach file")
   "m d" '(notmuch-draft-save :which-key "save as draft"))
  (:states '(normal visual)
   :keymaps '(notmuch-search-mode-map)
   "d" 'my/notmuch-trash
   "t" 'notmuch-search-add-tag)
  :config
  (setq notmuch-fcc-dirs "Sent -unread +sent"))

(use-package elfeed
  :ensure t
  :custom
  (rmh-elfeed-org-files (list "~/Dokumente/elfeed.org"))
  (elfeed-search-filter "@6-days-ago +unread +favorite")
  :general
  (:states '(normal visual motion)
   :keymaps '(elfeed-search-mode-map)
   "b" 'elfeed-search-browse-url))

(use-package elfeed-org
  :ensure t
  :custom
  (rmh-elfeed-org-files (list "~/Dokumente/elfeed.org"))
  :config
  (elfeed-org))

(use-package ement :ensure t)

(use-package empv
 :custom (empv-invidious-instance "https://yt.artemislena.eu/api/v1")
 :config
 ; TODO find better bindings
 (defhydra hydra-empv (evil-normal-state-map "SPC o i")
   "music"
    ("j" empv-playlist-next "next")
    ("k" empv-playlist-prev "prev")
    ("p" empv-toggle "play/pause")
    ("g" empv-volume-up "volume up")
    ("h" empv-volume-down "volume down")
    ("s" empv-playlist-select "select")))

(use-package peertube
  :ensure t
  :init
  (defun peertube-mpv-open-video ()
    "Open the video under the cursor using `browse-url'."
    (interactive)
    (let ((url (peertube-video-url (peertube--get-current-video))))
      (browse-url-mpv url)))
  (defun peertube-mpv-open-audio ()
    "Open the video under the cursor using `browse-url'."
    (interactive)
    (let ((url (peertube-video-url (peertube--get-current-video))))
      (browse-url-mpv-audio url)))
  :general
  (:states '(normal visual)
  :keymaps 'peertube-mode-map
  "enter" 'peertube-mpv-open-video
  "o" 'peertube-mpv-open-video
  "a" 'peertube-mpv-open-audio
  "c" 'peertube-goto-channel
  "i" 'peertube-show-video-info
  "d" 'peertube-download-video
  "q" 'peertube-quit
  "s" 'peertube-search
  "m" 'peertube-change-sort-method
  "t" 'peertube-preview-thumbnail))

(use-package async)

(use-package org-caldav
  :init
  (defun org-caldav-timestamp-has-time-p (timestamp)
  "Checks whether a timestamp has a time.
Returns nil if not and (sec min hour) if it has."
  (let ((ti (org-parse-time-string timestamp)))
    (or (nth 0 ti) (nth 1 ti) (nth 2 ti))))
  (defun org-caldav-skip-function (backend)
    (org-caldav-debug-print 2 "Skipping over excluded entries")
    (when (eq backend 'icalendar)
      (org-map-entries
       (lambda ()
         (let ((pt (save-excursion (apply 'org-agenda-skip-entry-if org-caldav-skip-conditions)))
               (ts (when org-caldav-days-in-past (* (abs org-caldav-days-in-past) -1)))
               (stamp (or (org-entry-get nil "TIMESTAMP" t) (org-entry-get nil "CLOSED" t))))
           (when (or pt (and stamp ts (> ts (org-time-stamp-to-now stamp))))
             (delete-region (point) (progn (org-next-visible-heading 1) (point)))
             (setq org-map-continue-from (point)))))))
    (org-caldav-debug-print 2 "Finished skipping"))
  :custom
  (org-export-with-broken-links t)
  (org-caldav-url "http://flospi:5232")
  (org-caldav-inbox "~/Dokumente/calendar.org")
  (org-caldav-calendar-id "any/d02d9914-eb09-4210-b1d1-1b330b274af9/")
  (org-caldav-resume-aborted 'never)
  (org-caldav-delete-calendar-entries 'always)
  (org-caldav-skip-conditions '(nottimestamp scheduled deadline))
  )

(use-package jinx
  :custom
  (jinx-languages "de_DE en-computers en_US")
  :init
  (global-jinx-mode 1)
  :config
  (setq vertico-multiform-categories
             '((jinx grid (vertico-grid-annotate . 20))))
  :general
  (:states '(normal visual)
   :prefix "SPC"
   "j" 'jinx-correct))

(recentf-mode 1)
(winner-mode 1)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

;; Browser function
(setq no-eww-reg (rx (or "youtube." "youtu.be" "v.reddit.it" "dailymotion."
                         "github.com" "streamable.com"  "liveleak.com"
                         "vimeo.com" "gfycat.com" "atlassian.com" "atlassian.net"
                         (and (or ".mp3" ".mp4" ".m4v" ".svg" ".gif" ".gifv")
                              line-end))))

(setq browse-url-handlers
      `((,(rx (and line-start "mailto:")) .
         ,(lambda (url &rest args)
            (notmuch-mua-mail
             (string-remove-prefix "mailto:" url)
             nil
             `((From  . ,(completing-read "From: " (notmuch-user-emails)))))))
        (,no-eww-reg . browse-url-linkopen)
        ("." . eww-browse-url)))

(defun browse (prog url)
  (setq url (browse-url-encode-url url))
  (apply #'start-process (append `(,(concat (car prog) " " url) nil) prog `(,url))))
(defun browse-url-firefox-new-window (url &rest agrs)
  (browse-url-firefox url t))

(setq browse-url-secondary-browser-function 'browse-url-firefox-new-window)

(general-define-key
 :states '(normal visual motion)
 "g s" 'evil-avy-goto-char
 "g r" 'xref-find-references)

(general-define-key :states '(normal visual)
  :keymaps '(Info-mode-map)
  "s" 'consult-info)

(general-define-key
 :keymaps '(text-mode-map prog-mode-map)
 "<tab>" 'completion-at-point)

(defun my/delete-this-file ()
  (interactive)
  (if (yes-or-no-p (format "Really delete file: %s " (buffer-file-name)))
      (progn (delete-file (buffer-file-name))
             (kill-current-buffer))))

;; mostly copied from doom emacs
(defun my/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (copy-file old-path new-path (or force-p 1))
    (message "File copied to %S" (abbreviate-file-name new-path))))

;; mostly copied from doom emacs
(defun my/move-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (kill-current-buffer)
    (find-file new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))

;; mostly copied from doom emacs
(defun my/yank-buffer-path (&optional root)
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or (buffer-file-name (buffer-base-buffer))
                        (bound-and-true-p list-buffers-directory)))
      (let ((path (abbreviate-file-name
                   (if root
                       (file-relative-name filename root)
                     filename))))
        (kill-new path)
        (if (string= path (car kill-ring))
            (message "Copied path: %s" path)
          (user-error "Couldn't copy filename in current buffer")))
    (error "Couldn't find filename in current buffer")))

(defun my/yank-buffer-path-relative-to-project ()
  "Copy the current buffer's path to the kill ring."
  (interactive "P")
  (my/yank-buffer-path
     (project-root (project-current))))

;;;###autoload
(defun project-vterm ()
  "Start vterm in the current project's root directory.
If a buffer already exists for running vterm in the project's root,
switch to it.  Otherwise, create a new vterm buffer.
With \\[universal-argument] prefix arg, create a new vterm buffer even
if one already exists."
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
      (vterm t))))

;;;###autoload
(defun project-magit ()
  "Start magit in the current project's root directory."
  (interactive)
  (let ((default-directory (expand-file-name (project-root (project-current t)))))
    (magit-status)))

(setq project-switch-commands
  '((project-find-file "Find file")
   (my/consult-ripgrep "Find regexp" "r")
   (project-magit "git" "g")
   (project-eshell "Eshell")
   (project-vterm "terminal" "t")))

(defun my/consult-ripgrep ()
  "Changes consult-ripgrep so that we grep for evil selection and ignore .ignore in meqo respository"
  (interactive)
  (let ((consult-ripgrep-args
         (if (my/meqo-server-respository?)
             (string-join `(,consult-ripgrep-args " --no-ignore-dot"))
                          consult-ripgrep-args)))
    (if evil-visual-region-expanded
        (consult-ripgrep nil (regexp-quote (buffer-substring-no-properties (mark) (point))))
      (consult-ripgrep))))

(defun my/consult-ripgrep-from-kill-ring ()
  (interactive)
  (consult-ripgrep nil (read-from-kill-ring "ripgrep")))

(defun my/consult-line ()
  (interactive)
  (if evil-visual-region-expanded
    (let ((region (buffer-substring-no-properties (mark) (point))))
      (evil-force-normal-state)
      (consult-line (replace-regexp-in-string " " "\\\\s-" (regexp-quote region))))
    (consult-line)))

(defun my/consult-line-from-kill-ring ()
  "Select consult line search term from kill ring."
  (interactive)
  (consult-line (read-from-kill-ring "Consult line")))

(defun my/switch-pdf-txt ()
  "Open corresponding pdf to txt file or vice versa if it exist."
  (interactive)
  (if (string-equal (file-name-extension buffer-file-name) "pdf")
      (find-file (concat (file-name-base (buffer-name)) ".txt"))
    (find-file (concat (file-name-base (buffer-name)) ".pdf"))))

(defun my/rename-pdf-txt (new-base &optional force-p)
  (interactive
   (list (read-string "Move scan to: ")
         current-prefix-arg))
  "Move current pdf/txt pair to new base name NEW-BASE.
   If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-base (file-name-base buffer-file-name))
        (current-extension (file-name-extension buffer-file-name)))
    (unless (or (string-equal current-extension "pdf") (string-equal current-extension "txt"))
      (user-error "Buffer is not visiting pdf or txt file"))
    (rename-file (file-name-with-extension old-base ".pdf") (file-name-with-extension new-base ".pdf") (or force-p 1))
    (rename-file (file-name-with-extension old-base ".txt") (file-name-with-extension new-base ".txt") (or force-p 1))
    (kill-current-buffer)
    (find-file (file-name-with-extension new-base current-extension))
    (message "Scan moved to %S" new-base)))

(defvar scans-minor-mode-map
  (make-sparse-keymap)
  "scans-minor-mode keymap.")

(define-minor-mode scans-minor-mode
  "Toggle scans mode.  Keybindings for my scans."
 :init-value t)

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 :keymaps 'scans-minor-mode-map
 "m o" '(my/switch-pdf-txt :which-key "switch to the corresponding pdf/txt")
 "m r" '(my/rename-pdf-txt :which-key "rename this pdf/txt pair"))

(defun my/enable-scans-minor-mode ()
  (if (s-prefix? (concat "/home/" (user-login-name) "/Dokumente/scans") buffer-file-name)
      (scans-minor-mode 1)
    (scans-minor-mode -1)))

(add-hook 'post-command-hook 'my/enable-scans-minor-mode)

(defvar my-keys-minor-mode-map
  (make-sparse-keymap)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(general-define-key :states '(normal visual motion emacs)
 :keymaps '(my-keys-minor-mode-map)
 :prefix "SPC"
 ":" 'execute-extended-command
 "h" '(:ignore t :which-key "Help")
 "h b b" 'embark-bindings
 "h f" 'helpful-callable
 "h v" 'helpful-variable
 "h k" 'helpful-key
 "h m" 'describe-mode
 "h p" 'describe-package
 "h i" 'info
 "f" '(:ignore t :which-key "file")
 "f c" '(my/copy-this-file :which-key "Copy this file")
 "f d" '(my/delete-this-file :which-key "Delete this file")
 "f f" '(find-file :which-key "Find file")
 "f l" '(locate :which-key "Locate file")
 "f r" '(consult-recent-file :which-key "Recent files")
 "f R" '(my/move-this-file :which-key "Rename/move file")
 "f s" '(save-buffer :which-key "Save file")
 "f S" '(write-file :which-key "Save file as...")
 "f y" '(my/yank-buffer-path :which-key "Yank file path")
 "f Y" '(my/yank-buffer-path-relative-to-project :which-key "Yank file path from project")
 "b" '(:ignore t :which-key "Buffer")
 "b b" 'consult-buffer
 "b r" 'revert-buffer
 "b d" 'kill-current-buffer
 "b s" 'save-some-buffers
 "w" '(:ignore t :which-key "Window")
 "w m m" 'maximize-window
 "w j" 'evil-window-down
 "w k" 'evil-window-up
 "w h" 'evil-window-left
 "w l" 'evil-window-right
 "w v" 'evil-window-vsplit
 "w s" 'evil-window-split
 "w d" 'evil-window-delete
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
 "s" '(:ignore t :which-key "search")
 "s s" 'my/consult-line
 "s i" 'imenu
 "SPC" 'project-find-file
 "p" '(:ignore t :which-key "project")
 "p p" 'project-switch-project
 "p s" 'my/consult-ripgrep
 "s p" 'my/consult-ripgrep
 "." '(find-file :which-key "Find file" )
 "q" '(:ignore t :which-key "quit")
 "q q" 'save-buffers-kill-terminal
 "c" '(:ignore t :which-key "code")
 "c c" 'compile
 "c i" 'eldoc-print-current-symbol-info
 "c r" 'eglot-rename
 "o" '(:ignore t :which-key "open")
 "o e" 'elfeed
 "o c" 'ement-connect
 "o t" 'vterm
 "o y" 'ytel
 "o p" 'peertube
 "o m" '(my/notmuch-search :which-key "notmuch")
 "k" '(:ignore t :which-key "killring")
 "k y" 'yank-from-kill-ring
 "k p" 'my/consult-ripgrep-from-kill-ring
 "k s" 'my/consult-line-from-kill-ring)

(my-keys-minor-mode 1)

(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.

Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
    (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(add-hook 'after-load-functions 'my-keys-have-priority)

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 :keymaps 'emacs-lisp-mode-map
  "m e e" 'eval-last-sexp)

;; Font size adjustment
(defun hoagie-adjust-font-size (frame)
  "Inspired by https://emacs.stackexchange.com/a/44930/17066. FRAME is ignored.
If I let Windows handle DPI everything looks blurry."
  (if (equal (getenv "XDG_SESSION_TYPE") "x11")
      (let* ((attrs (frame-monitor-attributes)) ;; gets attribs for current frame
             (width-px (-fourth-item (-second-item attrs))))
        (cond ((eq width-px 2560) (set-frame-font "monospace 20"))  ;; laptop screen
              ((eq width-px 1920) (set-frame-font "monospace 18")) ;; External monitor at home
              (t (set-frame-font "monospace 18"))))
    (set-frame-font "monospace 12")))

(add-hook 'window-size-change-functions #'hoagie-adjust-font-size)

(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(dolist (mapping '((python-mode . python-ts-mode)
                   (css-mode . css-ts-mode)
                   (typescript-mode . tsx-ts-mode)
                   (js-mode . js-ts-mode)
                   (css-mode . css-ts-mode)
                   (yaml-mode . yaml-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

(setq evil-fold-list
  '(((hs-minor-mode)
     :open-all   hs-show-all
     :close-all  hs-hide-all
     :toggle     hs-toggle-hiding
     :open       hs-show-block
     :open-rec   nil
     :close      hs-hide-block
   )))

(add-hook 'prog-mode-hook 'hs-minor-mode)

;; tabs
(setq indent-tabs-mode nil)
(add-hook 'js-json-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'nxml-mode-hook (lambda () (setq indent-tabs-mode nil)))

(defun my/meqo-server-directory ()
  "Gets the server directory of meqo."
  (let ((root (project-root (project-current))))
    (if (s-suffix? "server" root)
        root
      (string-join `(,root "server/")))))

(defun my/meqo-demo-directory ()
  "Gets the demo directory of meqo."
  (string-join `(,(my/meqo-server-directory) "demo/")))

(defun my/meqo-format ()
  "Formats the current buffer with the meqo script."
  (interactive)
  (let ((default-directory (my/meqo-server-directory)))
    (shell-command
     (string-join
      `("direnv exec scripts/format_one.sh" " " ,(buffer-file-name (current-buffer)))))
    (revert-buffer :NOCONFIRM t)))

(defun my/meqo-format-on-save ()
  "Formats the current buffer with the meqo script if the project is meqo."
  (when (and (project-current)
	     (my/meqo-server-respository?)
	     (string-match (rx (or (seq ".ts" eol) (seq ".tsx" eol) (seq ".hs" eol)))
			   (buffer-file-name (current-buffer)))
	     (not (string-match (rx (seq line-start "/demo/" (one-or-more anychar)))
				(buffer-file-name (current-buffer)))))
    (my/meqo-format)))

(add-hook 'after-save-hook 'my/meqo-format-on-save)

(defun my/meqo-demo-format ()
  "Formats the current buffer with the meqo demo script."
  (interactive)
  (let ((default-directory (my/meqo-demo-directory)))
    (shell-command
     (string-join
      `("direnv exec . npm run format -- --write" " " ,(buffer-file-name (current-buffer)))))
    (revert-buffer :NOCONFIRM t)))

(defun my/meqo-demo-format-on-save ()
  "Formats the current buffer with the meqo script if the project is meqo demo."
  (when (and (project-current)
             (my/meqo-server-respository?)
             (string-match (rx (seq "/demo/" (one-or-more (not "/")) (or (seq ".ts" eol) (seq ".js" eol) (seq ".xml" eol) (seq ".json" eol) (seq ".json5" eol))))
                           (buffer-file-name (current-buffer))))
    (my/meqo-demo-format)))

(add-hook 'after-save-hook 'my/meqo-demo-format-on-save)

(defun my/meqo-shake-targets ()
  "Selects a target from shake."
  (let* ((default-directory (my/meqo-server-directory))
         (programs
          (-filter
           (lambda (x) (string-match (rx (seq alnum " " "(" (one-or-more anychar) ")" )) x))
           (s-split "\n" (shell-command-to-string (format "direnv exec %s ./shake -l" default-directory)))))
         (what (completing-read "programm: " programs)))
    (car (s-split " " what))))

(define-compilation-mode meqo-compilation-mode "Meqo-Compile"
  "Meqo compilation mode."
  (let ((prefix "ERROR in ."))
    (set
     (make-local-variable 'compilation-error-regexp-alist)
     (cons
     `(,(rx (group (literal prefix) (one-or-more (not ":"))) ":" (group (one-or-more digit)) ":" (group (one-or-more digit)) (not digit)) 1 2 3 nil 1) compilation-error-regexp-alist))
    (set
     (make-local-variable 'compilation-transform-file-match-alist)
     `((,(rx "server/protos") "/protos")
       (,(rx (literal prefix)) "/js")))))

(defun my/meqo-compile ()
  "Uses shake in meqo to compile."
  (interactive)
  (let* ((default-directory (my/meqo-server-directory))
         (target (my/meqo-shake-targets)))
    (compile (format "way=f cd %s && direnv exec %s ./shake %s" default-directory default-directory target) 'meqo-compilation-mode)))

(defun my/meqo-webpack-entry-points ()
  "Selects a entry points from webpack."
  (let* ((default-directory (my/meqo-server-directory))
         (programs
           (s-split "\n" (shell-command-to-string (format "direnv exec %s ./js/scripts/webpack --list" default-directory))))
         (what (completing-read "entry point: " programs)))
    (car (s-split " " what))))

(define-compilation-mode meqo-webpack-compilation-mode "Meqo-Webpack"
  "Meqo ts webpack compilation mode."
  (let ((prefix "ERROR in ."))
    (set
     (make-local-variable 'compilation-error-regexp-alist)
     `((,(rx (group (literal prefix) (one-or-more (not ":"))) ":" (group (one-or-more digit)) ":" (group (one-or-more digit)) (not digit)) 1 2 3 nil 1)))
    (set
     (make-local-variable 'compilation-transform-file-match-alist)
       `((,(rx (literal prefix)) ,(string-join `(,(my/meqo-server-directory) "/js")))))))

(defun my/meqo-webpack ()
  "Run wepack"
  (interactive)
  (let* ((default-directory (my/meqo-server-directory))
         (entry-point (my/meqo-webpack-entry-points)))
    (compile (format "way=f cd %s && direnv exec %s ./js/scripts/webpack --watch %s" default-directory default-directory entry-point) 'meqo-webpack-compilation-mode)))

(define-compilation-mode meqo-ts-test-compilation-mode "Meqo-Test"
  "Meqo ts tests compilation mode."
  (let ((prefix "(build/webpack/dev/tests/webpack:/checkpad-workflow"))
    (set
     (make-local-variable 'compilation-error-regexp-alist)
     `((,(rx (group (literal prefix) (one-or-more (not ":"))) ":" (group (one-or-more digit)) ":" (group (one-or-more digit)) (not digit)) 1 2 3 nil 1)))
    (set
     (make-local-variable 'compilation-transform-file-match-alist)
     `((,(rx (or ":/checkpad-workflow/src/checkpad/testInfrastructure" ":/checkpad-workflow/src/checkpad/util" "helpers.ts")) nil)
       (,(rx (literal prefix)) ,(string-join `(,(my/meqo-server-directory) "/js")))))))


(defun my/meqo-run-ts-tests ()
  "Run meqo ts tests"
  (interactive)
  (let* ((default-directory (my/meqo-server-directory))
         (filter (read-from-minibuffer "Filter: ")))
    (compile
     (format "cd %s && direnv exec %s ./js/scripts/runTests %s" default-directory default-directory filter)
     'meqo-ts-test-compilation-mode)))

(defun my/org-roam-commit-on-save ()
  "Auto commits in org-roam"
  (when (and (project-current)
             (string-match "/org-roam/" (project-root (project-current))))
    (let ((name "org-roam-auto-commit"))
      (call-process "git" nil nil nil "add" ".")
      (call-process "git" nil nil nil "commit" (format "--message=%s" name)))))

(add-hook 'after-save-hook 'my/org-roam-commit-on-save)

;; synchronize every 2 hour in the background. Show results if something failed
(run-at-time
 "5 minutes"
 (* 2 60 60)
 #'async-start
 `(lambda ()
    (with-timeout ((* 60 30) "timed out")
      ,(async-inject-variables (rx "load-path"))
      (require 'org-caldav)
      (defun org-caldav-timestamp-has-time-p (timestamp)
        "Checks whether a timestamp has a time.
        Returns nil if not and (sec min hour) if it has."
        (let ((ti (org-parse-time-string timestamp)))
          (or (nth 0 ti) (nth 1 ti) (nth 2 ti))))
      (defun org-caldav-skip-function (backend)
        (org-caldav-debug-print 2 "Skipping over excluded entries")
        (when (eq backend 'icalendar)
          (org-map-entries
           (lambda ()
             (let ((pt (save-excursion (apply 'org-agenda-skip-entry-if org-caldav-skip-conditions)))
                   (ts (when org-caldav-days-in-past (* (abs org-caldav-days-in-past) -1)))
                   (stamp (or (org-entry-get nil "TIMESTAMP" t) (org-entry-get nil "CLOSED" t))))
               (when (or pt (and stamp ts (> ts (org-time-stamp-to-now stamp))))
                 (delete-region (point) (progn (org-next-visible-heading 1) (point)))
                 (setq org-map-continue-from (point)))))))
        (org-caldav-debug-print 2 "Finished skipping"))
      ,(async-inject-variables
        (rx
         (or
          "org-export-with-broken-links"
          "org-caldav-url"
          "org-caldav-inbox"
          "org-caldav-calendar-id"
          "org-caldav-resume-aborted"
          "org-caldav-delete-calendar-entries"
          "org-caldav-skip-conditions"
          "org-caldav-files")))
      ;; Make sure url is reachable
      (if (not (= (call-process "curl" nil nil nil "--connect-timeout" "10" org-caldav-url) 0))
          "can't connect to url"
        ;; Make sure calendar.org does exist
        (unless (file-exists-p org-caldav-inbox)
          (make-empty-file org-caldav-inbox))
        ;; Make sure we don't get stuck in a yes no prompt
        (setq org-caldav-delete-org-entries 'always
              org-caldav-delete-calendar-entries 'always)
        (org-caldav-sync)
        (org-caldav-sync-result-filter-errors))))
 '(lambda (errors)
    (if errors
        (let ((error-msg (format "Syncing calendar failed: %s" errors)))
          (my/org-insert-auto-schedule error-msg)
          (message error-msg))
      (my/org-set-auto-schedule-done-rx "Syncing calendar failed: ")
      (message "calendar synced"))))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  :hook
  (nix-mode . eglot-ensure)
  (haskell-mode . eglot-ensure))


(defun joaot/eglot-project-find-function (dir)
  (when eglot-lsp-context
    (let* ((marker nil)
           ;; We use "dominating files" here, but we could use some
           ;; other criteria, like an Elisp data structure, or hack in
           ;; dir-locals for DIR (maybe with
           ;; `hack-dir-local-variables-non-file-buffer').
           (root (locate-dominating-file
                  dir
                  (lambda (d)
                    ;; Here, we search for these markers regardless of
                    ;; other state such current major mode.  We could
                    ;; fix that easily, but that's out of scope for this small example.
                    (let ((files (directory-files d nil (rx (or "pyrightconfig.json" "pyproject.toml" (sequence (one-or-more any) ".cabal"))))))
                             (if files
                                (progn (setq marker (car files))
                                       t)
                                nil))))))
      (message root)
      (message marker)
      (when root
        ;; Use the experimental eglot--project project type in latest
        ;; Eglot which has some space for user-defined props,
        ;; `:marker' in this case.  One could make use of it, say, in a
        ;; functional entry to `eglot-server-programs', to decide
        ;; which server to run based on the marker or the type of
        ;; the project.
        `(eglot--project ,root :marker ,marker)))))

(add-to-list 'project-find-functions
             #'joaot/eglot-project-find-function)

(add-hook 'before-save-hook 'eglot-format-buffer)

(defun my/save-no-hooks ()
  "Save buffer without any hooks. Sometimes formating does the wrong thing"
  (interactive)
  (let ((before-save-hook nil)
        (after-save-hook nil))
  (save-buffer)))

(defun my/get-run-for-github-url (txt)
  "Gets the run for the github url"
  (string-match (rx (
                     seq "https://github.com/"
                     (group (seq (one-or-more (not "/")) "/" (one-or-more (not "/"))))
                     "/actions/runs/"
                     (group (one-or-more num))))
                txt)
  `((:project . ,(match-string 1 txt))
    (:id . ,(match-string 2 txt))))

(defun my/open--github-action-logs-in-buffer (txt)
  "Opens the first github action link in buffer if it exists."
  (if-let* ((run (my/get-run-for-github-url txt))
           (project (alist-get :project run))
           (id (alist-get :id run)))
      (shell-command (concat "gh run -R " project " view " id " --log-failed")
                     (concat project " github action run: " id))))

(defun my/open-github-action-logs-in-buffer ()
  "Opens the first github action link in buffer if it exists."
  (interactive)
  (let ((txt (buffer-substring-no-properties 1 (buffer-end 1))))
    (my/open--github-action-logs-in-buffer txt)))

(defun my/open--github-action-rerun (txt)
  "Rerun the first github action link in buffer if it exists."
  (if-let* ((run (my/get-run-for-github-url txt))
           (project (alist-get :project run))
           (id (alist-get :id run)))
      (shell-command (concat "gh run -R " project " rerun " id " --failed")
                     (concat project " github action run: " id))))

(defun my/open-github-action-rerun ()
  "Rerun the first github action link in buffer if it exists."
  (interactive)
  (let ((txt (buffer-substring-no-properties 1 (buffer-end 1))))
    (my/open--github-action-rerun txt)))


(defun my/github-pr-runs ()
  "get all runs for the current pull request"
  (if-let* ((txt (buffer-substring-no-properties 1 (buffer-end 1)))
            (_ (string-match (rx (
                                  seq "https://github.com/"
                                  (group (seq (one-or-more (not "/")) "/" (one-or-more (not "/"))))
                                  "/pull/"
                                  (group (one-or-more num))))
                             txt))
            (project (match-string 1 txt))
            (id (match-string 2 txt))
            (matches (s-split "\n" (shell-command-to-string (concat "gh pr -R " project " checks " id)))))
      (completing-read "run: " matches)))

(defun my/select-github-action-run-and-open-in-buffer ()
  "Select run from runs of pr and open the log in buffer"
  (interactive)
  (my/open--github-action-logs-in-buffer (my/github-pr-runs)))

(defun my/select-github-action-run-and-rerun ()
  "Select run from runs of pr and rerun it"
  (interactive)
  (my/open--github-action-rerun (my/github-pr-runs)))

;; functions for org links into documnets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fd-dired
  :ensure t)

(defun my/find-in-scans (what)
  (interactive)
  "find regex in scan"
  (fd-dired "~/Dokumente/scans/" what))

(defun my/grep-in-scans (what)
  (interactive)
  "grep regex in scan"
  (fd-grep-dired "~/Dokumente/scans/" what))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elpher)

(setq yaml-indent-offset 2)
