(package-initialize)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)           ; Disable menubar
(setq ring-bell-function 'ignore)

(require 'use-package)

(use-package my-nix-paths
  :config
  (defun browse-url-mpv (url &rest args) (browse `(,nix-mpv-path "--save-position-on-quit") url))
  (defun browse-url-linkopenwithx (url &rest args) (browse `(,nix-linkopenwithx-path) url))
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

(use-package vimish-fold
  :ensure
  :after evil)

(use-package evil-vimish-fold
  :ensure
  :after vimish-fold
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

(use-package evil-collection
  :ensure
  :init
  (evil-collection-init))

(use-package evil-embrace
  :ensure
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

(use-package company
  :config
  (global-company-mode))

(use-package tempel
  :general
  (:states '(normal visual motion)
   :prefix "SPC"
   "c i" 'tempel-insert))

(use-package tempel-collection
  :ensure t
  :after tempel)

(use-package consult
  :ensure t
  :custom (xref-show-xrefs-function consult-xref))

(use-package avy :ensure)

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


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
  :config
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook #'auto-revert-mode)
  (add-hook 'org-mode-hook #'(lambda () (ispell-change-dictionary "de_DE")))
  :general
  (:states '(normal visual motion)
   :prefix "SPC"
   "n a" 'org-agenda
   "n c" 'org-capture
   "n l" 'org-store-link)
  :general
  (:states '(normal visual motion)
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
  (:states '(normal visual)
   :keymaps '(org-agenda-mode-map)
   :prefix "SPC"
   ))

(use-package ol-notmuch
  :ensure t)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (general-unbind
    :states '(normal visual motion)
    :keymaps '(org-agenda-mode-map org-agenda-keymap)
    "SPC")
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "SPC") nil))
  )

(use-package org-roam
  :ensure
  :commands
  (org-roam-node-list org-roam-node-file org-roam-filter-by-regex)
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
    (setq org-agenda-files (delete-dups (cons "~/Dokumente/notes.org" (my/org-roam-list-notes-by-regex "<[0-9]\\{4\\}\\-[0-9]\\{2\\}\\-[0-9]\\{2\\}\\|<%%(diary-float")))))
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
  :config
  (org-roam-db-autosync-mode)
  (advice-add 'org-roam-refile :after 'org-save-all-org-buffers)
  :general
  (:states '(normal visual motion)
	   :prefix "SPC"
           "n" '(:ignore t :which-key "org")
	   "n f" 'org-roam-node-find
	   "n i" 'org-roam-node-insert))

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
  (:states '(normal visual motion)
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
  (:states '(normal visual motion)
  :prefix "SPC"
   "g" '(:ignore t :which-key "magit")
   "g g" 'magit-status
   "g c" 'magit-clone)
  :config
  (with-eval-after-load 'magit-log
    (define-key magit-log-mode-map (kbd "SPC") nil))
  (with-eval-after-load 'magit-status
    (define-key magit-status-mode-map (kbd "SPC") nil))
  (with-eval-after-load 'magit-revision
    (define-key magit-revision-mode-map (kbd "SPC") nil))
  )

(use-package git-timemachine
  :ensure t
  :after hydra
  :general
  (:states '(normal visual motion)
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
  :ensure t)

(use-package pr-review 
  :after magit
  :ensure t
  :config
  (defun my/pr-review-at-point ()
    "opens the pull request at point in pr-review"
    (interactive)
    (if-let ((url (forge-get-url (or (forge-post-at-point)
                                   (forge-current-topic)))))
        (pr-review url)
      (user-error "Nothing at point with a URL")))
  )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :general
  (:states '(normal visual motion)
  :prefix "SPC"
   "c x" 'flycheck-list-errors))

(use-package flycheck-eglot
  :ensure t
  :config
  (global-flycheck-eglot-mode 1))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package haskell-mode
  :ensure t
  :init
  (defun my/init-haskell ()
    "init haskell-mode with etags or eglot depending on the project"
    (interactive)
    (if (and (project-current) (string-match (rx (and "/checkpad/server/" line-end)) (project-root (project-current))))
	(progn
	  (custom-set-variables '(haskell-tags-on-save t))
          (set (make-local-variable 'company-backends)
               (append '(company-etags) company-backends))
          (general-define-key
	     :keymaps 'local
	     :states '(normal visual motion)
             "g d" 'haskell-mode-tag-find))
      (eglot-ensure)))
  :hook (haskell-mode . my/init-haskell))

(use-package nix-mode :ensure)

(use-package tide
  :ensure t
  :after (company flycheck envrc)
  :custom
  (tide-server-max-response-length 204800)
  (typescript-ts-mode-indent-offset 4)
  :init
  (defun my/eslint-format ()
    "runs eslint on the current file, falls back on tide-format"
    (interactive)
    (envrc-propagate-environment (lambda ()
      (when
      	(= (call-process "npx" nil nil nil "eslint" "--quiet" "--fix" (buffer-file-name (current-buffer))) 127)
        (message "command npx not found")
        (tide-format-before-save)))))
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . my/eslint-format))
  :config
  (evil-add-command-properties #'tide-goto-line-reference :jump t)
  (evil-add-command-properties #'tide-jump-to-definition :jump t)
  :general
  (:states '(normal visual motion)
   :keymaps '(tsx-ts-mode typescript-ts-mode)
  "g r" 'tide-references)
  :general
  (:states '(normal visual motion)
   :keymaps '(tsx-ts-mode typescript-ts-mode)
  :prefix "SPC"
  "c d" 'tide-jump-to-definition
  "c r" 'tide-references))

(use-package csv-mode
  :ensure t
  :hook (csv-mode . csv-guess-set-separator))

(use-package notmuch
  :ensure t
  :init
  (setq-default notmuch-search-oldest-first nil)
  :custom
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
    "Open notmuch in serach mode"
    (interactive)
     (notmuch-search "tag:inbox and date:3M..today"))
   ;;;###autoload
   (defun +notmuch/compose ()
     "Compose new mail"
     (interactive)
     (notmuch-mua-mail
      nil
      nil
      (list (cons 'From  (completing-read "From: " (notmuch-user-emails))))))
   (defun my/notmuch-trash ()
     "Add trash label to mail"
     (interactive)
     (notmuch-search-add-tag '("+trash")))
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
  :config
  (general-unbind
    :states '(normal visual motion)
    :keymaps '(elfeed-search-mode-map elfeed-show-mode-map)
    "SPC"))

(use-package elfeed-org
  :ensure t
  :custom
  (rmh-elfeed-org-files (list "~/Dokumente/elfeed.org"))
  :config
  (elfeed-org))

(use-package ement :ensure t)

(use-package ytel
  :ensure t
  :custom
  :config
  (setq invidious-instances-url "https://api.invidious.io/instances.json?pretty=1&sort_by=health")
  (defun ytel-instances-fetch-json ()
    "Fetch list of invidious instances as json, sorted by health."
    (let
        ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json"))))
      (with-current-buffer
          (url-retrieve-synchronously invidious-instances-url)
        (goto-char (point-min))
        (re-search-forward "^$")
        (let*
            ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'string))
          (json-read)))))

  (defun ytel-instances-alist-from-json ()
    "Make the json of invidious instances into an alist."
    (let
        ((jsonlist (ytel-instances-fetch-json))
         (inst ()))
      (while jsonlist
        (push (concat "https://" (caar jsonlist)) inst)
        (setq jsonlist (cdr jsonlist)))
      (nreverse inst)))

  (defun ytel-choose-instance ()
    "Prompt user to choose an invidious instance to use."
    (interactive)
    (setq ytel-invidious-api-url
          (or (condition-case nil
                  (completing-read "Using instance: "
                                    (ytel-instances-alist-from-json)
                                   nil "confirm" "https://")
                (error nil))
              "https://invidious.tube"))) ; fallback

  (defun ytel-watch ()
    "Stream video at point in mpv."
    (interactive)
    (let* ((video (ytel-get-current-video))
	   (id (ytel-video-id video)))
      (start-process "ytel mpv" nil
		     "mpv"
		     (concat "https://www.youtube.com/watch?v=" id))
      "--ytdl-format=bestvideo[height<=?720]+bestaudio/best")
    (message "Starting streaming..."))


  (setq evil-motion-state-modes (append '(ytel-mode) evil-motion-state-modes))
  (general-unbind
    :states '(normal visual motion)
    :keymaps 'ytel-mode-map
    "s" "<" ">" "q")
  :general
    (:states '(normal visual motion)
     :keymaps 'ytel-mode-map
      "o" 'ytel-watch))

(use-package peertube
  :ensure t
  :init
  (defun peertube-mpv-open-video ()
    "Open the video under the cursor using `browse-url'."
    (interactive)
    (let ((url (peertube-video-url (peertube--get-current-video))))
      (browse-url-mpv url)))
  :general
  (:states '(normal visual)
  :keymaps 'peertube-mode-map
  "enter" 'peertube-mpv-open-video
  "o" 'peertube-mpv-open-video
  "c" 'peertube-goto-channel
  "i" 'peertube-show-video-info
  "d" 'peertube-download-video
  "q" 'peertube-quit
  "s" 'peertube-search
  "m" 'peertube-change-sort-method
  "t" 'peertube-preview-thumbnail))

(recentf-mode 1)
(winner-mode 1)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

;; Browser function
(setq no-eww-reg (rx (or "youtube." "youtu.be" "v.reddit.it" "dailymotion."
                         "github.com" "streamable.com"  "liveleak.com"
                         "vimeo.com" "gfycat.com"
                         (and (or ".mp3" ".mp4" ".m4v" ".svg" ".gif" ".gifv")
                              line-end))))

(setq browse-url-handlers `((,no-eww-reg . browse-url-linkopenwithx)
                            ("." . eww-browse-url)))

(defun browse (prog url)
  (setq url (browse-url-encode-url url))
  (apply #'start-process (append `(,(concat (car prog) " " url) nil) prog `(,url))))
(defun browse-url-firefox-new-window (url &rest agrs)
  (browse-url-firefox url t))

(setq browse-url-secondary-browser-function 'browse-url-firefox-new-window)

;; don't override spc. It should always be the leader key
(general-unbind
  :states '(normal visual motion)
  :keymaps '(dired-mode-map Info-mode-map view-mode-map debugger-mode-map help-mode-map org-agenda-mode-map org-agenda-keymap)
  "SPC")

(general-define-key :states '(normal visual motion)
  "g s" 'evil-avy-goto-char)

(general-define-key :states '(normal visual)
  :keymaps '(Info-mode-map)
  "s" 'consult-info)

(defun my/delete-this-file ()
    (interactive)
    (delete-file (buffer-file-name))
    (kill-current-buffer))

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
    (move-file old-path new-path (or force-p 1))
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
   (consult-ripgrep "Find regexp" "r")
   (project-magit "git" "g")
   (project-eshell "Eshell")
   (project-vterm "terminal" "t")))


(defun my/consult-ripgrep ()
  (interactive)
  (if evil-visual-region-expanded
    (consult-ripgrep nil (regexp-quote (buffer-substring-no-properties (mark) (point))))
    (consult-ripgrep)))

(defun my/consult-line ()
  (interactive)
  (if evil-visual-region-expanded
    (let ((region (buffer-substring-no-properties (mark) (point))))
      (evil-force-normal-state)
      (consult-line (replace-regexp-in-string " " "\\\\s-" (regexp-quote region))))
    (consult-line)))

(general-define-key :states '(normal visual motion)
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
 "SPC" 'project-find-file
 "p" '(:ignore t :which-key "project")
 "p p" 'project-switch-project
 "p s" 'my/consult-ripgrep
 "s p" 'my/consult-ripgrep
 "." '(find-file :which-key "Find file" )
 "q" '(:ignore t :which-key "quit")
 "q q" 'save-buffers-kill-terminal
 "o" '(:ignore t :which-key "open")
 "o e" 'elfeed
 "o c" 'ement-connect
 "o t" 'vterm
 "o y" 'ytel
 "o p" 'peertube
 "o m" '(my/notmuch-search :which-key "notmuch"))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 :keymaps 'emacs-lisp-mode-map
  "m e e" 'eval-last-sexp)

;; Font size adjustment
(defun hoagie-adjust-font-size (frame)
  "Inspired by https://emacs.stackexchange.com/a/44930/17066. FRAME is ignored.
If I let Windows handle DPI everything looks blurry."
  (let* ((attrs (frame-monitor-attributes)) ;; gets attribs for current frame
         (width-px (-fourth-item (-second-item attrs)))
         (size 18))
    (when (eq width-px 2560) ;; laptop screen
      (setq size 20))
    (when (eq width-px 1920)  ;; External monitor at home
      (setq size 18))
    (set-frame-font (format "monospace %s" size))
    ))
(add-hook 'window-size-change-functions #'hoagie-adjust-font-size)

(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window))


(setq ediff-window-setup-function 'ediff-setup-windows-plain)
