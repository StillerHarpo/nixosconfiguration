(package-initialize)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)           ; Disable menubar
(setq visible-bell -1)

(require 'use-package)

(use-package general :ensure)

(use-package evil
 :ensure
 :init
 (setq evil-want-keybinding nil)
 (setq evil-undo-system 'undo-redo)
 :config
 (evil-mode t))

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

(use-package consult :ensure)

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
  (org-read-date-force-compatible-dates nil)
  (org-modules '(ol-bibtex org-habit))
  :config
  (unbind-key "SPC" org-agenda-mode-map)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook #'auto-revert-mode)
  (add-hook 'org-mode-hook #'(lambda () (ispell-change-dictionary "de_DE")))
  :general
  (:states '(normal visual)
   :prefix "SPC"
   "n a" 'org-agenda)
  )

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :ensure
  :custom
  (org-roam-directory (file-truename "~/Dokumente/org-roam"))
  :config
  (org-roam-db-autosync-mode)
  (advice-add 'org-roam-refile :after 'org-save-all-org-buffers)
  (defun my/org-roam-filter-by-regex (regex)
    (lambda (file)
      (string-match regex (org-file-contents file))))
  
  (defun my/org-roam-list-notes-by-regex (regex)
    (seq-filter
     (my/org-roam-filter-by-regex regex)
     (mapcar #'org-roam-node-file (org-roam-node-list))))
  (defun my/org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (delete-dups (my/org-roam-list-notes-by-regex "<[0-9]\\{4\\}\\-[0-9]\\{2\\}\\-[0-9]\\{2\\}\\|<%%(diary-float"))))
  (my/org-roam-refresh-agenda-list)
  :general
  (:states '(normal visual)
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
  "c" 'vterm-clear)
  :general
  (:states '(normal visual)
  :prefix "SPC"
  "o t" 'vterm))

(use-package link-hint
  :ensure
  :general
  (:states '(normal visual)
  :prefix "SPC"
  "s l" 'link-hint-open-link))

(use-package magit
  :ensure
  :general
  (:states '(normal visual)
   :keymaps 'magit-status-mode-map
   "S-SPC" 'magit-diff-show-or-scroll-up)
  :general
  (:states '(normal visual)
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

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package haskell-mode :ensure)

(use-package nix-mode :ensure)

(use-package tide
  :ensure t
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package notmuch
  :ensure t
  :init
  (setq notmuch-search-oldest-first 'f)
  (setq message-default-mail-headers "Cc: \nBcc: \n")
  ;; postponed message is put in the following draft directory
  (setq message-auto-save-directory "~/Maildir/draft")
  ;; change the directory to store the sent mail
  (setq message-directory "~/Maildir/")
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox and not tag:trash and date:3M..today" :key "i")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "must read" :query "not tag:rechnung and not tag:agb and not tag:anmeldung and not tag:trash and date:3M..today" :key "r")
          (:name "sent" :query "tag:sent" :key "s")
          (:name "drafts" :query "tag:draft" :key "d")
          (:name "all mail" :query "date:3M..today" :key "a")))
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
  :general
  (:states '(normal visual)
   :prefix "SPC"
   "o m" '(my/notmuch-search :which-key "notmuch"))
  (:states '(normal visual)
   :prefix "SPC"
   :keymaps '(notmuch-search-mode-map)
   "m u" '(my/notmuch-update :which-key "update mails"))
  (:states '(normal visual)
   :prefix "SPC"
   :keymaps '(notmuch-message-mode-map)
   "m c" '(notmuch-mua-send-and-exit :which-key "send mail" )
   "m a" '(mml-attach-file :which-key "attach file")
   "m d" '(notmuch-draft-save :which-key "save as draft"))
  (:states '(normal visual)
   :keymaps '(notmuch-message-mode-map)
   "d" '(notmuch-search-add-tag "+trash")
   "t" 'notmuch-search-add-tag)
  :config
  (setq notmuch-fcc-dirs "Sent -unread +sent")
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   ))

(winner-mode 1)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

;; don't override spc. It should always be the leader key
(general-unbind
  :states '(normal visual)
  :keymaps '(dired-mode-map Info-mode-map view-mode-map)
  "SPC")

(general-define-key :states '(normal visual)
  "g s" 'evil-avy-goto-char)

(general-define-key :states '(normal visual)
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
 "f" '(:ignore t :which-key "File")
 "f f" 'find-file
 "f s" 'save-buffer
 "b" '(:ignore t :which-key "Buffer")
 "b b" 'consult-buffer
 "b r" 'revert-buffer
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
 "s s" 'consult-line
 "SPC" 'project-find-file
 "p" '(:ignore t :which-key "project")
 "p p" 'project-switch-project
 "p s" 'consult-ripgrep
 "s p" 'consult-ripgrep
 "." '(find-file :which-key "Find file" )
 "q" '(:ignore t :which-key "quit")
 "q q" 'save-buffers-kill-terminal)

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 :keymaps 'emacs-lisp-mode-map
  "m e e" 'eval-last-sexp)

(setq ytel-invidious-api-url "https://yewtu.be")

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
