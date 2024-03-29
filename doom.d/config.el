;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Florian Engel")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 18 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "monospace" :size 20))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(defun youtube-feed ()
  (interactive)
    (let ((link (read-string "Link: ")))
      (insert (shell-command-to-string (concat "~/scripts/youtubeFeed " link)))))

;; Browser function
(setq mediareg (rx (or "youtube."
                       "youtu.be"
                       (and ".mp3" eol)
                       (and ".mp4" eol)
                       (and ".m4v$" eol)
                       "v.redd.it")))

(setq browse-url-handlers `((,mediareg . browse-url-mpv)
                            ("." . eww-browse-url)))

(defun browse-url-firefox-new-window (url &rest agrs)
  (browse-url-firefox url t))

(setq browse-url-secondary-browser-function 'browse-url-firefox-new-window)

;; biblioraphy
(setq reftex-default-bibliography '("~/Dokumente/bibliography/references.bib"))

(setq rmh-elfeed-org-files (list "~/Dokumente/elfeed.org"))

(setq elfeed-search-filter "@6-days-ago +unread +favorite")

;; auto completion for agda
(set-company-backend! 'agda2-mode 'company-capf)

;; Sign messages by default.
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
(setq mml-secure-openpgp-sign-with-sender t)

(map! :leader :desc "eshell" :n "o s" #'eshell
              :desc "woman" :n "o w" #'woman
              :desc "elfeed" :n "o e" #'elfeed)

(map! :mode 'elfeed-search-mode :n "b" #'elfeed-search-browse-url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail configuration

(map! :mode 'notmuch-search-mode :n "RET" #'notmuch-search-show-thread
      :n "d" (cmd! (notmuch-search-add-tag "+trash"))
      :n "t" #'notmuch-search-add-tag)
(map! :leader :mode 'notmuch-message-mode :localleader :desc "send mail" "c" #'notmuch-mua-send-and-exit
      :localleader :desc "attach file" "a" #'mml-attach-file
      :localleader :desc "save as draft" "d" #'notmuch-draft-save)
(setq +notmuch-mail-folder "~/Maildir")
(setq +notmuch-sync-backend 'mbsync)
(setq +notmuch-search-oldest-first 'f)
;; save send mails
(after! notmuch (setq notmuch-fcc-dirs "Sent -unread +sent"))
(setq mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)
(setq notmuch-saved-searches
      '((:name "inbox" :query "tag:inbox and not tag:trash and date:3M..today" :key "i")
        (:name "unread" :query "tag:unread" :key "u")
        (:name "flagged" :query "tag:flagged" :key "f")
        (:name "must read" :query "not tag:rechnung and not tag:agb and not tag:anmeldung and not tag:trash and date:3M..today" :key "r")
        (:name "sent" :query "tag:sent" :key "s")
        (:name "drafts" :query "tag:draft" :key "d")
        (:name "all mail" :query "date:3M..today" :key "a")))
(setq message-default-mail-headers "Cc: \nBcc: \n")
;; postponed message is put in the following draft directory
(setq message-auto-save-directory "~/Maildir/draft")
;; change the directory to store the sent mail
(setq message-directory "~/Maildir/")
(setq +notmuch-home-function (lambda () (notmuch-search "tag:inbox and date:3M..today")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq lsp-enable-file-watchers t
      lsp-file-watch-threshold 16384)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org config
(setq org-src-window-setup 'current-window)
(setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
;; NO spell check for embedded snippets
(defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
  (let* ((rlt ad-return-value)
         (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|example\\|quote\\)")
         (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|example\\|quote\\)")
         (case-fold-search t)
         b e)
    (when ad-return-value
      (save-excursion
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t))))
      (if (and b e (< (point) e)) (setq rlt nil)))
    (setq ad-return-value rlt)))
(setq org-highlight-latex-and-related '(latex script entities))
(setq org-ref-bibliography-notes "~/Dokumente/bibliography/notes.org"
      org-ref-default-bibliography reftex-default-bibliography
      org-ref-pdf-directory "~/Dokumente/bibliography/bibtex-pdfs/")

(setq bibtex-completion-bibliography reftex-default-bibliography
      bibtex-completion-library-path org-ref-bibliography-notes
      bibtex-completion-notes-path org-ref-pdf-directory)

;; automatic saves of org buffers
(advice-add 'org-refile :after 'org-save-all-org-buffers)
(advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)

(setq org-read-date-force-compatible-dates nil)
(setq org-modules '(ol-bibtex org-habit))
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org roam config
(setq org-roam-directory (file-truename "~/Dokumente/org-roam"))
(setq org-roam-file-exclude-regexp ".stversions")
(setq org-capture-templates
      '(("d" "default" entry
         (file+headline +org-capture-todo-file "Inbox")
         "* [ ] %?\n%A\n" :prepend t)))
(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target
        (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}")
        :unnarrowed t)
        ("l" "location" plain "* %A %?" :target
        (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}")
        :unnarrowed t)))
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %U %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: ~ %<%Y-%m-%d>\n"))
        ("t" "traueme" plain "#+zeit: %^{von}-%^{bis}\n* Traum 1\n%?" :target
         (file+head "%<%Y-%m-%d>-Traueme.org" "#+title: ~ Träume vom %<%d.%m.%Y>\n")
        :unnarrowed t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(setq org-log-into-drawer "LOGBOOK")

(setq langtool-mother-tongue "de-DE")

(general-evil-define-key '(normal visual) 'vterm-mode-map
          "p" 'vterm-yank
          :prefix ","
          "e" 'vterm-send-next-key
          "c" 'vterm-clear)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only consider files with active date for agenda
;; This speeds up the agenda
(defun my/org-roam-filter-by-regex (regex)
  (lambda (file)
    (string-match regex (org-file-contents file))))

(defun my/org-roam-list-notes-by-regex (regex)
  (seq-filter
   (my/org-roam-filter-by-regex regex)
   (mapcar #'org-roam-node-file (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (delete-dups (my/org-roam-list-notes-by-regex "<[0-9]\\{4\\}\\-[0-9]\\{2\\}\\-[0-9]\\{2\\}"))))

(after! org-roam (my/org-roam-refresh-agenda-list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun with-nix-pathes (nix-path-alist)
    "config that need nix-paths is called with this functions"
  (let ((shell (cdr (assoc 'nix-zsh-path nix-path-alist)))
         (latexmk (cdr (assoc 'nix-latexmk-path nix-path-alist)))
         (mpv (cdr (assoc 'nix-mpv-path nix-path-alist)))
         (jdk (cdr (assoc 'nix-jdk-path nix-path-alist)))
         (languagetool (cdr (assoc 'nix-languagetool-path nix-path-alist))))
     (defun browse-url-mpv (url &rest args) (browse `(,mpv "--save-position-on-quit") url))
     (setq vterm-shell shell)
     (setq org-latex-pdf-process (list (concat latexmk " -shell-escape -bibtex -f -pdfxe %f")))
     (setq langtool-java-classpath (concat languagetool "/share/")
           langtool-java-user-arguments `("-Dfile.encoding=UTF-8"
                                          "-cp" ,(concat languagetool "/share/"))
           langtool-java-bin jdk
           langtool-language-tool-jar (concat languagetool "/share/languagetool-commandline.jar")
           langtool-language-tool-server-jar (concat languagetool "/share/languagetool-server.jar"))))
