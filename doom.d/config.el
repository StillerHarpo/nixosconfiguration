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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

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
(setq +notmuch-mail-folder "~/Maildir")
(setq +notmuch-sync-backend 'mbsync)
(setq +notmuch-search-oldest-first 'f)

(setq org-src-window-setup 'current-window)
(setq org-agenda-files (list "~/Dokumente" "~/org"))
(setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))

(defun current-dreams ()
  (interactive)
  (find-file
   (format-time-string
    "~/Dokumente/Traeume/%Y/%B/Traeume_Vom_%d_%m_%Y.org"))
  (auto-fill-mode))
(map! :leader :desc "Write current dream" :n "o t" #'current-dreams)

(defun youtube-feed ()
  (interactive)
    (let ((link (read-string "Link: ")))
      (insert (shell-command-to-string (concat "~/scripts/youtubeFeed " link)))))

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

;; Browser function
(setq mediareg (rx (or "youtube."
                       "youtu.be"
                       (and ".mp3" eol)
                       (and ".mp4" eol)
                       (and ".m4v$" eol)
                       "v.redd.it")))

  (setq org-highlight-latex-and-related '(latex script entities))

  (setq browse-url-browser-function `((,mediareg . browse-url-mpv)
                                        ;                                   ("." . eww-browse-url)))
                                      ("." . browse-url-firefox-new-window)))

  (defun browse (prog url)
    (setq url (browse-url-encode-url url))
    (start-process (concat prog url) nil prog url))
  (defun browse-url-mpv (url &rest args) (browse "mpv" url))
  (defun browse-url-firefox-new-window (url &rest agrs)
    (browse-url-firefox url t))

;; biblioraphy
(setq reftex-default-bibliography '("~/Dokumente/bibliography/references.bib"))

(setq org-ref-bibliography-notes "~/Dokumente/bibliography/notes.org"
      org-ref-default-bibliography reftex-default-bibliography
      org-ref-pdf-directory "~/Dokumente/bibliography/bibtex-pdfs/")

(setq bibtex-completion-bibliography reftex-default-bibliography
      bibtex-completion-library-path org-ref-bibliography-notes
      bibtex-completion-notes-path org-ref-pdf-directory)

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdfxe %f"))
(setq rmh-elfeed-org-files (list "~/dotfiles/elfeed.org"))
(after! elfeed
  (setq elfeed-search-filter "@6-days-ago +unread +favorite"))

;; auto completion for agda
(set-company-backend! 'agda2-mode 'company-capf)

;; Sign messages by default.
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
(setq mml-secure-openpgp-sign-with-sender t)

(map! :leader :desc "eshell" :n "o s" #'eshell
              :desc "woman" :n "o w" #'woman)

(map! :mode 'notmuch-search-mode :n "RET" #'notmuch-search-show-thread
                                 :n "d" (cmd! (notmuch-search-add-tag "+trash"))
                                 :n "t" #'notmuch-search-add-tag)

(map! :leader :mode 'notmuch-message-mode :localleader :desc "send mail" "c" #'notmuch-mua-send-and-exit
                                          :localleader :desc "attach file" "a" #'mml-attach-file
                                          :localleader :desc "save as draf" "d" #'notmuch-draft-save)


(after! notmuch
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox not tag:trash" :key "i")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "must read" :query "not tag:rechnung not tag:agb not tag:anmeldung not tag:trash" :key "r")
          (:name "sent" :query "tag:sent" :key "s")
          (:name "drafts" :query "tag:draft" :key "d")
          (:name "all mail" :query "*" :key "a")))

  (setq message-default-mail-headers "Cc: \nBcc: \n")
  ;; postponed message is put in the following draft directory
  (setq message-auto-save-directory "~/Maildir/draft")
  ;; change the directory to store the sent mail
  (setq message-directory "~/Maildir/"))

(setq lsp-enable-file-watchers t
      lsp-file-watch-threshold 16384)
