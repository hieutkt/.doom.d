;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Hieu Phay"
      user-mail-address "hieunguyen31371@gmail.com"
      default-input-method 'vietnamese-telex
      +doom-dashboard-banner-dir doom-private-dir
      +doom-dashboard-banner-file "favicon-pixel.png"
      +doom-dashboard-banner-padding '(0 . 2))

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
(setq doom-font "Iosevka 12"
      doom-variable-pitch-font "Iosevka Aile 12")

(when (member "Source Han Sans" (font-family-list))
  (set-fontset-font t 'han (font-spec :name "Source Han Sans")))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox
      doom-themes-treemacs-theme 'doom-colors)
(custom-set-faces!
  `(outline-3 :foreground ,(doom-color 'blue)))

;; Start Doom fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/Notes")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


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
(use-package! doom-modeline
  :config
  (setq doom-modeline-persp-name t))

(use-package! hl-todo
  :hook (text-mode . hl-todo-mode))

(use-package! evil-escape
  :config
  (setq evil-esc-delay 0.25))

(use-package! evil-vimish-fold
  :config
  (global-evil-vimish-fold-mode))

(use-package! evil-goggles
  :init
  (setq evil-goggles-enable-delete t
        evil-goggles-enable-change t
        evil-goggles-duration      0.25)
  :config
  (evil-goggles-use-magit-faces)
  (custom-set-faces!
    `(evil-goggles-yank-face :inherit magit-diff-added-highlight)
    `(evil-goggles-change-face :inherit magit-diff-base)
    `(evil-goggles-surround-face :inherit diff-refine-added)
    `(evil-goggles-indent-face :inherit diff-refine-changed)))

(map! :map (term-mode vterm-mode)
      "C-c C-z" 'other-window)

(use-package! org
  :init
  :config
  ;; ORG LATEX PREVIEW
  (setq org-startup-with-latex-preview t
        ;; Make latex preview with "C-c C-x C-l" slightly bigger
        org-format-latex-options
        (plist-put org-format-latex-options :scale 1.8)
        ;; Cache the preview images elsewhere
        org-preview-latex-image-directory "~/.cache/ltximg/"
        org-ellipsis " ⤵"))

(use-package! org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("⁖")))

(use-package! org-tempo)

(use-package! ox-latex
  :config
  ;; Highlight code blocks in org-latex-export-to-pdf
  ;; Minted options can be found in:
  ;; http://mirror.kku.ac.th/CTAN/macros/latex/contrib/minted/minted.pdf
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-minted-options '(("fontsize" "\\footnotesize")
                                   ("bgcolor" "yellow!5"))
        org-latex-pdf-process
        '("latexmk -pdflatex='%latex -shell-escape -bibtex -interaction=nonstopmode' -pdf -output-directory=%o -f %f"))

  ;; Default packages
  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t ("pdflatex"))
          ("T1" "fontenc" t ("pdflatex"))
          ("" "fontspec" t ("xelatex"))
          ("" "graphicx" t)
          ("" "grffile" t)
          ;; Array, tabularx, booktabs are for tables
          ("" "array" nil)
          ("" "tabularx" nil)
          ("" "booktabs" nil)
          ("" "multirow" nil)
          ("" "siunitx" nil)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "amsmath" t)
          ("" "mathrsfs" t)
          ("" "textcomp" t)
          ("" "amssymb" t)
          ("" "capt-of" nil)
          ("dvipsnames" "xcolor" nil)
          ("colorlinks=true, linkcolor=Blue, citecolor=BrickRed, urlcolor=PineGreen" "hyperref" nil)
          ("" "indentfirst" nil)))

  ;; Add KOMA-scripts classes to org export
  (add-to-list 'org-latex-classes
               '("koma-article" "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("koma-report" "\\documentclass{scrreprt}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (add-to-list 'org-latex-classes
               '("koma-book" "\\documentclass[11pt]{scrbook}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(use-package! ox-hugo
  :config
  (setq org-hugo-use-code-for-kbd t))

(use-package! org-agenda
  :config
  ;; Agenda folder - .org files are found recursively
  (setq org-agenda-files '("~/Dropbox/Notes/Agenda"))
  ;; Also to make refiling easier
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  ;; Setting the TODO keywords
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"                    ;What needs to be done
           "NEXT(n)"                    ;A project without NEXTs is stuck
           "|"
           "DONE(d)")
          (sequence
           "HOLD(h)"                    ;Task is on hold because of me
           "PROJ(p)"                    ;Contains sub-tasks
           "WAIT(w)"                    ;Tasks delegated to others
           "|"
           "STOP(c)"                    ;Stopped/cancelled
           "MEETING(m)"                 ;Meetings
           ))
        org-todo-keyword-faces
        '(("NEXT" . +org-todo-active)
          ("WAIT" . +org-todo-active)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)))
  ;; Tags triggers
  ;; (setq org-todo-state-tags-triggers)
  ;;
  ;; Capture templates
  (setq org-capture-templates
        '(("i" "Inbox" entry (file "~/Dropbox/Notes/Agenda/inbox.org")
           "* TODO %?\n  %i\n")
          ("m" "Meeting" entry (file "~/Dropbox/Notes/Agenda/inbox.org")
           "* MEETING with %? :meeting:\n%t" :clock-in t :clock-resume t)))
  ;; Clocking
  (setq org-clock-persist 'history
        org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  (org-clock-persistence-insinuate)
  )

(use-package! org-habit
  :config
  (setq org-habit-show-all-today t))

(use-package! org-super-agenda
  :after org-agenda
  :config
  ;; Enable org-super-agenda
  (org-super-agenda-mode)
  ;; Customise the agenda view
  (setq org-super-agenda-groups
        '((:auto-outline-path t :time-grid t)))
  ;; Make evil keymaps works on org-super-agenda headers
  (after! evil-org-agenda
    (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map))))


(use-package! org-ref
  :init
  (map! :leader
        :desc "helm-bibtex" "]" #'helm-bibtex)
  (map! :map helm-map
        :g "C-j" #'helm-next-line
        :g "C-k" #'helm-previous-line)
  :config
  (setq
   org-ref-default-bibliography     '("~/Dropbox/Notes/Research/papers.bib")
   org-ref-pdf-directory             "~/Dropbox/Notes/Papers/"
   bibtex-dialect                    'biblatex
   bibtex-completion-notes-extension "_notes.org"
   bibtex-completion-notes-path      "~/Dropbox/Notes/Org-roam/"
   bibtex-completion-bibliography    "~/Dropbox/Notes/Research/papers.bib"
   bibtex-completion-library-path    "~/Dropbox/Notes/Papers/"
   ;; Optimize for 80 character frame display
   bibtex-completion-display-formats
   '((t . "${title:46} ${author:20} ${year:4} ${=type=:3}${=has-pdf=:1}${=has-note=:1}"))
   bibtex-completion-notes-template-multiple-files
   "${author-or-editor} (${year}): ${title}\n#+roam_tags: literature\n#+roam_key: cite:${=key=}i\n#+created: %U\n#+last_modified: %U\n\n"
   ;; Open pdf in external tool instead of in Emacs
   bibtex-completion-pdf-open-function
   (lambda (fpath)
     (call-process "evince" nil 0 nil fpath)))
  ;; Make org-ref-cite-face a bit less intrusive
  (custom-set-faces!
    `(org-ref-cite-face :weight unspecified :foreground unspecified
                        :underline ,(doom-color 'grey))))

(use-package! citeproc-org
  :after org-ref
  :config
  (citeproc-org-setup))


(use-package! org-roam
  :hook
  (after-init . org-roam-mode)
  :init
  (setq org-roam-directory "~/Dropbox/Notes/Org-roam/"
        +org-roam-open-buffer-on-find-file nil)
  ;; Make org-roam faces less intrusive
  (custom-set-faces!
    `((org-roam-link org-roam-link-current)
      :inherit unspecified :underline ,(doom-color 'grey))))

(use-package! org-roam-db
  :config
  (setq org-roam-db-location "~/.emacs.d/org-roam.db"))

(use-package! org-roam-graph
  :init
  (setq org-roam-graph-executable           (executable-find "dot")
        org-roam-graph-extra-config        '(("overlap" . "false")
                                             ("concentrate" . "true")
                                             ("bgcolor" . "lightblue"))
        org-roam-graph-edge-cites-extra-config
        '(("color" . "gray")
          ("style" . "dashed")
          ("sep" . "20"))
        org-roam-graph-shorten-titles      'wrap
        org-roam-graph-max-title-length    50
        org-roam-graph-exclude-matcher     '("journal")))

(use-package! org-roam-capture
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           ""
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+roam_tags: %?\n#+roam_alias: \n#+created: %U\n#+last_modified: %U\n\n"
           :unnarrowed t))
        org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "#+roam_key: ${ref}\n%?"
           :file-name "%<%Y%m%d%H%M%S>_web_${slug}"
           :head "#+title: ${title}\n#+roam_tags: website\n#+created: %U\n#+last_modified: %U\n\n"
           :unnarrowed t))
        org-roam-dailies-capture-templates
        '(("d" "daily" plain (function org-roam-capture--get-point)
           ""
           :immediate-finish t
           :file-name "journal_%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d %a>\n#+roam_tags: journal\n#+startup: content\n#+created: %U\n#+last_modified: %U\n\n")))
  ;; Update the `last-modified` field on save
  (defun zp/org-find-time-file-property (property &optional anywhere)
    "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
             (save-excursion
               (re-search-forward org-outline-regexp-bol nil t))))
        (when (re-search-forward (format "^#\\+%s:" property)
                                 (if anywhere nil first-heading)
                                 t)
          (point)))))

  (defun zp/org-has-time-file-property-p (property &optional anywhere)
    "Return the position of time file PROPERTY if it is defined.
As a special case, return -1 if the time file PROPERTY exists but
is not defined."
    (when-let ((pos (zp/org-find-time-file-property property anywhere)))
      (save-excursion
        (goto-char pos)
        (if (and (looking-at-p " ")
                 (progn (forward-char)
                        (org-at-timestamp-p 'lax)))
            pos -1))))

  (defun zp/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                        (zp/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

  (defun zp/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (zp/org-set-time-file-property "last_modified")))
  :hook (before-save . zp/org-set-last-modified))

(use-package! org-roam-dailies
  :config
  (map! :leader
        :prefix "n"
        (:prefix ("j" . "journal")
          :desc "Arbitrary date" "d" #'org-roam-dailies-date
          :desc "Today"          "j" #'org-roam-dailies-today
          :desc "Tomorrow"       "m" #'org-roam-dailies-tomorrow
          :desc "Yesterday"      "y" #'org-roam-dailies-yesterday)))

(use-package! org-roam-server
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))


(use-package! visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode)
  :init
  (setq visual-fill-column-width 85))

(use-package! ess
  :config
  (set-popup-rules!
    '(("^\\*R:*\\*$" :side right :size 0.5 :ttl nil))))

(use-package! julia-repl
  :config
  ;; Make popup position similar to `ess'
  (set-popup-rules!
    '(("^\\*julia.*\\*$" :side right :size 0.5 :ttl nil))))

(use-package! lexic
  :commands lexic-search lexic-list-dictionary
  :init
  (defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
    "Look up the definition of the word at point (or selection) using `lexic-search'."
    :override #'+lookup/dictionary-definition
    (interactive
     (list (or (doom-thing-at-point-or-region 'word)
               (read-string "Look up in dictionary: "))
           current-prefix-arg))
    (lexic-search identifier nil nil t))
  :config
  (set-popup-rules!
    '(("^\\*lexic\\*$" :size 0.4)))
  (map! :map lexic-mode-map
        :n "q" #'lexic-return-from-lexic
        :nv "RET" #'lexic-search-word-at-point
        :n "a" #'outline-show-all
        :n "h" (cmd! (outline-hide-sublevels 3))
        :n "o" #'lexic-toggle-entry
        :n "n" #'lexic-next-entry
        :n "N" (cmd! (lexic-next-entry t))
        :n "p" #'lexic-previous-entry
        :n "P" (cmd! (lexic-previous-entry t))
        :n "b" #'lexic-search-history-backwards
        :n "f" #'lexic-search-history-forwards
        :n "/" (cmd! (call-interactively #'lexic-search))))

(use-package! lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))
