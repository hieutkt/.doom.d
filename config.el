;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Hieu Phay"
      user-mail-address "hieunguyen31371@gmail.com"
      default-input-method 'vietnamese-telex)

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
(setq doom-theme 'doom-gruvbox)

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
(use-package! evil-escape
  :config
  (setq evil-esc-delay 0.25))

(use-package! evil-goggles
  :init
  (setq evil-goggles-enable-delete t
        evil-goggles-enable-change t)
  :config
  (evil-goggles-use-magit-faces)
  (custom-set-faces!
    `(evil-goggles-yank-face :inherit hl-line :weight bold)
    `(evil-goggles-change-face :inherit magit-diff-base)
    `(evil-goggles-surround-face :inherit diff-refine-added)
    `(evil-goggles-indent-face :inherit diff-refine-changed)))

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


(use-package! org-agenda
  :config
  (setq org-agenda-files '("~/Dropbox/Notes")))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups '((:name "Urgent tasks!!!"
                                   :tag "urgent")
                                  (:name "Important tasks:"
                                   :priority "A")
                                  (:name "Scheduled:"
                                   :time-grid t
                                   :scheduled today)
                                  (:name "Due:"
                                   :deadline today)
                                  (:name "Overdue:"
                                   :deadline past)
                                  (:name "Due soon:"
                                   :deadline future)))
  (after! org-agenda
    (org-super-agenda-mode))
  ;; Make evil keymaps works on org-super-agenda headers
  (after! evil-org-agenda
    (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map))))


(use-package! org-ref
  :init
  (map! :leader
        :desc "helm-bibtex" "]" #'helm-bibtex)
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
   "${author-or-editor} (${year}): ${title}\n#+roam_tags: bibliography\n#+roam_key: cite:${=key=}"
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
  (setq org-roam-directory "~/Dropbox/Notes/Org-roam/")
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
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+roam_tags:\n#+roam_alias:\n"
           :unnarrowed t))
        org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "#+roam_key: ${ref}\n%?"
           :file-name "%<%Y%m%d%H%M%S>_web_${slug}"
           :head "#+title: ${title}\n#+roam_tags: website\n"
           :unnarrowed t))
        org-roam-dailies-capture-templates
        '(("d" "daily" plain (function org-roam-capture--get-point)
           ""
           :immediate-finish t
           :file-name "journal_%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d %a>\n#+roam_tags: journal\n"))))

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


(use-package! org-journal
  :init
  (setq org-journal-date-format "%A, %Y-%m-%d"
        org-journal-date-prefix "* Daily Journal "
        org-extend-today-until 4
        org-journal-file-format "journal_%Y-%m-%d.org"
        org-journal-dir "~/Dropbox/Notes/Org-roam/"
        org-journal-file-header "#+title: %Y-%m-%d %a\n#+roam_tags: journal\n"
        org-journal-enable-agenda-integration t))

(use-package! org-fragtog
  :config (add-hook 'org-mode-hook 'org-fragtog-mode))


(use-package! visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode)
  :init
  (setq visual-fill-column-width 85))

(use-package! lsp-julia
  :config
  (setq lsp-julia-package-dir nil))

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
    '(("^\\*lexic\\*$" :size 1)))
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
