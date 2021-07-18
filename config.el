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
(setq doom-font (font-spec :name "Sarasa Mono Slab J" :size 20)
      doom-variable-pitch-font (font-spec :name "Alegreya" :size 22)
      doom-unicode-font (font-spec :name "JuliaMono" :size 20))

(use-package! unicode-fonts
  :config
  ;; CJK characters
  (dolist (unicode-block '("CJK Unified Ideographs" "CJK Symbols and Punctuation" "CJK Radicals Supplement" "CJK Compatibility Ideographs"))
      (push "Sarasa Mono Slab SC" (cadr (assoc unicode-block unicode-fonts-block-font-mapping))))
  (dolist (unicode-block '("Hangul Syllables" "Hangul Jamo Extended-A" "Hangul Jamo Extended-B"))
    (push "Sarasa Mono Slab K" (cadr (assoc unicode-block unicode-fonts-block-font-mapping)))))

(use-package! mixed-pitch
  :hook ((org-mode helpful-mode) . mixed-pitch-mode)
  :config
  (pushnew! mixed-pitch-fixed-pitch-faces 'warning 'org-cite-key)
  (setq mixed-pitch-set-height t))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox
      doom-themes-treemacs-theme 'doom-colors
      doom-themes-treemacs-enable-variable-pitch nil)


(add-hook! 'doom-load-theme-hook
  (setq org-preview-latex-image-directory
        (concat doom-cache-dir "org-latex/" (symbol-name doom-theme) "/"))
  (dolist (buffer (doom-buffers-in-mode 'org-mode (buffer-list)))
    (with-current-buffer buffer
      (+org--toggle-inline-images-in-subtree (point-min) (point-max) 'refresh)
      (org-clear-latex-preview (point-min) (point-max))
      (org--latex-preview-region (point-min) (point-max)))))

;; Start Doom fullscreen
(add-to-list 'default-frame-alist '(width . 92))
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(alpha 97 100))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(if (and (string-match-p "Windows" (getenv "PATH")) (not IS-WINDOWS))
    (setq dropbox-directory "/mnt/c/Users/X380/Dropbox/")
  (setq dropbox-directory "~/Dropbox/"))

(setq org-directory (concat dropbox-directory "Notes/"))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(remove-hook! '(text-mode-hook) #'display-line-numbers-mode)

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))
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
  (setq evil-goggles-enable-change t
        evil-goggles-enable-delete t
        evil-goggles-pulse         t
        evil-goggles-duration      0.25)
  :config
  (custom-set-faces!
    `((evil-goggles-yank-face evil-goggles-surround-face)
      :background ,(doom-blend (doom-color 'blue) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-paste-face
      :background ,(doom-blend (doom-color 'green) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-delete-face
      :background ,(doom-blend (doom-color 'red) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-change-face
      :background ,(doom-blend (doom-color 'orange) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-commentary-face
      :background ,(doom-blend (doom-color 'grey) (doom-color 'bg-alt) 0.5)
      :extend t)
    `((evil-goggles-indent-face evil-goggles-join-face evil-goggles-shift-face)
      :background ,(doom-blend (doom-color 'yellow) (doom-color 'bg-alt) 0.25)
      :extend t)
    ))

(use-package! yasnippet
  :config
  ;; It will test whether it can expand, if yes, change cursor color
  (defun hp/change-cursor-color-if-yasnippet-can-fire (&optional field)
    (interactive)
    (setq yas--condition-cache-timestamp (current-time))
    (let (templates-and-pos)
      (unless (and yas-expand-only-for-last-commands
                   (not (member last-command yas-expand-only-for-last-commands)))
        (setq templates-and-pos (if field
                                    (save-restriction
                                      (narrow-to-region (yas--field-start field)
                                                        (yas--field-end field))
                                      (yas--templates-for-key-at-point))
                                  (yas--templates-for-key-at-point))))
      (set-cursor-color (if (and templates-and-pos (first templates-and-pos)
                                 (eq evil-state 'insert))
                            (doom-color 'red)
                          (face-attribute 'default :foreground)))))
  :hook (post-command . hp/change-cursor-color-if-yasnippet-can-fire))

(map! :map (term-mode vterm-mode)
      "C-c C-z" 'other-window)

(use-package! epa
  :config
  (epa-file-enable))

(use-package! org
  :config
  (define-key! 'org-mode-map "C-c [" nil) ;org-agenda-file-to-front
  ;; ORG LATEX PREVIEW
  (setq org-startup-with-latex-preview t
        ;; Make latex preview with "C-c C-x C-l" slightly bigger
        org-format-latex-options
        (plist-put org-format-latex-options
                   :scale 1.1)
        ;; Cache the preview images elsewhere
        org-preview-latex-image-directory "~/.cache/ltximg/"
        org-highlight-latex-and-related nil
        org-image-actual-width (/ (display-pixel-width) 3)
        org-ellipsis " "
        org-hidden-keywords '(title))
  ;; Setup custom links
  (+org-init-custom-links-h)
  ;; Custom some face
  (custom-set-faces!
    '((org-block-begin-line org-block-end-line)
      :slant italic)
    '((org-document-title)
      :height 1.5))
  ;; Custom keyword
  (font-lock-add-keywords 'org-mode
                          '(("^\\(?:\"\\)?\\(?:\\[.*\\[|\\)?[[:upper:]]" . 'org-warning)))
  ;; Replace two consecutive hyphens with the em-dash
  (defun hp/org-mode-load-prettify-symbols ()
    (interactive)
    (setq prettify-symbols-alist
          (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                  `(("#+begin_src" . "")
                    ("#+end_src" . "⎺")
                    ("#+begin_quote" . "")
                    ("#+end_quote" . "⎺")
                    ("#+begin_verse" . "")
                    ("#+end_verse" . "⎺")
                    (":properties:" . "")
                    (":end:" . "⎺")
                    (":attach:" . "")
                    ("#+results:" . ""))))
    (pushnew! prettify-symbols-alist
              '("--" . "—")
              '("TODO" . "")
              '("HOLD" . "")
              '("WAIT" . "")
              '("NEXT" . "")
              '("STOP" . "")
              '("DONE" . "")
              '("PROJ" . "")
              '("REPEAT" . "")
              '("REVIEW" . ""))
    (prettify-symbols-mode 1))
  (add-hook 'org-mode-hook 'hp/org-mode-load-prettify-symbols)
  )

(use-package! org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("⁖")
        org-superstar-cycle-headline-bullets nil))

(use-package! org-fancy-priorities
  :config
  (setq org-fancy-priorities-list '("[!!]" "[-!]" "[--]")))

(use-package! org-tempo
  :config
  (tempo-define-template
   "Hugo info" '("#+attr_shortcode info\n#+begin_notice\n" p "\n#+end_notice">)
   "<info")
  (tempo-define-template
   "Hugo tip" '("#+attr_shortcode tip\n#+begin_notice\n" p "\n#+end_notice">)
   "<tip")
  (tempo-define-template
   "Hugo warning" '("#+attr_shortcode warning\n#+begin_notice\n" p "\n#+end_notice">)
   "<warning")
  (tempo-define-template
   "Hugo error" '("#+attr_shortcode error\n#+begin_notice\n" p "\n#+end_notice">)
   "<error")
  (tempo-define-template
   "Hugo example" '("#+attr_shortcode example\n#+begin_notice\n" p "\n#+end_notice">)
   "<example")
  (tempo-define-template
   "Hugo question" '("#+attr_shortcode question\n#+begin_notice\n" p "\n#+end_notice">)
   "<question")
  )

(use-package! oc
  :init
  :config
  (setq org-cite-global-bibliography (list (concat org-directory "/References/zotero.bib"))
        org-cite-export-processors '((latex biblatex)
                                     (t csl)))
  (custom-set-faces!
    `(org-cite-key :foreground ,(face-attribute 'org-formula :foreground)))
  )

(use-package! oc-biblatex
  :after oc
  :config
  (setq org-cite-biblatex-options "backend=biber,style=alphabetic,citestyle=authoryear"))

(use-package! oc-csl
  :after oc
  ;; :init
  ;; (setq org-cite-csl-styles-dir (concat dropbox-directory "Assets/CSL")
  ;;       org-cite-csl-locales-dir org-cite-csl-styles-dir)
  )

(use-package! ox
  :config
  ;; Auto export acronyms as small caps
  ;; Copied from tecosaur
  (defun org-latex-substitute-verb-with-texttt (content)
    "Replace instances of \\verb with \\texttt{}."
    (replace-regexp-in-string
     "\\\\verb\\(.\\).+?\\1"
     (lambda (verb-string)
       (replace-regexp-in-string
        "\\\\" "\\\\\\\\" ; Why elisp, why?
        (org-latex--text-markup (substring verb-string 6 -1) 'code '(:latex-text-markup-alist ((code . protectedtexttt))))))
     content))

  (defun org-export-filter-text-acronym (text backend _info)
    "Wrap suspected acronyms in acronyms-specific formatting.
Treat sequences of 2+ capital letters (optionally succeeded by \"s\") as an acronym.
Ignore if preceeded by \";\" (for manual prevention) or \"\\\" (for LaTeX commands).

TODO abstract backend implementations."
    (let ((base-backend
           (cond
            ;; ((org-export-derived-backend-p backend 'latex) 'latex)
            ((org-export-derived-backend-p backend 'html) 'html)))
          (case-fold-search nil))
      (when base-backend
        (replace-regexp-in-string
         "[;\\\\]?\\b[A-Z][A-Z]+s?\\(?:[^A-Za-z]\\|\\b\\)"
         (lambda (all-caps-str)
           (cond ((equal (aref all-caps-str 0) ?\\) all-caps-str)                ; don't format LaTeX commands
                 ((equal (aref all-caps-str 0) ?\;) (substring all-caps-str 1))  ; just remove not-acronym indicator char ";"
                 (t (let* ((final-char (if (string-match-p "[^A-Za-z]" (substring all-caps-str -1 (length all-caps-str)))
                                           (substring all-caps-str -1 (length all-caps-str))
                                         nil)) ; needed to re-insert the [^A-Za-z] at the end
                           (trailing-s (equal (aref all-caps-str (- (length all-caps-str) (if final-char 2 1))) ?s))
                           (acr (if final-char
                                    (substring all-caps-str 0 (if trailing-s -2 -1))
                                  (substring all-caps-str 0 (+ (if trailing-s -1 (length all-caps-str)))))))
                      (pcase base-backend
                        ('latex (concat "\\acr{" (s-downcase acr) "}" (when trailing-s "\\acrs{}") final-char))
                        ('html (concat "<span class='smallcap'>" (s-downcase acr) "</span>" (when trailing-s "<small>s</small>") final-char)))))))
         text t t))))

  (add-to-list 'org-export-filter-plain-text-functions
               #'org-export-filter-text-acronym)

  ;; We won't use `org-export-filter-headline-functions' because it
  ;; passes (and formats) the entire section contents. That's no good.

  (defun org-html-format-headline-acronymised (todo todo-type priority text tags info)
    "Like `org-html-format-headline-default-function', but with acronym formatting."
    (org-html-format-headline-default-function
     todo todo-type priority (org-export-filter-text-acronym text 'html info) tags info))
  (setq org-html-format-headline-function #'org-html-format-headline-acronymised)

  ;; (defun org-latex-format-headline-acronymised (todo todo-type priority text tags info)
  ;;   "Like `org-latex-format-headline-default-function', but with acronym formatting."
  ;;   (org-latex-format-headline-default-function
  ;;    todo todo-type priority (org-latex-substitute-verb-with-texttt
  ;;                             (org-export-filter-text-acronym text 'latex info)) tags info))
  ;; (setq org-latex-format-headline-function #'org-latex-format-headline-acronymised)
  )

(use-package! ox-latex
  :config
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='%latex -shell-escape -bibtex -interaction=nonstopmode' -pdf -output-directory=%o -f %f"))

  (add-to-list
   'org-preview-latex-process-alist
   '(dvipng :programs
            ("latex" "dvipng")
            :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
            (1.0 . 1.0)
            :latex-compiler
            ("latex -shell-escape -interaction nonstopmode -output-directory %o %f")
            :image-converter
            ("dvipng -D %D -T tight -bg Transparent -o %O %f")))

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
  (setq org-agenda-files (list (concat org-directory "/Agenda")))
  ;; Also to make refiling easier
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  ;; Setting the TODO keywords
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"                    ;What needs to be done
           "NEXT(n)"                    ;A project without NEXTs is stuck
           "|"
           "DONE(d)")
          (sequence
           "REPEAT(e)"                    ;Repeating tasks
           "|"
           "DONE")
          (sequence
           "HOLD(h)"                    ;Task is on hold because of me
           "PROJ(p)"                    ;Contains sub-tasks
           "WAIT(w)"                    ;Tasks delegated to others
           "REVIEW(r)"                  ;Daily notes that need reviews
           "|"
           "STOP(c)"                    ;Stopped/cancelled
           "MEETING(m)"                 ;Meetings
           ))
        org-todo-keyword-faces
        '(("REPEAT" . +org-todo-project)
          ("NEXT" . +org-todo-active)
          ("WAIT" . +org-todo-active)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("REVIEW" . font-lock-keyword-face)))
  ;; Appearance
  (setq org-agenda-category-icon-alist
        `(("work" ,(list (all-the-icons-material "short_text")) nil nil :ascent center)
          ("personal" ,(list (all-the-icons-material "short_text")) nil nil :ascent center)
          ("gcal" ,(list (all-the-icons-material "cloud")) nil nil :ascent center)
          ("birthday" ,(list (all-the-icons-material "cake" :face 'all-the-icons-lpink)) nil nil :ascent center)
          ("learn" ,(list (all-the-icons-material "library_books" :face 'all-the-icons-cyan)) nil nil :ascent center)
          ("blog" ,(list (all-the-icons-material "short_text" :face 'all-the-icons-green)) nil nil :ascent center)
          ("life" ,(list (all-the-icons-material "healing" :face 'all-the-icons-dred)) nil nil :ascent center)
          ("code" ,(list (all-the-icons-material "code" :face 'all-the-icons-green)) nil nil :ascent center)
          ("write" ,(list (all-the-icons-material "create" :face 'all-the-icons-yellow)) nil nil :ascent center)
          )
        org-agenda-prefix-format       " %i %?-2 t%s"
        org-agenda-todo-keyword-format "%-6s"
        org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now"
        org-agenda-time-grid '((today require-timed remove-match)
                               (0900 1200 1400 1700 2100)
                               "      "
                               "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        org-agenda-scheduled-leaders '("" "")
        org-agenda-deadline-leaders '("Deadline: " "Deadline: ")
        )
  ;; Tags triggers
  ;; (setq org-todo-state-tags-triggers)
  ;;
  ;; Clocking
  (setq org-clock-persist 'history
        org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
        org-agenda-start-with-log-mode t)
  (org-clock-persistence-insinuate)
  )

(use-package! org-capture
  :config
  ;;CAPTURE TEMPLATES
  ;;Auxiliary functions
  (defun hp/capture-ox-hugo-post (lang)
    (setq hp/ox-hugo-post--title (read-from-minibuffer "Post Title: ")
          hp/ox-hugo-post--fname (org-hugo-slug hp/ox-hugo-post--title)
          hp/ox-hugo-post--fdate (format-time-string "%Y-%m-%d"))
    (expand-file-name (format "%s_%s.%s.org" hp/ox-hugo-post--fdate hp/ox-hugo-post--fname lang)
                      (concat dropbox-directory "/Blogs/hieutkt/content-org/")))
  ;; Capture templates
  (setq org-capture-templates
        `(("i" "Inbox" entry (file ,(concat org-directory "/Agenda/inbox.org"))
           "* TODO %?\n  %i\n")
          ("m" "Meeting" entry (file ,(concat org-directory "/Agenda/inbox.org"))
           "* MEETING with %? :meeting:\n%t" :clock-in t :clock-resume t)
          ;; Capture template for new blog posts
          ("b" "New blog post")
          ("be" "English" plain (file (lambda () (hp/capture-ox-hugo-post "en")))
           ,(string-join
             '("#+title: %(eval hp/ox-hugo-post--title)"
               "#+subtitle:"
               "#+author: %n"
               "#+date: %(eval hp/ox-hugo-post--fdate)"
               "#+export_file_name: %(concat hp/ox-hugo-post--fname \".en.md\")"
               "#+hugo_paired_shortcodes: <notice notice"
               "#+macro: sidenote {{< sidenote >}}$1{{< /sidenote >}}"
               "#+hugo_base_dir: ../"
               "#+hugo_section: ./posts/"
               "#+hugo_tags: %?"
               "#+hugo_custom_front_matter:"
               "#+hugo_draft: false"
               "#+startup: content"
               "#+options: toc:2 num:t\n")
             "\n")
           :immediate-finish t
           :jump-to-captured t)
          ("bv" "Vietnamese" plain (file (lambda () (hp/capture-ox-hugo-post "vi")))
           ,(string-join
             '("#+title: %(eval hp/ox-hugo-post--title)"
               "#+subtitle:"
               "#+author: %n"
               "#+date: %(eval hp/ox-hugo-post--fdate)"
               "#+export_file_name: %(concat hp/ox-hugo-post--fname \".vi.md\")"
               "#+hugo_paired_shortcodes: <notice notice"
               "#+macro: sidenote {{< sidenote >}}$1{{< /sidenote >}}"
               "#+hugo_base_dir: ../"
               "#+hugo_section: ./posts/"
               "#+hugo_tags: %?"
               "#+hugo_custom_front_matter:"
               "#+hugo_draft: false"
               "#+startup: content"
               "#+options: toc:2 num:t\n")
             "\n")
           :immediate-finish t
           :jump-to-captured t))))

(use-package! org-habit
  :config
  (setq org-habit-show-all-today t))

(use-package! org-super-agenda
  :after org-agenda
  :config
  ;; Enable org-super-agenda
  (org-super-agenda-mode)
  (setq org-agenda-block-separator ?_)
  ;; Customise the agenda view
  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "")
            (todo "NEXT" ((org-super-agenda-groups
                           '((:auto-outline-path t)))))
            (tags-todo "INBOX")
            (tags-todo "@work" ((org-super-agenda-groups
                                 '((:auto-outline-path t)))))
            (tags-todo "@home" ((org-super-agenda-groups
                                 '((:auto-outline-path t)))))))))
  ;; Make evil keymaps works on org-super-agenda headers
  (after! evil-org-agenda
    (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map)))
  ;; Change header face to make it standout more
  (custom-set-faces!
    '(org-super-agenda-header
      :inherit 'variable-pitch
      :weight bold)
    `(org-agenda-structure
      :inherit 'variable-pitch
      :weight bold :foreground ,(doom-color 'blue)))
  )

(use-package! bibtex-completion
  :config
  (setq
   bibtex-dialect                    'biblatex
   bibtex-completion-notes-extension "_notes.org"
   bibtex-completion-bibliography    (concat org-directory "/References/zotero.bib")
   ;; Template for generated note for each entry
   bibtex-completion-notes-path      (concat org-directory "/Org-roam/")
   bibtex-completion-notes-template-multiple-files
   (string-join
    '(
      "${author-or-editor} (${year}): ${title}"
      "#+roam_tags: \"literature\""
      "#+roam_key: cite:${=key=}"
      "#+date: %U"
      "#+last_modified: %U"
      "#+startup: overview"
      "#+startup: hideblocks"
      "#+options: toc:2 num:t"
      "#+hugo_base_dir: ~/Dropbox/Blogs/hieutkt/"
      "#+hugo_section: ./notes"
      "#+hugo_paired_shortcodes: <notice notice"
      "#+macro: sidenote {{< sidenote >}}%(string (char-from-name \"DOLLAR SIGN\"))1{{< /sidenote >}}"
      "#+hugo_custom_front_matter: :exclude true :math true"
      "#+hugo_custom_front_matter: :bibinfo '((doi .\"${doi}\") (isbn . \"${isbn}\") (url . \"${url}\") (year . \"${year}\") (month . \"${month}\") (date . \"${date}\") (author . \"${author}\") (journal . \"${journal}\"))"
      "#+hugo_tags:"
      "#+hugo_series: \"Reading notes\""
      ""
      "* What?"
      "* Why?"
      "* How?"
      "* And?"
      ) "\n")
   ))

(use-package! bibtex-actions
  :init
  (setq bibtex-actions-template-suffix '((t . "          ${=key=:15}    ${=type=:12}    ${keywords:*}")))
  :config
  (dolist (field '(url doi keywords))
    (add-to-list 'bibtex-completion-additional-search-fields field))
  (setq bibtex-actions-symbols
        `((pdf . (,(all-the-icons-faicon "file-pdf-o" :v-adjust 0.02 :face 'all-the-icons-dred) .
                  ,(all-the-icons-faicon "file-pdf-o" :v-adjust 0.02 :face 'bibtex-actions-icon-dim)))
          (note . (,(all-the-icons-faicon "file-text-o" :v-adjust 0.02 :face 'all-the-icons-dblue) .
                   ,(all-the-icons-faicon "file-text-o" :v-adjust 0.02 :face 'bibtex-actions-icon-dim)))
          (link .
                (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
                 ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'bibtex-actions-icon-dim)))))
  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface bibtex-actions-icon-dim
    '((((background dark)) :foreground "#282c34")
      (((background light)) :foreground "#fafafa"))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces)
  ;; Bibtex-actions uses a cache to speed up library display.
  ;; This is great for performance, but means the data can become stale if you modify it.
  (file-notify-add-watch bibtex-completion-bibliography '(change) 'bibtex-actions-refresh)
  ;; Make the 'bibtex-actions' bindings and targets available to `embark'.
  (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
  (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
  (add-to-list 'embark-keymap-alist '(citation . bibtex-actions-map-buffer))
  )

(use-package! bibtex-actions-org-cite
  :bind (("C-c b" . org-cite-insert)
         ("M-o" . org-open-at-point)
         :map minibuffer-local-map
         ("M-b" . bibtex-actions-insert-preset))
  :after (embark org oc bibtex-actions)
  :config
  ;; Make the 'bibtex-actions' org-cite target and bindings available to `embark'.
  (add-to-list 'embark-target-finders 'bibtex-actions-org-cite-citation-finder)
  ;; Specify the org-cite processors to use.
  (setq org-cite-follow-processor 'bibtex-actions-org-cite
        org-cite-insert-processor 'bibtex-actions-org-cite))


(use-package! org-ref
  :config
  (setq
   org-ref-default-bibliography      `(,(concat org-directory "/References/zotero.bib"))
   org-ref-pdf-directory             (concat org-directory "/papers/"))
  ;; make org-ref-cite-face a bit less intrusive
  (custom-set-faces!
    `(org-ref-cite-face :weight unspecified :foreground unspecified
                        :underline ,(doom-color 'grey))))

;; (use-package! citeproc-org
;;   :after org-ref
;;   :config
;;   (setq org-ref-default-bibliography (list bibtex-completion-bibliography))
;;   (citeproc-org-setup))

(use-package! org-roam
  :after org
  :init
  (setq org-roam-directory (concat org-directory "/Org-roam/")
        org-roam-v2-ack t
        ;; These are org-related config
        org-id-track-globally t
        org-id-link-to-org-use-id t)
  :config
  (org-roam-setup)
  ;; Make org-roam faces less intrusive
  (custom-set-faces!
    `((org-roam-link org-roam-link-current)
      :inherit unspecified :underline ,(doom-color 'violet))
    `((org-link)
      :inherit unspecified :underline ,(doom-color 'blue)))
  ;; how the hierarchy of a node relative to the file it is in
  (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
    "Return the file TITLE for the node."
    (org-roam-get-keyword "TITLE" (org-roam-node-file node)))

  (cl-defmethod org-roam-node-firsttag ((node org-roam-node))
    "The first tag of notes are used to denote note type"
    (let* ((specialtags '("journal" "concept" "data" "bio" "literature" "compilation" "argument"))
           (tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node)))
           (firsttag (seq-intersection specialtags tags 'string=)))
      (concat
       (if firsttag
           (all-the-icons-octicon "gear" :face 'all-the-icons-silver :v-adjust 0.02)
         (all-the-icons-octicon "gear" :face 'org-roam-dim :v-adjust 0.02))
       (string-join firsttag ", "))))

  (cl-defmethod org-roam-node-cleantags ((node org-roam-node))
    "Return the file TITLE for the node."
    (let* ((tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node)))
           (specialtags '("journal" "concept" "data" "bio" "literature" "compilation" "argument"))
           (othertags (seq-difference tags specialtags 'string=)))
      (concat
       (if othertags
           (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02)) " "
                   (propertize (string-join othertags ", ") 'face 'all-the-icons-dgreen))))

  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    "Return the hierarchy for the node."
    (let* ((title (org-roam-node-title node))
          (olp (mapcar (lambda (s) (if (> (length s) 10) (concat (substring s 0 10)  "...") s)) (org-roam-node-olp node)))
          (level (org-roam-node-level node))
          (filetitle (org-roam-node-filetitle node))
          (shortentitle (if (> (length filetitle) 10) (concat (substring filetitle 0 10)  "...") filetitle))
          (separator (concat " " (all-the-icons-material "chevron_right") " ")))
      (cond
       ((= level 1) (concat (all-the-icons-material "list" :face 'all-the-icons-green :v-adjust 0.02) " "
                            (propertize shortentitle 'face 'org-roam-dim) separator title))
       ((> level 1) (concat (all-the-icons-material "list" :face 'all-the-icons-dpurple :v-adjust 0.02) " "
                             (propertize (concat shortentitle separator (string-join olp separator)) 'face 'org-roam-dim)
                             separator title))
       (t (concat (all-the-icons-faicon "file-text-o" :face 'all-the-icons-yellow :v-adjust 0.02) " " title)))
      ))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")]
                         (org-roam-node-id node)))))
      (if (> count 0)
        (concat (all-the-icons-material "link" :face 'all-the-icons-dblue) (format "%d" count))
        (concat (all-the-icons-material "link" :face 'org-roam-dim) " "))))

  (setq org-roam-node-display-template
        (concat  "${backlinkscount:3} ${firsttag:13} ${hierarchy} ${cleantags}"))
  ;; Keys binding
  (map! :leader
        :prefix "n"
        (:prefix ("r" . "Org-roam")
         :desc "Org Roam Capture"              "c" #'org-roam-capture
         :desc "Find node"                     "f" #'org-roam-node-find
         :desc "Show graph"                    "g" #'org-roam-graph
         :desc "Insert"                        "i" #'org-roam-node-insert
         :desc "Toggle roam buffer"            "r" #'org-roam-buffer-toggle
         :desc "Refile"                        "w" #'org-roam-refile
         (:prefix ("a" . "Roam Alias")
          :desc "Add alias"                    "a" #'org-roam-alias-add
          :desc "Remove alias"                 "d" #'org-roam-alias-remove)))
  (map! :map org-roam-mode-map
        :n "<return>" #'org-roam-visit-thing
        :n "<tab>" #'magit-section-toggle)

  ;; Custom popup rule
  (set-popup-rules!
    '(("^\\*org-roam\\*$" :side right :size 0.25 :ttl nil))))

(use-package! org-roam-db
  :config
  (setq org-roam-db-location "~/.emacs.d/org-roam.db"))

(use-package! org-roam-capture
  :config
  (setq org-roam-capture-templates
        '((("d" "default" plain "%?"
            :if-new
            (file+head "${slug}_%<%Y-%m-%d--%H-%M-%S>.org"
                       "#+title: ${title}\n#+filetags: %?\n#+created: %U\n#+last_modified: %U\n\n")
            :unnarrowed t)))
        org-roam-dailies-capture-templates
        '(("d" "daily" entry
           "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %a>\n#+filetags: journal\n#+startup: overview\n#+created: %U\n#+last_modified: %U\n\n")
           :immediate-finish t))
        org-roam-capture-ref-templates
        '(("r" "ref" plain "%?" :if-new
           (file+head "web_${slug}_%<%Y-%m-%d--%H-%M-%S>.org"
                      "#+title: ${title}\n#+filetags: website\n#+created: %U\n#+last_modified: %U\n")
           :unnarrowed t)
          ;; Browser bookletmark template:
          ;; javascript:location.href =
          ;; 'org-protocol://roam-ref?template=w&ref='
          ;; + encodeURIComponent(location.href)
          ;; + '&title='
          ;; + encodeURIComponent(document.getElementsByTagName("h1")[0].innerText)
          ;; + '&hostname='
          ;; + encodeURIComponent(location.hostname)
          ("w" "webref" entry "* ${title} ([[${ref}][${hostname}]])\n%?"
           :if-new
           (file+head "%<%Y-%m-%d>.org"
                      "#+title: %<%Y-%m-%d %a>\n#+filetags: journal\n#+startup: overview\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)
          ))

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
  (setq org-roam-dailies-directory "journal/")
  (map! :leader
        :prefix "n"
        (:prefix ("j" . "journal")
          :desc "Arbitrary date" "d" #'org-roam-dailies-goto-date
          :desc "Today"          "j" #'org-roam-dailies-goto-today
          :desc "Tomorrow"       "m" #'org-roam-dailies-goto-tomorrow
          :desc "Yesterday"      "y" #'org-roam-dailies-goto-yesterday)))

;; HACK org-roam-bibtex is loaded but unused
;;      so that the capture template is not overrided
;; (use-package! org-roam-bibtex
;;   :after org-roam
;;   :hook (org-roam-mode . org-roam-bibtex-mode))

;; (use-package! org-roam-server
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8080
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))


(use-package! org-noter
  :config
  ;; Split fraction: (horizontal . vertical)
  (setq org-noter-doc-split-fraction '(0.66 . 0.66))
  ;; Correct some mappings
  (after! pdf-tools
    (map! :map pdf-view-mode-map
          :n "i" #'org-noter-insert-note
          :n "I" #'org-noter-insert-note-toggle-no-questions
          :n "Q" #'org-noter-kill-session)))

(use-package! org-appear
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t))

(use-package! org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

(use-package! visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode)
  :init
  (setq visual-fill-column-width 92
        visual-fill-column-center-text t
        visual-fill-column-fringes-outside-margins nil))

(use-package! ess
  :config
  (set-popup-rules!
    '(("^\\*R:*\\*$" :side right :size 0.5 :ttl nil))))

(use-package! ess-stata-mode
  :after ess
  :config
  (setq inferior-STA-start-args ""
        inferior-STA-program (executable-find "stata")
        inferior-STA-program-name (executable-find "stata"))
  (add-to-list 'org-src-lang-modes '("jupyter-stata" . stata)))

(use-package! python
  :config
  (set-popup-rules!
    '(("^\\*Python:*\\*$" :side right :size 0.5 :ttl nil))))

(use-package! julia-repl
  :config
  ;; Use vterm instead of the defautl term
  (when (featurep 'vterm)
    (julia-repl-set-terminal-backend 'vterm))
  ;; Make popup position similar to `ess'
  (set-popup-rules!
    '(("^\\*julia.*\\*$" :side right :size 0.5 :ttl nil))))

;; lsp-mode seems to serve an invalid response to the Julia server.
;; The pseudo-fix is rather simple at least.
(after! julia-mode
  (add-hook! 'julia-mode-hook
    (setq-local lsp-enable-folding t
                lsp-folding-range-limit 100)))



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


(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
                      :height 0.9)
    '(keycast-key :inherit custom-modified
                  :height 1.1
                  :weight bold)))

(use-package! org-gcal
  :commands org-gcal-fetch
  :config
  (load-file (concat dropbox-directory "/Auths/org-gcal-settings.el.gpg")))

(use-package! org-download
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  ;; Change how inline images are displayed
  (setq org-download-display-inline-images nil))

(use-package! org-transclusion
  :commands org-transclusion-mode
  :config
  (setq org-transclusion-include-first-section t)
  (add-to-list  'org-transclusion-exclude-elements 'keyword))


(use-package! ansi-color
  :config
  (defun hp/display-ansi-colors ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))


(use-package! tree-sitter
  :config
  (use-package! tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package! emojify
  :config
  (setq emojify-emoji-set "twemoji-v2"))

(use-package! page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package! clip2org)
(use-package! org-ol-tree)
