;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(unpin! doom-themes)

(package! mixed-pitch)
(package! visual-fill-column)
(package! org-ref)
(package! lsp-treemacs)
(package! org-super-agenda)
(package! keycast)

(package! tree-sitter)
(package! tree-sitter-langs)

(package! org-appear
  :recipe (:host github :repo "awth13/org-appear"))
(package! org-fragtog
  :recipe (:host github :repo "io12/org-fragtog"))
(package! lexic
  :recipe (:host github :repo "tecosaur/lexic"))
(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server"))
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(package! citeproc-org
  :recipe (:host github :repo "andras-simonyi/citeproc-org"))
(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion"))
(package! ess-stata-mode
  :recipe (:host github :repo "emacs-ess/ess-stata-mode"))
