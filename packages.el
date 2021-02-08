;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(unpin! doom-themes)

(package! mixed-pitch)
(package! visual-fill-column)
(package! org-ref)
(package! lsp-treemacs)
(package! org-super-agenda)

(package! eglot-jl)

(package! org-appear
  :recipe (:host github :repo "awth13/org-appear"))
(package! lexic
  :recipe (:host github :repo "tecosaur/lexic"))
(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server"))
(package! citeproc-org
  :recipe (:host github :repo "andras-simonyi/citeproc-org"))
(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion"))

(package! keycast)

