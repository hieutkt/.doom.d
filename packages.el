;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! visual-fill-column)
(package! org-ref)
(package! lsp-julia)
(package! lsp-treemacs)
(package! org-fragtog)
(package! org-super-agenda)

(package! lexic :recipe (:host github :repo "tecosaur/lexic"))
(package! org-roam
  :recipe (:host github :repo "jethrokuan/org-roam" :branch "master"))
(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server"))
(package! citeproc-org
  :recipe (:host github :repo "andras-simonyi/citeproc-org"))
