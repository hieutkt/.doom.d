;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! visual-fill-column)
(package! org-ref)
(package! lsp-julia)
(package! org-fragtog)
(package! org-super-agenda)
(package! sdcv)
(package! org-roam
  :recipe (:host github :repo "jethrokuan/org-roam" :branch "master"))
(package! company-org-roam
  :recipe (:host github :repo "jethrokuan/company-org-roam"))
(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server"))
