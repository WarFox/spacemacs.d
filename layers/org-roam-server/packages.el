(defconst org-roam-server-packages
  '(org-roam-server))

(defun org-roam-server/init-org-roam-server ()
  (use-package org-roam-server
    :defer
    :config
    (setq org-roam-server-host "127.0.0.1"
          org-roam-server-port 8686
          org-roam-server-authenticate nil
          org-roam-server-export-inline-images t
          org-roam-server-serve-files nil
          org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
          org-roam-server-network-poll t
          org-roam-server-network-arrows nil
          org-roam-server-network-label-truncate t
          org-roam-server-network-label-truncate-length 60
          org-roam-server-network-label-wrap-length 20)))
