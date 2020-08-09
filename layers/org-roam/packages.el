(defconst org-roam-packages
  '(org-roam))

(defun org-roam/init-org-roam ()
  (use-package org-roam
    :hook
    (after-init . org-roam-mode)
    :custom
    (org-roam-directory "~/Workspace/FundingCircle/pink-elephants/research")
    :config
    (setq org-roam-dailies-capture-templates
          '(("d" "daily" plain (function org-roam-capture--get-point) ""
             :immediate-finish t
             :file-name "journals/%<%Y-%m-%d>"
             :head "#+TITLE: %<%Y-%m-%d>")))
    :init
    (progn
      (spacemacs/declare-prefix "aor" "org-roam")
      (spacemacs/set-leader-keys
        "aorl" 'org-roam
        "aort" 'org-roam-dailies-today
        "aorf" 'org-roam-find-file
        "aorg" 'org-roam-graph)

      (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "rl" 'org-roam
        "rt" 'org-roam-dailies-today
        "rb" 'org-roam-switch-to-buffer
        "rf" 'org-roam-find-file
        "ri" 'org-roam-insert
        "rg" 'org-roam-graph))))
