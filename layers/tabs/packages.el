;;; packages.el --- tabs layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018, 2020 Sylvain Benner & Contributors
;;
;; Author: Deepu Puthrote <git@deepumohan.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst tabs-packages
  '(centaur-tabs))

(defun tabs/init-centaur-tabs ()
  (use-package centaur-tabs
    :demand
    :config
    (setq centaur-tabs-cycle-scope 'tabs
          centaur-tabs-gray-out-icons 'buffer
          centaur-tabs-height 32
          centaur-tabs-modified-marker "⚠"
          centaur-tabs-set-bar 'left
          centaur-tabs-set-icons t
          centaur-tabs-set-modified-marker t
          centaur-tabs-show-navigation-buttons t
          centaur-tabs-style "bar"
          uniquify-separator "/"
          uniquify-buffer-name-style 'forward)
    (centaur-tabs-headline-match)
    (centaur-tabs-group-by-projectile-project)
    (centaur-tabs-mode t)
    :bind (("C-<prior>" . centaur-tabs-backward)
           ("C-<next>" . centaur-tabs-forward)
           ("C-c t" . centaur-tabs-counsel-switch-group))))

;;; packages.el ends here
