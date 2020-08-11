;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; Copyright (C) 2016, 2017, 2018 by Deepu Mohan Puthrote

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (ansible :variables
              ansible-auto-encrypt-decrypt t)
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t)
     better-defaults
     c-c++
     (clojure :variables
              clojure-enable-fancify-symbols nil
              clojure-enable-sayid t
              clojure-enable-linters t
              clojure-enable-clj-refactor t)
     colors
     command-log
     common-lisp
     confluence
     copy-as-format
     csv
     dap ; debug adapter protocol
     dash
     deft
     docker
     elixir
     elm
     emacs-lisp
     emoji
     erlang
     tabs ;; private layer in layers/
     games
     (git :variables
          ;; magit
          magit-refresh-status-buffer nil
          magit-repository-directories `(("~/Workspace/" . 3)
                                         (,user-emacs-directory . 1)))
     github
     go
     haskell
     html
     imenu-list
     ivy
     (java :variables
           java-backend 'lsp)
     (javascript :variables
                 javascript-import-tool 'import-js
                 javascript-fmt-tool 'prettier
                 javascript-fmt-on-save t
                 javascript-repl `nodejs)
     latex
     lsp
     (markdown :variables
               markdown-fmt-tool 'prettier
               markdown-live-preview-engine 'vmd)
     (multiple-cursors :variables
                       multiple-cursors-backend 'evil-mc)
     nginx
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t
          org-enable-org-journal-support t
          org-journal-enable-agenda-integration t
          org-want-todo-bindings t
          org-enable-jira-support t
          org-enable-sticky-header nil
          org-journal-date-prefix "#+title: "
          org-journal-file-format "%Y-%m-%d.org"
          org-journal-date-format "%A, %d %B %Y"
          org-journal-dir "~/Dropbox/org-mode/journals/" ;; this is overridden in local.el for worklaptop
          org-jira-working-dir "~/Dropbox/org-mode/org-jira" ;; override in local.el for worklaptop
          org-projectile-file "TODOs.org"
          org-projectile-projects-file "~/Dropbox/org-mode/Projects.org"
          org-directory "~/Dropbox/org-mode"
          org-display-custom-times t
          ;; org-time-stamp-custom-formats '("<%d/%m/%Y %a>" . "<%d/%m/%Y %a %H:%M>")
          ;; Override this on each org-file by adding
          ;; #+REVEAL_ROOT: http://cdn.jsdelivr.net/reveal.js/3.1.0/
          org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js/")
     org-roam ;; private layer
     osx
     pdf
     (plantuml :variables
               plantuml-executable-path "/usr/local/bin/plantuml"
               org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2020.15/libexec/plantuml.jar")
     prettier
     (python :variables
             python-backend 'lsp
             python-lsp-server 'pyls
             python-formatter 'black
             python-format-on-save t
             python-pipenv-activate t
             python-test-runner 'pytest
             python-sort-imports-on-save t
             python-auto-set-local-pyvenv-virtualenv 'on-visit ;; Automatically set pyvenv virtualenv from \".venv\".)
             python-auto-set-local-pyenv-version 'on-visit)
     racket
     (ranger :variables
             ranger-show-literal nil)
     react
     restclient
     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-test-runner 'rspec
           ruby-enable-enh-ruby-mode t)
     ruby-on-rails
     rust
     (scala :variables
            scala-auto-insert-asterisk-in-comments t
            scala-indent:use-javadoc-style t)
     scheme
     search-engine
     semantic
     (shell :variables
            shell-default-height 30
            shell-default-shell 'vterm
            shell-default-term-shell "/usr/local/bin/fish"
            shell-default-position 'bottom)
     shell-scripts
     slack
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     enable-flyspell-auto-completion nil)
     spotify
     (sql :variables
          sql-capitalize-keywords t
          sql-auto-indent t)
     swift
     syntax-checking
     (terraform :variables
                terraform-auto-format-on-save t)
     (treemacs :variables
               treemacs-use-filewatch-mode t
               treemacs-use-follow-mode t
               treemacs-use-icons-dired t
               treemcas-use-git-mode 'deferred
               treemacs-use-scope-type 'Perspectives)
     vagrant
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-diff-side 'left)
     (vue :variables
          vue-backend 'dumb)
     xkcd
     yaml)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(all-the-icons
                                      atomic-chrome
                                      beacon
                                      doom-themes
                                      easy-hugo
                                      ejc-sql
                                      eterm-256color
                                      exec-path-from-shell
                                      feature-mode
                                      fira-code-mode
                                      format-sql
                                      github-review
                                      gradle-mode
                                      groovy-mode
                                      highlight-indent-guides
                                      langtool
                                      sqlup-mode)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-28.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (bookmarks . 3)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-dracula
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom :separator arrow ) ; :seperator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("SauceCodePro Nerd Font Mono"
                               :size 14
   dotspacemacs-default-font '("Fira Code"
                               :size 12
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names t

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 5

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols nil

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual` takes precedence over `relative`.
   ;; (default nil)
   dotspacemacs-line-numbers
   '(:relative t
     :disabled-for-modes dired-mode
                         doc-view-mode
                         pdf-view-mode
     :size-limit-kb 1000)

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "pt" "ag" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S [%t] %a"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (add-hook 'term-mode-hook 'toggle-truncate-lines)

  (setq configuration-layer-elpa-archives '(("melpa-stable" . "stable.melpa.org/packages/")
                                            ("melpa" . "melpa.org/packages/")
                                            ("org" . "orgmode.org/elpa/")
                                            ("gnu" . "elpa.gnu.org/packages/")))

  ;; spell-checking use aspell and default to british language
  (setq ispell-program-name "aspell"
        ispell-dictionary "british"))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (beacon-mode 1)

  ;; Atomic chrome
  (use-package atomic-chrome
    :init
    ;; (setq atomic-chrome-ghost-text-port 4001)
    :config
    (atomic-chrome-start-server))

  (use-package fira-code-mode
    :custom (fira-code-mode-disabled-ligatures '())  ; ligatures you don't want
    :hook prog-mode)                                 ; mode to enable fira-code-mode in

  (use-package window-purpose ; workaround until https://github.com/bmag/emacs-purpose/issues/158 is fixed
    :if (= emacs-major-version 27))

  ;; highlight indent
  (use-package highlight-indent-guides-mode
    :hook prog-mode
    :init
     (setq highlight-indent-guides-method 'character))

  ;; doom theme start
  (use-package doom-themes
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t  ; if nil, italics is universally disabled
          doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

  (use-package easy-hugo
    :init
    (setq easy-hugo-basedir "~/Workspace/Personal/tech/"
          easy-hugo-url "https://deepumohan.com/"
          easy-hugo-postdir "content/tech"
          easy-hugo-amazon-s3-bucket-name "your-amazon-s3-bucket-name"
          easy-hugo-default-ext ".org"
          easy-hugo-previewtime "300")
    :bind ("C-c C-e" . easy-hugo))

  ;; frame
  (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . t))

  (add-to-list 'default-frame-alist
               '(ns-appearance . dark)) ;; or light - depending on your theme


  ;; https://github.com/purcell/exec-path-from-shell
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path "/usr/local/bin")
  (exec-path-from-shell-copy-env "PATH")

  ;; M-3 is mapped to window 3, so map M-# to get £ sign GBP (pound sign)
  ;; (This is for Dvorak layout, UK layout may need to map # instead)
  ;; (global-set-key (kbd "M-#") '(lambda() (interactive) (insert "£")))

  ;; This work for british layout
  ;; (global-set-key (kbd "M-£") '(lambda() (interactive) (insert "#")))
  ;; (define-key winum-keymap "\M-3" nil)
  ;; (global-set-key (kbd "M-3")(lambda () (interactive) (insert "#")))

  (setq ns-alternate-modifier 'meta
        ns-right-alternate-modifier 'none)

  (defun my/langtool--version ()
    (nth 2 (split-string (shell-command-to-string "languagetool --version"))))

  ;; Change evil-hybrid-state-cursor cursor to box
  (spacemacs/add-evil-cursor "hybrid" "SkyBlue2" 'box)

  ;; bulk setq section
  (setq
   auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")
   evil-escape-key-sequence "jk"
   ;; javascript
   js2-basic-offset 4
   js-indent-level 2
   ;; langtool settings
   langtool-language-tool-jar (format "/usr/local/Cellar/languagetool/%s/libexec/languagetool-commandline.jar" (my/langtool--version))
   langtool-default-language "en-GB"
   ;; multi-term
   multi-term-scroll-show-maximum-output 't
   multi-term-scroll-to-bottom-on-output 'this
   ;; Set deft-directory to Dropbox so it is in sync
   deft-directory "~/Dropbox/org-mode/deft"

   ;; org-plantuml-jar-path "/usr/local/bin/plantuml"
   org-plantuml-cmd-path "/usr/local/bin/plantuml"

   ;; Projectile disable caching
   projectile-enable-caching t

   ;; pytest fish configuration
   ;; pytest-cmd-format-string "cd %s; and  %s %s %s"
   ;; sh configuration
   pytest-cmd-format-string "cd %s &  %s %s %s"

   python-shell-completion-native-enable nil

   blacken-line-length 100
   blacken-skip-string-normalization t

   ;; Set shell
   shell-file-name "/bin/bash"
   vterm-shell "/usr/local/bin/fish"
   exec-path-from-shell-shell-name "/usr/local/bin/fish"

   ;; use x-widget-webkit-browse-url as default browse-url
   browse-url-browser-function 'xwidget-webkit-browse-url)

  ;; syntax-highlighting for 'dash.el function
  (eval-after-load 'dash '(dash-enable-font-lock))

  (with-eval-after-load 'org
    ;; here goes your Org config :)
    ;; to avoid conflicts with the org shipped with emacs

    ;; active Babel languages
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (clojure . t)
       (emacs-lisp . t)
       (http . t)
       (lisp . t)
       (plantuml . t)
       (python . t)
       (restclient . t)
       (shell . t)
       (sql . t)
       (sqlite . t)))

    ;; load org-tempo
    (use-package org-tempo)

    (with-eval-after-load 'org-capture
      (use-package org-projectile
        :config
        (push (org-projectile-project-todo-entry) org-capture-templates)))

    (with-eval-after-load 'org-agenda
      (setq org-agenda-files '("~/Dropbox/org-mode/deft"
                               "~/Dropbox/org-mode/org-jira"
                               "~/Dropbox/org-mode/journal"))
      ;; Add projectile TODOs.org files to agenda
      (use-package org-projectile
        :config
        (setq org-agenda-files
              (--> (org-projectile-todo-files)
                   (-filter #'file-exists-p it)
                   (-concat it org-agenda-files))))))

  ;; web-mode
  (add-to-list 'auto-mode-alist '("\\.swig\\'" . web-mode))
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))

  ;; Play routes
  (add-to-list 'auto-mode-alist '("\\.routes$" . play-routes-mode))

  ;; Capitalize keywords in SQL mode
  (add-hook 'sql-mode-hook 'sqlup-mode)
  ;; Capitalize keywords in an interactive session (e.g. psql)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode)

  (defun my/change-file-extension ()
    (interactive)
    (let* ((new-extension (read-from-minibuffer "Type the new extension including the dot (.): "))
           (new-file-name (concat (file-name-sans-extension buffer-file-name) new-extension))
           (filename (buffer-file-name)))
      (rename-file filename new-file-name t)
      (rename-buffer (concat (file-name-sans-extension (buffer-name)) new-extension))
      (set-visited-file-name new-file-name)
      (set-buffer-modified-p nil)
      (message (concat "File renamed to " new-file-name))))

  ;; hexo
  (defun my/hexo-blog ()
    "open my hexo blog"
    (interactive)
    (hexo "~/Workspace/Personal/tech"))

  (setq hexo-posix-compatible-shell-file-path "/bin/bash")

  ;; projectile open README file
  (defun my/projectile-open-readme ()
    "open README.md file in the current project root"
    (interactive)
    (find-file (concat (projectile-project-root) "README.md")))

  (defun my/projectile-open-readme-org ()
    "open README.org file in the current project root"
    (interactive)
    (find-file (concat (projectile-project-root) "README.org")))

  (spacemacs/declare-prefix "o" "user")
  (spacemacs/declare-prefix "op" "projectile")
  (spacemacs/declare-prefix "opr" "README")
  (spacemacs/set-leader-keys
    "opro" 'my/projectile-open-readme-org
    "oprm" 'my/projectile-open-readme)

  (spacemacs/declare-prefix "of" "file")
  (spacemacs/set-leader-keys "ofx" 'my/change-file-extension)
  (spacemacs/set-leader-keys "oh" 'my/hexo-blog)

  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

  ;; display time mode in graphics mode
  (defun my/modeline-extras()
    (setq display-time-24hr-format t
          display-time-day-and-date t)
    (display-time-mode t)
    (fancy-battery-mode t))

  (if (display-graphic-p)
      (my/modeline-extras))

  ;; load local.el file it it exists
  (when-let ((local-file "~/.spacemacs.d/local.el")
             (file-exists-p local-file))
    (load-file local-file)))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
