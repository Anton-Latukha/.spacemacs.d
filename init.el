;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

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

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm    ; Crusial, Do not disable! Provides an easy-to-use API for developers wishing to build their own Helm applications in Emacs, powerful search tools and dozens of already built-in commands providing completion to almost everything. Emacs framework for incremental completions and narrowing selections.
     ;; auto-completion
     ;; better-defaults    ; Better keybindings for Emacs hotkey mode (in config contrary Vim bindings used
     emacs-lisp    ; Crusial, Do not disable!
     multiple-cursors
     treemacs
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     enable-flyspell-auto-completion t
                     )
     ;; version-control    ; Emacs support for different VCS (caution, can collide with separate Git control layers/configs
     ;; ----------------------------------------------------------------
     ;; Manually configured block
     ;;

     ;; Useful to anyone
     syntax-checking
     (org :variables
          org-enable-org-journal-support t ; Organization mode (useful for time tracking!, outlining, note-taking, TO DO lists, spreadsheets, hyperlinks, project planning, GTD, HTML and LaTeX authoring) (real name: evil-org-mode on org-mode of Emacs)
          org-agenda-files
          '(
            "~/org/Flow.org"
            "~/org/Save.org"
            )
          org-highest-priority ?A
          org-default-priority ?E
          org-lowest-priority ?E
          org-tags-match-list-sublevels 'indented ;; in tags search indent sublevels of entries
          ;;org-agenda-todo-list-sublevels nil ;; do not include sublevel TODOs into agenda result
          org-todo-keywords '((sequence "NEXT:(n@)" "PAUS:(p@)" "VIEW:(v!)" "|" "DONE:(d!)" "ODGE:(o!)" "ELEG:(e@)" "CANS:(c@)")) ;; "TODO:(t!)"
          org-journal-dir "~/org/journal/"
          org-journal-file-format "%Y-%m-%d"
          ;; start agenda weekly on the current day
          org-agenda-start-on-weekday nil
          ;; Skip items having a deadline and done
          org-agenda-skip-deadline-if-done t
          ;; Skip items haing a schedule and done
          org-agenda-skip-scheduled-if-done t
          org-deadline-warning-days 14
          org-deadline-string "DUET:"
          org-scheduled-string "SCHT:"
          org-todo-keyword-faces
          '(
            ;; Func list-colors-display to see color names
            ;;("TODO:" . (:foreground "dark orange" :weight bold))
            ("NEXT:" . (:foreground "green" :weight bold))
            ("PAUS:" . (:foreground "grey" :weight bold))
            ("VIEW:" . (:foreground "green" :weight bold))
            ("DONE:" . (:foreground "white" :weight bold))
            ("OGDE:" . (:foreground "white" :weight bold))
            ("ELEG:" . (:foreground "grey" :weight bold))
            ("CANS:" . (:foreground "black" :weight bold))
            )
          ;; Don't allow to DONE the task, until all siblings are DONE
          org-enforce-todo-dependencies t
          ;; Ask for a note when DONE the task
          org-log-done 'note
          org-pomodoro-play-sounds t
          ;; Play sound at start?
          org-pomodoro-start-sound-p t
          org-pomodoro-start-sound "/home/pyro/.spacemacs.d/.audio/bell (+8dB).wav"
          org-pomodoro-finished-sound "/home/pyro/.spacemacs.d/.audio/bell (+8dB).wav"
          org-pomodoro-killed-sound-p t
          org-pomodoro-killed-sound "/home/pyro/.spacemacs.d/.audio/wrapper (norm).wav"
          org-pomodoro-long-break-frequency 4
          org-pomodoro-ask-upon-killing t
          org-pomodoro-length 25
          org-pomodoro-format "%s"
          ;;org-pomodoro-time-format "%.2m:%.2s"
          my-org-refile-additional-targets
          '(
            "~/org/Notes.org"
            "~/org/haskell/haskell.org"
            "~/org/nix/nix.org"
            "~/org/tex/tex.org"
            "~/org/org/org.org"
            )
          org-refile-targets
          '(
            (nil :maxlevel . 9)
            (org-agenda-files :maxlevel . 9)
            (my-org-refile-additional-targets :maxlevel . 9)
            )
          org-outline-path-complete-in-steps nil         ; Refile in a single go
          org-refile-use-outline-path 'file                  ; Show full paths for refiling, value 'file' includes the files themself.
          org-habit-graph-column 130
          ;; Turn off all org autoindentation completely
          org-startup-indented nil
          ;; Turn off manual indentation completely
          org-adapt-indentation nil
          org-brain-path "~/org/"
          )

     (haskell :variables
              haskell-enable-hindent t
              )
     javascript
     shell-scripts

     csv
     html
     markdown
     yaml

     sql

     (git :variables
          magit-repository-directories
          '(
            ("~/org/" . 0)
            ("~/src/" . 1)
            ("~/src/haskell/" . 1)
            ("~/src/ref/" . 1)
            ("~/src/synergy/" . 1)
            ("~/src/zDone/" . 1)
            ("~/src/zGarage/" . 1)
            ("/etc/nixos/" . 0)
            ("~/.emacs.d/" . 0)
            ("~/.spacemacs.d/" . 0)
            ("~/.config/" . 0)
            ("~/.config/fish/" . 0)
            ("~/.config/fish/functions" . 0)
            ("~/.config/git/" . 0)
            ("~/.config/systemd/user" . 0)
            ("~/.config/nixpkgs/" . 0)
            )
          magit-repolist-columns
          '(
            ("Name"    25 magit-repolist-column-ident                  ())
            ("State"    5 magit-repolist-column-dirty                  ())
            ("L"      1 magit-repolist-column-unpushed-to-upstream   ())
            ("U"      1 magit-repolist-column-unpulled-from-upstream ())
            ;; ("Ver" 25 magit-repolist-column-version                ((:right-align t)))
            ("Stash" 25 magit-repolist-column-stashes                ())
            ;; ("Branch" 25 magit-repolist-column-branch                ((:right-align t)))
            ;; ("Upstream" 25 magit-repolist-column-upstream                ((:right-align t)))
            ("Path"    99 magit-repolist-column-path                   ())
            )
          )


     ;; Preferences
     ;;
     ;; Note: some layers require installed packages on system to work with, and enable some features. Look at documentation at URL mentioned at the end of code section.
     ;;

     ;; spell-checking ; Requires aspell and `aspell` dictionary installed for your national languages (aka `aspell-en`)
     auto-completion
     ;; semantic    ; Collection of Emacs Development Environment Tools
     ;; ipython-notebook

     ;; == International support
     ;; unicode-fonts    ; Fonts for any ethnic languages
     ;; keyboard-layout    ; Support for any keyboard layouts
     ;; chinese

     ;; == Programming languages
     ;; agda
     ;; asm
     ;; c-c++
     ;; clojure
     ;; common-lisp
     ;; coq
     ;; csharp
     ;; d
     ;; elixir
     ;; erlang
     ;; fsharp
     ;; go
     ;; groovy
     ;; java
     ;; lua
     ;; ocaml
     ;; perl5
     ;; perl6
     ;; php
     ;; purescript
     python
     ;; racket
     ;; ruby
     ;; rust
     ;; scala
     ;; scheme
     ;; semantic-web    ; Support for Turtle language
     ;; swift
     typescript
     ;; windows-script

     ;; == Frameworks
     ;; django
     ;; react
     ;; ruby-on-rails

     ;; == Domain Specific Languages (DSL)
     ;; gpu    ; Languages to work with GPUs
     ;; major-modes    ; Support of more rare case DSL languages, like Arduino, MATLAB, QML, Vala, Wolfram...
     plantuml

     ;; == Markup languages
     ;; graphviz
     ;; latex
     ;; protobuf
     ;; restructuredtext

     ;; == Tools
     ansible
     (dash :variables ; Dash macOS & Zeal offline documentation tools
           helm-dash-docset-newpath "~/.local/share/Zeal/Zeal/docsets"
           )

     docker
     ;; nginx
     ;; node    ; Package manager
     ;; pandoc
     ;; pdf-tools
     ;; puppet
     ;; restclient
     ;; salt    ; Configuration management tool
     (shell :variables
            shell-default-shell 'eshell
            )
     ;; sphinx    ; Documentation generator by Python
     systemd
     (terraform :variables terraform-auto-format-on-save t)
     ;; tmux
     ;; transmission    ; Torrent client
     ;; vagrant

     ;; == Great
     ;; slack
     ;; osx
     ;; search-engine    ; Support of many search engines
     ;; elfeed    ; Atom and RSS feeds. Elfeed, a web feeds client
     ;; evernote
     spotify
     ;; twitter
     nixos
     ;; floobits    ; Support for pair programming

     ;; For fun
     colors    ; Too extreme fancy coloring
     ;; emoji    ; yes :satisfied:. But caution, it going to conflict with org-mode ':' symbol in time tracking
     ;; games
     ;; selectic    ; Typewriter typing sound
     ;; xkcd    ; xkcd

     ;;
     ;; For more layers and info: http://spacemacs.org/layers/LAYERS.html
     ;;
     ;; ----------------------------------------------------------------

     ;; hie-nix

     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      ;; 'pocket-reader
                                      ;; 'eww
                                      )

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
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

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
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

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
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   ;;dotspacemacs-default-font '("Inconsolata LGC"
   ;;                            :size 13
   ;;                            :weight normal
   ;;                            :width normal
   ;;                            :powerline-scale 1.1
   ;;                            )
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
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

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
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

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
   dotspacemacs-enable-paste-transient-state nil

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
   dotspacemacs-maximized-at-startup nil

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
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

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
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

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
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

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
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; ----------------------------------------------------------------
  ;; Manually configured block
  ;;
  ;;
  ;; FIXME: Agressive indentation broken for Haskell
  ;; (global-aggressive-indent-mode 1)
  (spacemacs/toggle-indent-guide-globally-on)
  (spacemacs/toggle-truncate-lines-off)    ;; turn-off truncating lines by default
  ;; WND: Nix layer global mode intendation problem (https://github.com/NixOS/nix-mode/issues/36) by using relativev-intendation
  ;;(eval-after-load 'nix-mode
  ;;  (add-hook 'nix-mode-hook
  ;;  (lambda ()
  ;;  (setq-local indent-line-function #'indent-relative))))
  ;; ----------------------------------------------------------------
  (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
  (add-hook 'org-mode-hook #'spacemacs/toggle-truncate-lines-off)    ;; turn-off truncating lines in org-mode - wrap them all

  (require 'org)

  (nconc org-modules
         '(
           org-capture
           org-habit
           org-id
           org-protocol
           org-brain
           ))

  ;;;;
  ;;;; This hides :PROPERTIES: in org files
  ;;;;
  ;; This is dumb copy-paste requirement for :PROPERTIES: drawer hiding to work in org-cycle-hide-drawers
  ;; Function to hide all :PROPERTIES: drawers. From this thread: https://www.reddit.com/r/emacs/comments/6tewyl/hide_properties_drawer/
  ;; (require 'org)
  ;; (defun org-cycle-hide-drawers (state)
  ;; "Re-hide all drawers after a visibility state change."
  ;; (when (and (derived-mode-p 'org-mode)
  ;;            (not (memq state '(overview folded contents))))
  ;;   (save-excursion
  ;;     (let* ((globalp (memq state '(contents all)))
  ;;            (beg (if globalp
  ;;                   (point-min)
  ;;                   (point)))
  ;;            (end (if globalp
  ;;                   (point-max)
  ;;                   (if (eq state 'children)
  ;;                     (save-excursion
  ;;                       (outline-next-heading)
  ;;                       (point))
  ;;                     (org-end-of-subtree t)))))
  ;;       (goto-char beg)
  ;;       (while (re-search-forward org-drawer-regexp end t)
  ;;         (save-excursion
  ;;           (beginning-of-line 1)
  ;;           (when (looking-at org-drawer-regexp)
  ;;             (let* ((start (1- (match-beginning 0)))
  ;;                    (limit
  ;;                      (save-excursion
  ;;                        (outline-next-heading)
  ;;                          (point)))
  ;;                    (msg (format
  ;;                           (concat
  ;;                             "org-cycle-hide-drawers:  "
  ;;                             "`:END:`"
  ;;                             " line missing at position %s")
  ;;                           (1+ start))))
  ;;               (if (re-search-forward "^[ \t]*:END:" limit t)
  ;;                 (outline-flag-region start (point-at-eol) t)
  ;;                 (user-error msg))))))))))

  (defun my-indent-setup (n) ;; Group the setting of indent into one variable 'standard indent'
    (setq-default standard-indent n)
    (setq-default evil-shift-width n)
    (setq-default tab-width n)
    (setq-default cperl-indent-level n)
    (setq-default python-indent-offset n)
    ;; java/c/c++
    (setq-default c-basic-offset n)
    ;; web development
    (setq-default coffee-tab-width n) ;; coffeescript
    (setq-default javascript-indent-level n) ;; javascript-mode
    (setq-default js-indent-level n) ;; js-mode
    (setq-default js2-basic-offset n) ;; js2-mode, in latest js2-mode, it's alias of js-indent-level
    (setq-default web-mode-markup-indent-offset n) ;; web-mode, html tag in html file
    (setq-default web-mode-css-indent-offset n) ;; web-mode, css in html file
    (setq-default web-mode-code-indent-offset n) ;; web-mode, js code in html file
    (setq-default css-indent-offset n) ;; css-mode
    (setq-default sh-basic-offset n) ;; shell
    (setq-default sh-indentation n) ;; shell
    (setq-default smie-indent-basic n) ;; shell
    (setq-default fish-indent-offset n) ;; fish shell
    )
  (my-indent-setup 2)

  ;; This aligns org-mode tags to the window border
  (add-hook 'focus-in-hook
            (lambda () (progn
                         ;; Used to use '-7' right align, but due to headline font size is different, nail to the headline:
                         ;;(setq org-tags-column (- 7 (window-body-width)))) (org-align-all-tags)))
                         (setq org-tags-column 0)) (org-align-all-tags)))
  (add-hook 'focus-out-hook
            (lambda () (progn
                         ;;(setq org-tags-column (- 7 (window-body-width)))) (org-align-all-tags)))
                         (setq org-tags-column 0)) (org-align-all-tags)))

  ;; ;;;;
  ;; ;;;; This block tried to fold all code blocks in files:
  ;; ;;;;
  ;; (let ((default-directory  "~/.spacemacs.d/lisp/")) ;; Loading customization el sources
  ;;   (normal-top-level-add-subdirs-to-load-path))
  ;; (load-library "persistent-overlays")
  ;; (when (not (< emacs-major-version 24)) ;; minor modes on by default for all programming modes
  ;;   (add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1) (setq hs-allow-nesting t) (persistent-overlays-minor-mode 1))))
  ;; (when (not (> emacs-major-version 23))
  ;;   (add-hook 'c-mode-hook (lambda () (hs-minor-mode 1) (setq hs-allow-nesting t) (persistent-overlays-minor-mode 1)))
  ;;   (add-hook 'c++-mode-hook (lambda () (hs-minor-mode 1) (setq hs-allow-nesting t) (persistent-overlays-minor-mode 1)))
  ;;   (add-hook 'emacs-lisp-mode-hook (lambda () (hs-minor-mode 1) (setq hs-allow-nesting t) (persistent-overlays-minor-mode 1)))

  ;;   )

  ;; See: https://code.orgmode.org/bzg/org-mode/src/master/lisp/org-faces.el
  (custom-theme-set-faces
   'user
   ;; Customize the Emacs theme to set all tags the same
   `(org-tag                   ((t (:foreground "gray18" :weight bold :height 0.8))))
   ;; So far not really working
   `(org-drawer                ((t (:foreground "black" :weight bold :height 0.8))))
   `(org-property-value        ((t (:foreground "black" :weight bold :height 0.8))))
   )

  ;; Another function that tries to hide properties
  ;; (defun org-toggle-properties ()
  ;;   ;; toggle visibility of properties in current header if it exists
  ;;   (save-excursion
  ;;     (when (not (org-at-heading-p))
  ;;       (org-previous-visible-heading 1))
  ;;     (when (org-header-property-p)
  ;;       (let* ((a (re-search-forward "\n\\:" nil t)))
  ;;         (if (outline-invisible-p (point))
  ;;             (outline-show-entry)
  ;;           (org-cycle-hide-drawers 'all))))))

  (setq org-capture-templates
        `(

          ;; ("P" "Protocol" entry (id 5fef2047-2983-41aa-90e7-5cd0cdf6a5b7)
          ;;  "* %^{Title}\n:PROPERTIES:\n:ADDED: %u\n:SOURCE: %c\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?")
          ;; ("L" "Protocol Link" entry (id 8e4beb2c-27d3-49b6-a1ff-599df21a510c)
          ;;  "* [[%:link][%:description]] %?\n:PROPERTIES:\n:ADDED: %u\n:END:")
          ("d" "Dictionary entry" entry (file ,(concat org-directory "/dictionary/dictionary.org"))
           "* %^{word}\n%\\1 - %^{phonetic} - %?")
          ("t" "TODO:" entry (id 8ec8520c-0d07-4d02-9700-f9f204df91b8)
           "* TODO: %^{Name} %?")
          ("n" "Note" entry (id 2d452153-8cc0-42a3-a2b0-eac119c445fb)
           "* %^{Name} %?")
          ("c" "Capture Generale" entry (id 2d452153-8cc0-42a3-a2b0-eac119c445fb)
           "* %?")
          ("W" "Web site" entry (id cdf8e391-a200-4fd8-8613-356d74303010)
           "* %a\n:properties:\n:added: %u\n:end:\n\n#+begin_comment\n%?\n#+end_comment\n\n%:initial")
          ("b" "Brain: add at the end" plain (function org-brain-goto-end)
           "* %i%?" :empty-lines 1)

          )
        )

  ;; On client launch - show org-agenda week buffer
  ;; This does not work due to Spacemacs initial buffer builds itself at startup
  ;; (setq initial-buffer-choice '(lambda () (get-buffer org-agenda-list)))

  ;; Enable time in the bar
  ;;(setq display-time-24hr-format t)
  (display-time-mode t)

  ;; 2019-02-20: FIXME: TMP fix for '<s[TAB]' - when there PR merge https://github.com/syl20bnr/spacemacs/issues/11798
  (require 'org-tempo)

  ;; Trigger autosave before quitting org-agenda
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

  (defun my-insert-current-date ()
    "Insert ISO date"
    (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))
    )
  (defun my-insert-note ()
    "Insert a NOTE comment"
    (interactive)
    (insert (shell-command-to-string "echo -n '    # NOTE: '$(date +%Y-%m-%d)': '"))
    )
  (defun my-helm-org-rifle-files ()
    "Find the org entry by matching content"
    (interactive)
    (helm-org-rifle-files (append my-org-refile-additional-targets org-agenda-files))
    )

  (spacemacs/declare-prefix "o" "custom")
  (spacemacs/set-leader-keys
    "od" 'my-insert-current-date
    "on" 'my-insert-note
    "or" 'my-helm-org-rifle-files
    )

  (spacemacs/declare-prefix-for-mode 'org-mode "o" "custom")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "oi" 'org-id-get-create
    )

  (add-to-list 'load-path "~/.spacemacs.d/lisp/org-protocol-capture-html")

  (require 'org-protocol-capture-html)
  ;; (setq recentf-max-saved-items 30)

  ;; Just follow those symlinc files
  (setq vc-follow-symlinks t)

  ;; Convert code blocks #+FOO -> #+foo
  (defun modi/lower-case-org-keywords ()
    "Lower case Org keywords and block identifiers.
Example: \"#+TITLE\" -> \"#+title\"
         \"#+BEGIN_EXAMPLE\" -> \"#+begin_example\"
Inspiration: https://code.orgmode.org/bzg/org-mode/commit/13424336a6f30c50952d291e7a82906c1210daf0."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil)
            (count 0))
        ;; Match examples: "#+FOO bar", "#+FOO:", "=#+FOO=", "~#+FOO~",
        ;;                 "‘#+FOO’", "“#+FOO”", ",#+FOO bar",
        ;;                 "#+FOO_bar<eol>", "#+FOO<eol>".
        (while (re-search-forward "\\(?1:#\\+[A-Z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)" nil :noerror)
          (setq count (1+ count))
          (replace-match (downcase (match-string-no-properties 1)) :fixedcase nil nil 1))
        (message "Lower-cased %d matches" count))))

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (emoji-cheat-sheet-plus company-emoji flyspell-correct-helm flyspell-correct auto-dictionary helm-company helm-c-yasnippet fuzzy company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-cabal auto-yasnippet ac-ispell auto-complete csv-mode yaml-mode web-mode web-beautify tagedit sql-indent slim-mode scss-mode sass-mode pug-mode livid-mode skewer-mode simple-httpd less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc insert-shebang helm-css-scss haml-mode fish-mode emmet-mode coffee-mode ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline smeargle restart-emacs rainbow-delimiters popwin persp-mode pcre2el paradox orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file neotree move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum linum-relative link-hint intero info+ indent-guide hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-ag haskell-snippets google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flycheck-pos-tip flycheck-haskell flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word company-ghci company-ghc column-enforce-mode cmm-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (evil-textobj-line emoji-cheat-sheet-plus company-emoji flyspell-correct-helm flyspell-correct auto-dictionary helm-company helm-c-yasnippet fuzzy company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-cabal auto-yasnippet ac-ispell auto-complete csv-mode yaml-mode web-mode web-beautify tagedit sql-indent slim-mode scss-mode sass-mode pug-mode livid-mode skewer-mode simple-httpd less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc insert-shebang helm-css-scss haml-mode fish-mode emmet-mode coffee-mode ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline smeargle restart-emacs rainbow-delimiters popwin persp-mode pcre2el paradox orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file neotree move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum linum-relative link-hint intero info+ indent-guide hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-ag haskell-snippets google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flycheck-pos-tip flycheck-haskell flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word company-ghci company-ghc column-enforce-mode cmm-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-drawer ((t (:foreground "black" :weight bold :height 0.8))))
 '(org-property-value ((t (:foreground "black" :weight bold :height 0.8))) t)
 '(org-tag ((t (:foreground "gray18" :weight bold :height 0.8)))))
)
