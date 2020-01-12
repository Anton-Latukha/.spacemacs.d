;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;;; Layers configuration

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
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(

;;; Layers

;;;; Initial, misc

     emacs-lisp    ; Crusial, Do not disable!
     helm    ; Crusial, Do not disable! Provides an easy-to-use API for developers wishing to build their own Helm applications in Emacs, powerful search tools and dozens of already built-in commands providing completion to almost everything. Emacs framework for incremental completions and narrowing selections.
     multiple-cursors
     treemacs
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     enable-flyspell-auto-completion t
                     )

     syntax-checking

     javascript
     shell-scripts

     csv
     html
     markdown
     yaml

     sql

     systemd
     (terraform :variables terraform-auto-format-on-save t)
     ;; tmux

     nixos

     colors    ; Too extreme fancy coloring

;;;; Org

     (org;; Organization mode (useful for time tracking!, outlining, note-taking, TO DO lists, spreadsheets, hyperlinks, project planning, GTD, HTML and LaTeX authoring) (real name: evil-org-mode on org-mode of Emacs)
      :variables
      org-enable-org-journal-support t
      org-agenda-files
      '(
        "~/org/Flow.org"
        "~/org/Habit flow.org"
        )
      org-highest-priority ?A
      org-default-priority ?E
      org-lowest-priority ?E
      org-tags-match-list-sublevels 'indented ;; in tags search indent sublevels of entries
      ;;org-agenda-todo-list-sublevels nil ;; do not include sublevel TODOs into agenda result
      org-todo-keywords '((sequence "NEXT:(n@)" "PAUS:(p@)" "VIEW:(v!)" "|" "DONE:(d!)" "ODGE:(o!)" "ELEG:(e@)" "CANS:(c@)")) ;; "TODO:(t!)"
      org-journal-dir "~/org/journal/"
      org-journal-file-format "%Y-%m-%d"
      org-agenda-start-on-weekday nil    ;; Start agenda weekly on the current day
      org-agenda-skip-deadline-if-done t    ;; Skip done with deadline
      org-agenda-skip-scheduled-if-done t    ;; Skip done with schedule
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
        ("DONE:" . (:foreground "black" :background "grey" :weight bold))
        ("OGDE:" . (:foreground "white" :weight bold))
        ("ELEG:" . (:foreground "grey" :weight bold))
        ("CANS:" . (:foreground "black" :background "grey" :weight bold))
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
        "~/org/Web-archive save.org"
        "~/org/Save.org"
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
      org-habit-preceding-days 56

      ;; Turn off all org autoindentation completely
      org-startup-indented nil
      ;; Turn off manual indentation completely
      org-adapt-indentation nil
      org-brain-path "~/org/"

      org-log-into-drawer t    ; if t log into the :LOGBOOK: drawer

      ;; org-agenda-time-grid
      ;; (quote
      ;;  (
      ;;   (daily today require-timed)
      ;;   (600 700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
      ;;   "      "
      ;;   "----------------"
      ;;   )
      ;;  )

      ;; ;; Taken from https://github.com/jwiegley/dot-emacs/blob/master/org-settings.el
      ;; org-agenda-sorting-strategy '((agenda habit-down time-up todo-state-up priority-down)
      ;;                               (todo priority-down category-keep)
      ;;                               (tags priority-down category-keep)
      ;;                               (search category-keep))

      ;; List of always declared tags, function my-org-auto-tag matches and autoadds occuring words from it

      org-agenda-time-grid nil    ;; Disable the time grid
      org-tag-alist
      '(
        ("advanced")
        ("ai")
        ("analyze")
        ("android")
        ("api")
        ("applicative")
        ("article")
        ("auto")
        ("automate")
        ("automation")
        ("babel")
        ("backup")
        ("bartosz")
        ("bash")
        ("basic")
        ("benchmark")
        ("best")
        ("blog")
        ("blogpost")
        ("book")
        ("bookmark")
        ("bpf")
        ("cache")
        ("calculus")
        ("category")
        ("chapter")
        ("cli")
        ("cloud")
        ("code")
        ("combinator")
        ("computer")
        ("computer_science")
        ("conf")
        ("conference")
        ("config")
        ("configuration")
        ("container")
        ("continuation")
        ("contrib")
        ("course")
        ("cps")
        ("database")
        ("db")
        ("debug")
        ("default")
        ("denotation")
        ("dep")
        ("devops")
        ("doc")
        ("drill")
        ("ebpf")
        ("efficiency")
        ("emacs")
        ("error")
        ("everything")
        ("experience")
        ("feature")
        ("firefox")
        ("fish")
        ("flow")
        ("forall")
        ("functional")
        ("functional_programming")
        ("functor")
        ("gadt")
        ("generic")
        ("git")
        ("good")
        ("good_code")
        ("gtd")
        ("guide")
        ("hackage")
        ("haskell")
        ("hedgehog")
        ("helm")
        ("ide")
        ("immidiately")
        ("important")
        ("integration")
        ("io")
        ("kde")
        ("key")
        ("lambda")
        ("lambda_calculus")
        ("later")
        ("learn")
        ("learning")
        ("lenses")
        ("lib")
        ("library")
        ("lisp")
        ("ltp")
        ("mantra")
        ("math")
        ("medium")
        ("memory")
        ("middle")
        ("minimal")
        ("minimize")
        ("monad")
        ("monoid")
        ("news")
        ("nix")
        ("nixops")
        ("nixos")
        ("nixpkgs")
        ("notation")
        ("note")
        ("optimization")
        ("optimize")
        ("order")
        ("order_theory")
        ("org")
        ("org-mode")
        ("organize")
        ("package")
        ("pattern")
        ("patternmatch")
        ("performance")
        ("pine64")
        ("pitfall")
        ("pkg")
        ("plan")
        ("planning")
        ("pr")
        ("pragma")
        ("profiling")
        ("programming")
        ("project")
        ("projectile")
        ("property")
        ("proxy")
        ("quickcheck")
        ("reddit")
        ("regex")
        ("report")
        ("request")
        ("research")
        ("resource")
        ("safe")
        ("science")
        ("semantic")
        ("servant")
        ("set")
        ("share")
        ("sh")
        ("shell")
        ("spacemacs")
        ("stability")
        ("style")
        ("style_guide")
        ("symbol")
        ("sync")
        ("term")
        ("terminal")
        ("termux")
        ("test")
        ("testing")
        ("text")
        ("theory")
        ("thread")
        ("time")
        ("tool")
        ("tooling")
        ("top")
        ("trace")
        ("tracing")
        ("tray")
        ("tutorial")
        ("type")
        ("vi")
        ("video")
        ("vim")
        ("web")
        ("webpage")
        ("website")
        ("wiki")
        ("work")
        ("workflow")
        ("xmonad")
        ("zsh")
        )
      org-cycle-separator-lines 0 ;; 0 - turns off empty lines between collapsed headers
      org-catch-invisible-edits 'smart  ;; Determins behaviour when you do invisible edits in (...)

      ;; org-bullets-bullet-list '(" ")    ;; no bullets, needs org-bullets package
      org-ellipsis "…"    ;; folding symbol
      org-pretty-entities t    ;; show parsed special syntax (unicode) insted of source code
      org-hide-emphasis-markers t    ;; show actually italicized text instead of /italicized text/
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-export-headline-levels 10
      org-enable-github-support t
      org-enable-bootstrap-support t
      org-enable-reveal-js-support t
      org-agenda-dim-blocked-tasks 'invisible    ;; Make blocked tasks invisible, show only child tasks.

      )

;;;; LSP

     (lsp :variables
          default-nix-wrapper (lambda (args)
                                (append
                                 (append (list "nix-shell" "-I" "." "--command" )
                                         (list (mapconcat 'identity args " "))
                                         )
                                 (list (nix-current-sandbox))
                                 )
                                )
          lsp-haskell-process-wrapper-function default-nix-wrapper
          )

;;;; Haskell

     (haskell :variables
              haskell-enable-hindent t
              haskell-completion-backend 'lsp
              haskell-process-type 'cabal-new-repl
              )

;;;; Git

     (git :variables
          magit-repository-directories
          '(
            ("~/org/" . 0)
            ("~/src/" . 1)
            ("~/src/haskell/" . 1)
            ("~/src/nix/" . 1)
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


     auto-completion    ;; Completion seems not of quality and unrelevant for now

     python
     typescript
     (plantuml
      ;; :variables
      ;; plantuml-java-command "/run/current-system/sw/bin/plantuml"
      ;; plantuml-jar-path " "
      ;; org-plantuml-jar-path "/run/current-system/sw/bin/plantuml"
      )

     ;; == Tools
     ansible
     (
      dash
      :variables ; Dash macOS & Zeal offline documentation tools
      helm-dash-docset-newpath "~/.local/share/Zeal/Zeal/docsets"
      )

     docker
     pandoc

;;;; Shell

     (
      shell
      :variables
      shell-default-shell 'ansi-term
      ;; shell-default-shell 'eshell
      shell-default-term-shell "/run/current-system/sw/bin/fish"
      )

;;;; Geolocation
     (
      geolocation
      :variables
      geolocation-enable-location-service t
      geolocation-enable-weather-forecast t

      calendar-location-name "Kyiv, Ukraine"
      calendar-latitude 50.4755605
      calendar-longitude 30.4039573
      )

;;;; Theming

     (theming
      :variables ;; Streamlines faces modifications

      theming-modifications
      '(
        (
         spacemacs-dark

         ;; Font locking
         ;; (font-lock-comment-face :slant italic)
         ;; (web-mode-html-attr-name-face :inherit font-lock-variable-name-face
         ;;                               :foreground nil)
         ;; ;; Modeline
         ;; (powerline-active1 :box (:color "#999999"
         ;;                                 :line-width 1
         ;;                                 :style released-button)
         ;;                    :background "#5a5a5a")))
         (
          org-tag
          :foreground "gray18"
          :weight bold
          :height 0.8)

         ;;;; This was a try to make all drawer almost invisible. It mimics the source face code and fails miserably.
         ;; (org-drawer ((class '((class color) (min-colors 89)) (
         ;;                                                       :foreground "gray18"
         ;;                                                       :background "gray18")
         ;;                                                       )))

         (
          org-property-value
          :foreground "gray18"
          )

         (
          default
          :background
          "black"
          )
         (
          h1-line
          :background
          "black"
          )
         (font-lock-comment-face
          :background
          "black"
          :foreground
          "Navajowhite4"
          )
         (font-lock-comment-delimiter-face
          :background
          "black"
          :foreground
          "grey16"
          )
         (helm-buffer-file
          :background
          "black"
          )
         (helm-buffer-directory
          :background
          "black"
          )
         (helm-ff-directory
          :background
          "black"
          )
         (helm-ff-file
          :background
          "black"
          )
         (org-special-keyword
          :foreground
          "grey18"
          )
         (org-block-begin-line
          :background
          "#000030"
          :foreground
          "#000030"
          )
         (org-block
          :background
          "#000030"
          )
         (org-block-end-line
          :background
          "#000030"
          :foreground
          "#000030"
          )
         (haskell-constructor-face
          :foreground
          "#ffa600"
          )
         (haskell-type-face
          :foreground
          "#7a37ff"
          )
         (haskell-definition-face
          :foreground
          "#ca8300"
          )
         (haskell-keyword-face
          :foreground
          "#00dede"
          ;; "#00ffff"
          ;; "#0084ff"
          )
         (haskell-operator-face
          :foreground
          "#00dede"
          ;; "#00ffff"
          ;; "dodger blue"
          )
         (font-lock-string-face
          :foreground
          "#356a00")
         ))

      ;; See: https://code.orgmode.org/bzg/org-mode/src/master/lisp/org-faces.el
      ;; (custom-theme-set-faces
      ;;  'user
      ;;  ;; Customize the Emacs theme to set all tags the same
      ;;  `(org-tag                   ((t (:foreground "gray18" :weight bold :height 0.8))))
      ;;  ;; So far not really working
      ;;  `(org-drawer                ((t (:foreground "black" :weight bold :height 0.8))))
      ;;  `(org-property-value        ((t (:foreground "black" :weight bold :height 0.8))))
      ;;  )

      )

;;;; WakaTime

     (wakatime
      :variables
      wakatime-api-key  "b5292c00-f691-4070-9a8c-2a3b61e6e360"
      ;; use the actual wakatime path
      wakatime-cli-path "/run/current-system/sw/bin/wakatime"
      )

;;;; Elfeed

     (elfeed
      :variables
      rmh-elfeed-org-files (list "~/org/elfeed.org")
      elfeed-db-directory "~/.cache/elfeed/"
      elfeed-initial-tags '(unread)
      ;; elfeed-enable-web-interface t
      ;; httpd-port 8081
      ;; url-queue-timeout 30
      )

;;;; Other layers

     unicode-fonts

     ;; github

     copy-as-format

     speed-reading

     (pdf
      :variables
      pdf-view-display-size 'fit-page    ;; Use fit-page by default
      pdf-annot-activate-created-annotations t    ;; Automatically start annotating what is highlighted
      pdf-view-resize-factor 1.1    ;; Grain of the zooming step
      )

     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
;;;; Packages

   dotspacemacs-additional-packages
   '(
     direnv
     nix-sandbox
     haskell-snippets
     org-super-agenda
     shm ;; Structured Haskell mode
     outshine ;; For managing code with Outlines
     pretty-mode
     telega ;; Emacs Telega.el client TODO: Wait when https://github.com/syl20bnr/spacemacs/issues/12800 solved
     org-drill ;; Moved-out of org-mode at 9.3
     )

;;;; Misc package options

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

;;; Init configuration

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

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

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
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

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

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

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

   ;; Default font, or prioritized list of fonts.
   ;;`powerline-scale' allows to quickly tweak the mode-line size to make
   ;; separators look not too crappy.
   dotspacemacs-default-font
   '(
     "Inconsolata LGC"
     :size 15
     :weight normal
     :width normal
     :powerline-scale 1.1
     )

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
   dotspacemacs-mode-line-unicode-symbols t

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
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
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

;;; User-{env, init, config}

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

;;; User-config

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
  ;; (add-hook 'haskell-mode-hook #'turn-on-haskell-unicode-input-method) ;; May be in conflict with Unicode visual replacement function that is also haskell-mode-hook
  (add-hook 'org-mode-hook #'spacemacs/toggle-truncate-lines-off)    ;; turn-off truncating lines in org-mode - wrap them all

  (require 'org)

  (nconc
   org-modules
   '(
     org-capture
     org-habit
     org-id
     org-protocol
     org-brain
     org-drill
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

;;;; Indenting setup

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

;;;; Align tags

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

;;;; Org capture templates

  (setq org-capture-templates
        `(

          ;; ("P" "Protocol" entry (id 5fef2047-2983-41aa-90e7-5cd0cdf6a5b7)
          ;;  "* %^{Title}\n:PROPERTIES:\n:ADDED: %u\n:SOURCE: %c\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?")
          ;; ("L" "Protocol Link" entry (id 8e4beb2c-27d3-49b6-a1ff-599df21a510c)
          ;;  "* [[%:link][%:description]] %?\n:PROPERTIES:\n:ADDED: %u\n:END:")

          ("d" "Dictionary entry" entry
           (file ,(concat org-directory "/dictionary/dictionary.org"))
           "* %^{word} :drill:\n%\\1 - %^{phonetic} - %?"
           :empty-lines-before 1
           :empty-lines-after 1
           )

          ("w" "Word entry" entry
           (file ,(concat org-directory "/word/word.org"))
           "* %?\n"
           :empty-lines-before 1
           :empty-lines-after 1
           )

          ("t" "TODO:" entry
           (id 8ec8520c-0d07-4d02-9700-f9f204df91b8)
           "* TODO: %^{Name} %?"
           :empty-lines-before 1
           :empty-lines-after 1
           )

          ("N" "Note" entry
           (id 2d452153-8cc0-42a3-a2b0-eac119c445fb)
           "* %^{Name} %?"
           :empty-lines-before 1
           :empty-lines-after 1
           )

          ("c" "Capture Generale" entry
           (id 2d452153-8cc0-42a3-a2b0-eac119c445fb)
           "* %?"
           :empty-lines-before 1
           :empty-lines-after 1
           )

          ("W" "Web site" entry
           (file ,(concat org-directory "/Web-archive save.org"))
           "* %a\n:properties:\n:added: %u\n:end:\n%:initial"
           :empty-lines-before 1
           :empty-lines-after 1
           ;; (id cdf8e391-a200-4fd8-8613-356d74303010)
           )

          ("b" "Brain: add at the end" plain
           (function org-brain-goto-end)
           "* %i%?" :empty-lines 1
           :empty-lines-before 1
           :empty-lines-after 1
           )

          ("n" "Next:" entry
           (id ebc243d3-f040-49a0-940d-fbfd16b46edd)
           "* NEXT: %?\nSCHT: %t"
           :empty-lines-before 1
           :empty-lines-after 1
           )

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

;;;; My functions

  (defun my-insert-current-date ()
    "Insert ISO date"
    (interactive)
    (insert (format-time-string "%Y-%m-%d"))
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

  (defun my-org-toggle-tag-drill ()
    "Toggle :drill: tag"
    (interactive)
    (org-toggle-tag "drill")
    )

  (defun my-safe-exit ()
    "Saving all files on exit"
    (interactive)
    (save-buffers-kill-terminal t)
    )

  ;; TODO: not used
  (defun my-archive-when-done ()
    "Archive current entry if it is marked as DONE (see `org-done-keywords')."
    (when (org-entry-is-done-p)
      (org-archive-subtree-default)))

  ;;;; 2019-06-12: NOTE: Strips link from selected text
  ;;;; Taken from: https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
  (defun my-org-link-delete-link ()
    "Remove the link part of an org-mode link at point and keep
only the description"
    (interactive)
    (let ((elem (org-element-context)))
      (if (eq (car elem) 'link)
          (let* ((content-begin (org-element-property :contents-begin elem))
                 (content-end  (org-element-property :contents-end elem))
                 (link-begin (org-element-property :begin elem))
                 (link-end (org-element-property :end elem)))
            (if (and content-begin content-end)
                (let ((content (buffer-substring-no-properties content-begin content-end)))
                  (delete-region link-begin link-end)
                  (insert content)))))))

  ;;;; Example of a more intriquiret refile
  ;; (defun my-local-org-refile (arg)
  ;;   "Refile to /level/ in /file/ by using use /prefix args/: 2+/this/[none], 1+/this/1, 1/choose-file/2"
  ;;   (interactive "P")
  ;;   (cond
  ;;    ((not (null arg))
  ;;     (let ((val (car current-prefix-arg))
  ;;           (current-prefix-arg nil)
  ;;           (org-refile-use-outline-path 'file)
  ;;           (org-reverse-note-order nil))
  ;;       (cond ((= val 4)
  ;;              (call-interactively 'org-refile))
  ;;             ((= val 16)
  ;;              (let* ((fil (read-file-name "Enter destination file: "))
  ;;                     (xfil (expand-file-name fil))
  ;;                     (_ (when (not (file-exists-p xfil))
  ;;                          (with-temp-file xfil (insert))))
  ;;                     (org-refile-targets
  ;;                      `((,xfil :maxlevel . 10))))
  ;;                (progn (call-interactively 'org-refile)
  ;;                       (find-file xfil)))))))
  ;;    (t
  ;;     (call-interactively 'org-refile))))

  ;; Refile to open buffers
  ;; (defun my-local-org-files-list ()
  ;;   (delq nil
  ;;         (mapcar (lambda (buffer)
  ;;                   (buffer-file-name buffer))
  ;;                 (org-buffer-list 'files t)
  ;;                 )))

  ;; (defun my-local-org-files-list ()
  ;;   (delq nil
  ;;         (buffer-file-name (current-buffer))
  ;;         )
  ;;   )

  ;; Get my current org file name
  (defun my-current-org-file ()
    (delq nil
          (list
           (buffer-file-name (current-buffer)))
          )
    )

  ;; Refile only in current file
  (defun my-local-org-refile ()
    "Refile only in current org-file"
    (interactive)
    (let ((org-refile-targets
           '((my-current-org-file :maxlevel . 10))
           ))
      (org-refile)
      ))

;;;; Keybindings

;;;;; Global

  (spacemacs/declare-prefix "o" "custom")
  (spacemacs/set-leader-keys
    "od" 'my-insert-current-date
    "on" 'my-insert-note
    "or" 'my-helm-org-rifle-files
    "op" 'sp-splice-sexp-killing-around
    "oa" 'org-agenda-list
    "qq" 'my-safe-exit
    )

;;;;; Local

  (spacemacs/declare-prefix-for-mode 'org-mode "o" "custom")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "oi" 'org-id-get-create
    "od" 'my-org-toggle-tag-drill
    "oL" 'my-org-link-delete-link
    "or" 'org-drill-resume
    "oR" 'org-drill
    "ol" 'my-local-org-refile
    )

  (add-to-list 'load-path "~/.spacemacs.d/lisp/org-protocol-capture-html/")

  (require 'org-protocol-capture-html)
  ;; (setq recentf-max-saved-items 30)

  ;; Just follow those symlinc files
  (setq vc-follow-symlinks t)

  ;; Convert code blocks #+FOO -> #+foo
  (defun my-lower-case-org-keywords ()
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

  ;; NOTE: 2019-05-02: Not completely figured-out a form and this list was not working properly
  ;; ;; Special symbols for org-entities-help
  ;; (nconc org-entities-user
  ;;        (
  ;;         ;; name    LaTeX    mathp HTML   ASCII Latin1 UTF-8
  ;;         '("vdots" "\\vdots{}" t "&x2999" "..." "..." "⁞")
  ;;         ))

  ;; Setting of org-tempo does not work because instead custom strings in expressions seems now accept only hardcoded template strings
  ;; (setq tempo-interactive t)

  ;; (defvar my-org-tempo-tags nil
  ;;   "Tempo tags for Org mode")

  ;; (add-hook 'org-mode-hook '(lambda ()
	;; 		                      (tempo-use-tag-list 'my-org-tempo-tags)
	;; 		                      ))

  ;; (tempo-define-template "my-tempo-template-org-haskell"
  ;;                        '("#+begin_src haskell" n p n "#+end_src" >
  ;;                          )
  ;;                        "<z"
  ;;                        "Insert Haskell template"
  ;;                        'my-org-tempo-tags)

  ;; (tempo-define-template "my-property"
  ;;                        '(":PROPERTIES:" p ":END:" >
  ;;                          )
  ;;                        "<p"
  ;;                        "Insert a property tempate"
  ;;                        'my-org-tempo-tags)

  ;; Read words from heading, match with tag dictionary, try trim 's' from the end and match, and autoset tags
  (defun my-org-auto-tag ()
    (interactive)
    (let ((alltags (append org-tag-persistent-alist org-tag-alist))
          (headline-words (split-string (downcase (org-get-heading t t))))
          )
      (mapcar (lambda (word) (if (assoc word alltags)
                                 (org-toggle-tag word 'on)
                               (
                                if (assoc (string-trim-right word "s") alltags)
                                   (org-toggle-tag (string-trim-right word "s") 'on)
                                   )))
              headline-words))
    )
  (add-hook 'org-capture-before-finalize-hook '(lambda ()
			                      (my-org-auto-tag)
			                      ))
  ;; 2019-05-09: Toggle truncated lines for some prompts to work correctly in Fish shell
  ;; link: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Btools/shell
  (add-hook 'term-mode-hook 'toggle-truncate-lines)


  ;; NOTE: 2019-06-04: Allows to run org-drill on cards with default type and empty body.
  ;;       Seems like not actually working.
  (defun org-drill-entry-status-workaround (oldfun &rest args)
    "Call adviced `org-drill-entry-status' as OLDFUN with ARGS.
Temporarily let `org-entry-empty-p' return nil for empty drill cards
with DRILL_CARD_TYPE nil."
    (let ((oldfun-entry-empty-p (symbol-function 'org-entry-empty-p)))
      (cl-letf (((symbol-function 'org-entry-empty-p)
                 (lambda ()
                   (and (funcall oldfun-entry-empty-p) ;; in principle the old fun
                        ;; with the exception:
                        (null (and
                               (org-drill-entry-p)
                               (null (org-entry-get (point) "DRILL_CARD_TYPE"))
                               (nth 3 (assoc nil org-drill-card-type-alist)))))))) ;; DRILL-EMPTY-P
        (apply oldfun args))))

  (advice-add 'org-drill-entry-status :around #'org-drill-entry-status-workaround)

  ;;;; 2019-06-12: NOTE: Prettify '<<<Radio targets>>>' to be shown as 'Radio targets' when org-descriptive-links set
  ;;;; This is improvement of the code from: Tobias&glmorous: https://emacs.stackexchange.com/questions/19230/how-to-hide-targets
  ;;;; There exists library created from the sample: https://github.com/talwrii/org-hide-targets
  (defcustom org-hidden-links-additional-re "\\(<<<\\)[[:print:]]+?\\(>>>\\)"
    "Regular expression that matches strings where the invisible-property of the sub-matches 1 and 2 is set to org-link."
    :type '(choice (const :tag "Off" nil) regexp)
    :group 'org-link)
  (make-variable-buffer-local 'org-hidden-links-additional-re)

  (defun org-activate-hidden-links-additional (limit)
    "Put invisible-property org-link on strings matching `org-hide-links-additional-re'."
    (if org-hidden-links-additional-re
        (re-search-forward org-hidden-links-additional-re limit t)
      (goto-char limit)
      nil))

  (defun org-hidden-links-hook-function ()
    "Add rule for `org-activate-hidden-links-additional' to `org-font-lock-extra-keywords'.
     You can include this function in `org-font-lock-set-keywords-hook'."
    (add-to-list 'org-font-lock-extra-keywords
                 '(org-activate-hidden-links-additional
                   (1 '(face org-target invisible org-link))
                   (2 '(face org-target invisible org-link)))))

  (add-hook 'org-font-lock-set-keywords-hook 'org-hidden-links-hook-function)

  ;; (add-hook 'haskell-mode-hook 'spacemacs/toggle-highlight-long-lines-on)

  ;;;; NOTE: 2019-08-02: Trying to make literate Haskell work in HIE
  ;; (require 'lsp)
  ;; (require 'lsp-haskell)
  ;; (add-hook 'literate-haskell-mode-hook #'lsp-haskell-enable)
  ;; (add-hook 'literate-haskell-mode-hook #'lsp)

  (add-hook 'haskell-mode-hook #'direnv-update-environment) ;; If direnv configured

  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          ;; (:name "Today"  ; Optionally specify section name
          ;;        :time-grid t  ; Items that appear on the time grid
          ;;        :todo "TODAY")  ; Items that have this TODO keyword
          ;; (:name "Important"
          ;;        ;; Single arguments given alone
          ;;        :tag "bills"
          ;;        :priority "A")
          ;; ;; Set order of multiple groups at once
          ;; (:order-multi (2 (:name "Shopping in town"
          ;;                         ;; Boolean AND group matches items that match all subgroups
          ;;                         :and (:tag "shopping" :tag "@town"))
          ;;                  (:name "Food-related"
          ;;                         ;; Multiple args given in list with implicit OR
          ;;                         :tag ("food" "dinner"))
          ;;                  (:name "Personal"
          ;;                         :habit t
          ;;                         :tag "personal")
          ;;                  (:name "Space-related (non-moon-or-planet-related)"
          ;;                         ;; Regexps match case-insensitively on the entire entry
          ;;                         :and (:regexp ("space" "NASA")
          ;;                                       ;; Boolean NOT also has implicit OR between selectors
          ;;                                       :not (:regexp "moon" :tag "planet")))))
          ;; ;; Groups supply their own section names when none are given
          ;; (:todo "WAITING" :order 8)  ; Set order of this section
          ;; (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
          ;;        ;; Show this group at the end of the agenda (since it has the
          ;;        ;; highest number). If you specified this group last, items
          ;;        ;; with these todo keywords that e.g. have priority A would be
          ;;        ;; displayed in that group instead, because items are grouped
          ;;        ;; out in the order the groups are listed.
          ;;        :order 9)
          ;; (:priority<= "B"
          ;;              ;; Show this section after "Today" and "Important", because
          ;;              ;; their order is unspecified, defaulting to 0. Sections
          ;;              ;; are displayed lowest-number-first.
          ;;              :order 1)
          ;; ;; After the last group, the agenda will display items that didn't
          ;; ;; match any of these groups, with the default order position of 99
          (:name "Flow"
                 :tag "work"
                 :order 1)
          (:name "Habit"
                 :tag "habit"
                 :order 9)
          )
        )

  (setq org-agenda-prefix-format
        '(
          ;; (agenda . " %i %-12:c%?-12t% s")
          ;; (todo . " %i %-12:c")
          ;; (tags . " %i %-12:c")
          ;; (search . " %i %-12:c")
          ;; (agenda  . "  • ")
          ;; (agenda  . "  ")
          ;; (agenda . " %i %-5t%-5s")
          (agenda . "  %i")
          ;; (timeline  . "  % s")
          (todo  . " %i %-12:c")
          (tags  . " %i %-12:c")
          (search . " %i %-12:c")
          )
        )

  (org-super-agenda-mode t)

  ;; (add-hook 'haskell-mode-hook #'structured-haskell-mode)

;;;; Managing code with Outlines using =outshine=

  (add-hook 'outline-minor-mode-hook #'outshine-mode)

  ;; Enables outline-minor-mode for *ALL* programming buffers
  (add-hook 'prog-mode-hook #'outline-minor-mode)
  ;; (add-hook 'nix-mode-hook #'outline-minor-mode)    ;; Seems like not worked

  ;; Narrowing now works within the headline rather than requiring to be on it
  (advice-add 'outshine-narrow-to-subtree :before
              (lambda (&rest args) (unless (outline-on-heading-p t)
                                     (outline-previous-visible-heading 1))))

  (spacemacs/set-leader-keys
    ;; Narrowing
    "nn" 'outshine-narrow-to-subtree
    "nw" 'widen

    ;; Structural edits
    "nj" 'outline-move-subtree-down
    "nk" 'outline-move-subtree-up
    "nh" 'outline-promote
    "nl" 'outline-demote)

  (let ((kmap outline-minor-mode-map))
    (define-key kmap (kbd "M-RET") 'outshine-insert-heading)
    (define-key kmap (kbd "<backtab>") 'outshine-cycle-buffer)

    ;; Evil outline navigation keybindings
    (evil-define-key '(normal visual motion) kmap
      "gh" 'outline-up-heading
      "gj" 'outline-forward-same-level
      "gk" 'outline-backward-same-level
      "gl" 'outline-next-visible-heading
      "gu" 'outline-previous-visible-heading))

  ;; From https://github.com/travisbhartwell/nix-emacs#flycheck
  ;; Flycheck can find executables of checkers that would be only accessible via nix-shell
  ;; (setq flycheck-command-wrapper-function
  ;;       (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
  ;;       flycheck-executable-find
  ;;       (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

;;;; Pretty-fonts

  (add-to-list 'load-path "~/.spacemacs.d/lisp/pretty-fonts/")
  (require 'pretty-fonts)
  ;; (pretty-fonts-set-kwds
  ;;  '((pretty-fonts-fira-font prog-mode-hook org-mode-hook)))

  (defun display/init-pretty-fonts ()
    (use-package pretty-fonts
      :config
      ;; !! This is required to avoid segfault when using emacs as daemon !!
      (spacemacs|do-after-display-system-init
       ;;;; Disabled this, because guy has ridicilous symbols set, and they override my setup.
       ;; (pretty-fonts-add-hook 'prog-mode-hook pretty-fonts-fira-code-alist)
       ;; (pretty-fonts-add-hook 'org-mode-hook  pretty-fonts-fira-code-alist)
       ;; (pretty-fonts-add-hook 'haskell-mode-hook  pretty-fonts-fira-code-alist)

       (pretty-fonts-set-fontsets-for-fira-code)
       ;;;; This symbols are not supported on my setup
       ;; (pretty-fonts-set-fontsets
       ;;  '(;; All-the-icons fontsets
       ;;    ("fontawesome"
       ;;     ;;                         
       ;;     #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

       ;;    ("all-the-icons"
       ;;     ;;    
       ;;     #xe907 #xe928)

       ;;    ("github-octicons"
       ;;     ;;                               
       ;;     #xf091 #xf059 #xf076 #xf075 #xe192  #xf016 #xf071)

       ;;    ("material icons"
       ;;     ;;              
       ;;     #xe871 #xe918 #xe3e7  #xe5da
       ;;     ;;              
       ;;     #xe3d0 #xe3d1 #xe3d2 #xe3d4)
       ;;    )
       ;;  )
       )
      )
    )

  ;;;; This is an empty call to notexistent func
  ;; (display/init-pretty-fonts)

;;;; Mathematical symbols
  ;; Doing setup from: http://www.modernemacs.com/post/prettify-mode/

  ;; Pretty mode
  (require 'pretty-mode)
  ;; (global-pretty-mode t)

  (pretty-deactivate-groups
   '(
     :equality
     :ordering
     :ordering-double
     :ordering-triple
     :arrows
     :arrows-twoheaded
     :punctuation
     :logic
     :sets
     )
   )

  (pretty-activate-groups
   '(
     ;; :sub-and-superscripts
     ;; :greek
     ;; :arithmetic-nary
     )
   )

  ;; (global-prettify-symbols-mode 1)

  (add-hook
   'python-mode-hook
   (lambda ()
     (mapc (lambda (pair) (push pair prettify-symbols-alist))
           '(;; Syntax
             ("def" .      #x2131)
             ("not" .      #x2757)
             ("in" .       #x2208)
             ("not in" .   #x2209)
             ("return" .   #x27fc)
             ("yield" .    #x27fb)
             ("for" .      #x2200)
             ;; Base Types
             ("int" .      #x2124)
             ("float" .    #x211d)
             ("str" .      #x1d54a)
             ("True" .     #x1d54b)
             ("False" .    #x1d53d)
             ;; Mypy
             ("Dict" .     #x1d507)
             ("List" .     #x2112)
             ("Tuple" .    #x2a02)
             ("Set" .      #x2126)
             ("Iterable" . #x1d50a)
             ("Any" .      #x2754)
             ("Union" .    #x22c3)))))

  (add-hook
   'haskell-mode-hook
   #'prettify-symbols-mode)
  (add-hook
   'haskell-mode-hook
   (lambda ()
     (mapc (lambda (pair) (push pair prettify-symbols-alist))
           '(
             ;; Syntax
             ;; 𝈀

             ("--"          . ?\ⅈ)

             ;; Single letters
             ("a"           . ?\𝑎)
             ("b"           . ?\𝑏)
             ("c"           . ?\𝑐)
             ("d"           . ?\𝑑)
             ("e"           . ?\𝘦) ;𝑒
             ("f"           . ?\𝑓)
             ("g"           . ?\𝑔)
             ("h"           . ?\ℎ)
             ("i"           . ?\𝑖)
             ("j"           . ?\𝑗)
             ("k"           . ?\𝑘)
             ("l"           . ?\𝑙)
             ("m"           . ?\𝑚)
             ("n"           . ?\𝑛)
             ("o"           . ?\𝑜)
             ("p"           . ?\𝑝)
             ("q"           . ?\𝑞)
             ("r"           . ?\𝑟)
             ("s"           . ?\𝑠)
             ("t"           . ?\𝑡)
             ("u"           . ?\𝑢)
             ("v"           . ?\𝑣)
             ("w"           . ?\𝑤)
             ("x"           . ?\𝑥)
             ("y"           . ?\𝑦)
             ("z"           . ?\𝑧)
             (" . "         . ?\●) ;;∘🞄
             (".."          . ?\‥)
             ("!!"          . ?\‼)
             ("[|"          . ?\⟦)
             ("|]"          . ?\⟧)
             ("where"       . ?\🕮) ;;📖🕮
             ("let"         . ?\📎) ;;📙📘📗📚
             ("Monoid"      . ?\🕀) ;;⊕𐃏𐀏⫘
             ("mappend"     . ?\🕀)
             ("<>"          . ?\🕀)
             ("Functor"     . ?\⮝) ;;╒ ⚯↗➚𝑭⇗
             ("fmap"        . ?\⮝) ;;╒ ⚯
             ("<$>"         . ?\⮞)
             ("Applicative" . ?\🅐) ;;⯮⟴💠⨂⇶
             ("<*>"         . ?\🅐)
             ;; ("ap"          . ?\⯮)
             ("++"          . ?\‡) ;;ⵜ
             ("return"      . ?\𝝶)
             ("pure"        . ?\𝝶)
             ("join"        . ?\𝝻)
             (">>="         . ?\⮊)  ;;🎯༻⍟ ➺ ➻ ➼ ➽⁂◉⦿ↂඏߘᔕ⌾⭖◎»⏩൰⭃
             ("=<<"         . ?\⮈)
             (">=>"         . ?\⫘)  ;;🞉⏛⚯
             ("<-"          . ?\◄)  ;;⍟ ➺ ➻ ➼ ➽⁂◉⦿
             ("$"           . ?\∫)  ;;Π◅
             ("()"          . ?\⬤)
             ("isUnique"    . ?\!)
             ("Monad"       . ?\🤀)
             ("=>"          . ?\🢥)  ;;⇒
             ("->"          . ?\🡪)  ;;→
             ("::"          . ?\∷)
             ("=="          . ?\≡)
             ("||"          . ?\∨)
             ("&&"          . ?\∧)
             ("*"           . ?\×)
             ("foldl"       . ?\⮒)
             ("foldr"       . ?\⮓)
             ("sum"         . ?\∑)
             ("set"         . ?\≬)

             ;; Data types

             ("Integer"     . ?\ℤ)
             ("Float"       . ?\ℝ)
             ("Rational"    . ?\ℚ)
             ("String"      . ?\𝕊)
             ("Bool"        . ?\𝔹)
             ("True"        . ?\✔)
             ("False"       . ?\✘)
             ("Complex"     . ?\ℂ)
             ("Arbitrary"   . ?\🎲)
             ("arbitrary"   . ?\🎲)
             ("Maybe"       . ?\?)
             ("Just"        . ?\∃)
             ("Nothing"     . ?\∄)
             ("Void"        . ?\⃝)
             ("undefined"   . ?\┻)

             ;; Operators

             ("<<"          . ?\≪)
             (">>"          . ?\≫)
             ("<="          . ?\≤)
             (">="          . ?\≥)
             ("<<<"         . ?\⋘)
             (">>>"         . ?\⋙)
             ("/="          . ?\≠)
             ("and"         . ?\⋀)
             ("or"          . ?\⋁)

             ;; Key words

             ("\`elem\`"    . ?\∈)
             ("elem"        . ?\∈)
             ("notElem"     . ?\∉)
             ("intersect"   . ?\∩)
             ("intersection" . ?\∩)
             ("union"       . ?\∪)
             ("case"        . ?\⁇)
             ("of"          . ?\:)
             ("do"          . ?\🕃)
             ("not "        . ?\¬)
             ("in"          . ?\∈) ;;∈∊
             ("not in"      . ?\∉)
             ("\\"          . ?\𝝺)
             ("IO"          . ?\☯) ;;☯🔯
             ("map"         . ?\⮚)
             ("if"          . ?\⛬) ;;𝕻∵⛬T
             ("then"        . ?\⊢)
             ("else"        . ?\⊬)
             ("forall"      . ?\∀)
             ("exists"      . ?\Ǝ) ;;ヨⴺƎᗱョᴲ∋ⱻॻՅ∍ᙐ℈ᗲЄє
             ("instance"    . ?\𝐈)
             ("instance"    . ?\𝐈)
             ("import"      . ?\i)
             ("module"      . ?\m)
             ("LANGUAGE"    . ?\ℒ)  ;ム
             ("deriving"    . ?\ට)  ;ᘐ ච ᘑ
             ("newtype"     . ?\ﬦ)
             ("Right"       . ?\𝕽)
             ("Left"        . ?\𝕷)
             ("*"           . ?\★)

             ))))

;;;; Line number

  (setq-default
   dotspacemacs-line-numbers
   '(
     :relative t

     :size-limit-kb 1000
     )
   )

  (setq linum-format "%4d ")

;;;; Saving Emacs Desktop

  ;; (setq  desktop-path '(("~/.spacemacs.d/desktops/")))
  ;; (desktop-save-mode 1)

;;;; Custom Org-mode priorities

  ;;;;; Org-priority regexp

  (setq org-priority-regexp ".*?\\(\\[\\([0-9][0-9]\\)\\] ?\\)")

  (defun org-font-lock-add-priority-faces (limit)
    "Add the special priority faces."
    ;; NOTE: 2019-10-26: Here regexp was: "^\\*+ .*?\\(\\[#\\(.\\)\\]\\)"
    ;; was in the place of org-priority-regexp, which should supply data here.
    ;; Remove note after battletesting went good.
    (while (re-search-forward org-priority-regexp limit t)
      (add-text-properties
       (match-beginning 1) (match-end 1)
       (list 'face (org-get-priority-face (string-to-char (match-string 2)))
             'font-lock-fontified t))))

  ;;;;; Org-priority range declaration

  (setq org-highest-priority 99)
  (setq org-default-priority 49)
  (setq org-lowest-priority 00)

  ;;;;; Overload default priority function with my own

  (defun org-get-priority (s)
    "My custom overload to find priority cookie and return priority."
    (save-match-data
      (* 1000 (if (string-match org-priority-regexp s)
          (string-to-number (match-string 2 s))
        org-default-priority
        ))))

;;;; Misc init

  (setq copy-as-format-default "github")

  (setq dotspacemacs-scratch-mode 'lisp-interaction-mode)

  (setq haskell-font-lock-symbols 't)  ;; Enable font ligatures

  ;;;;; For being safe that opening a PDF uses pdf-tools and not DocView - enable PDF tools

  (pdf-tools-install)

  ;;;;; Highlight LaTeX blocks

  (setq org-highlight-latex-and-related '(latex))

  ;;;;; Global short key for NEXT capture template
  (define-key global-map (kbd "<f5>")
    (lambda () (interactive) (org-capture nil "n")))

  )


;;; Emacs-custom-settings

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
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
    (pdf-tools elfeed-org elfeed-goodies ace-jump-mode noflet elfeed dap-mode bui tree-mode org-drill persist telega copy-as-format selectric-mode emojify emoji-cheat-sheet-plus company-emoji pretty-mode ox-twbs ox-gfm org-sticky-header org-re-reveal outshine outorg org-super-agenda ts zeal-at-point yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package treemacs-projectile treemacs-evil toc-org tide tagedit systemd symon symbol-overlay sunshine string-inflection sql-indent spotify spaceline-all-the-icons smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters pytest pyenv-mode py-isort pug-mode prettier-js popwin plantuml-mode pippel pipenv pip-requirements persp-mode pcre2el password-generator paradox pandoc-mode ox-pandoc overseer orgit org-projectile org-present org-pomodoro org-mime org-journal org-download org-cliplink org-bullets org-brain open-junk-file nodejs-repl nix-sandbox nix-mode nameless multi-term move-text mmm-mode markdown-toc magit-svn magit-gitflow macrostep lsp-ui lsp-treemacs lsp-python-ms lsp-haskell lorem-ipsum livid-mode live-py-mode link-hint json-navigator js2-refactor js-doc jinja2-mode intero insert-shebang indent-guide importmagic impatient-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-spotify-plus helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-nixos-options helm-mode-manager helm-make helm-lsp helm-hoogle helm-gitignore helm-git-grep helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy font-lock+ flyspell-popup flyspell-correct-helm flycheck-pos-tip flycheck-package flycheck-haskell flycheck-bashate flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav editorconfig dumb-jump dotenv-mode doom-modeline dockerfile-mode docker direnv diminish devdocs define-word dante cython-mode csv-mode company-web company-terraform company-tern company-statistics company-shell company-nixos-options company-lsp company-ghci company-ghc company-cabal company-ansible company-anaconda column-enforce-mode color-identifiers-mode cmm-mode clean-aindent-mode centered-cursor-mode blacken auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile attrap ansible-doc ansible aggressive-indent ace-link ace-jump-helm-line ac-ispell)))
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black"))))
 '(font-lock-comment-delimiter-face ((t (:background "black" :foreground "grey16"))))
 '(font-lock-comment-face ((t (:background "black" :foreground "Navajowhite4"))))
 '(font-lock-string-face ((t (:foreground "#356a00"))))
 '(h1-line ((t (:background "black"))))
 '(haskell-constructor-face ((t (:foreground "#ffa600"))))
 '(haskell-definition-face ((t (:foreground "#ca8300"))))
 '(haskell-keyword-face ((t (:foreground "#00dede"))))
 '(haskell-operator-face ((t (:foreground "#00dede"))))
 '(haskell-type-face ((t (:foreground "#7a37ff"))))
 '(helm-buffer-directory ((t (:background "black"))))
 '(helm-buffer-file ((t (:background "black"))))
 '(helm-ff-directory ((t (:background "black"))))
 '(helm-ff-file ((t (:background "black"))))
 '(org-block ((t (:background "#000030"))))
 '(org-block-begin-line ((t (:background "#000030" :foreground "#000030"))))
 '(org-block-end-line ((t (:background "#000030" :foreground "#000030"))))
 '(org-property-value ((t (:foreground "gray18"))) t)
 '(org-special-keyword ((t (:foreground "grey18"))))
 '(org-tag ((t (:foreground "gray18" :weight bold :height 0.8)))))
)
