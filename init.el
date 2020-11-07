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

     (syntax-checking :variables
                      flycheck-proselint-executable "proselint"
                      flycheck-textlint-executable "textlint"
                      ;; syntax-checking-enable-by-default nil ; 2020-10-27: NOTE: disables flycheck spellchecking process by default
                      )

     javascript
     shell-scripts

     csv
     html
     markdown
     yaml

     ;; sql

     systemd
     (terraform :variables terraform-auto-format-on-save t)
     ;; tmux

     ;; nixos

     colors    ; Too extreme fancy coloring

;;;; Org

     (org
      :variables
      org-directory "~/org/"
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
        ;; To see color names: func `list-colors-display`
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
      org-pomodoro-long-break-frequency 2
      org-pomodoro-ask-upon-killing t
      org-pomodoro-length 25
      org-pomodoro-format "%s"
      ;;org-pomodoro-time-format "%.2m:%.2s"
      my-org-refile-additional-targets
      '(
        "~/org/Notes.org"
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
      org-outline-path-complete-in-steps nil ; Refile in a single go
      org-refile-use-outline-path 'file ; Show full paths for refiling, value 'file' includes the files themself.
      org-habit-graph-column 130
      org-habit-preceding-days 56

      ;; Turn off all org autoindentation completely
      org-startup-indented nil
      ;; Turn off manual indentation completely
      org-adapt-indentation nil
      org-brain-path org-directory

      org-log-into-drawer t    ; if t log into the :LOGBOOK: drawer

      org-agenda-time-grid nil

      ;; org-agenda-time-grid
      ;; (quote
      ;;  (
      ;;   (daily today require-timed)
      ;;   (400 500 600 700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000)
      ;;   "      "
      ;;   "----------------"
      ;;   )
      ;;  )

      ;; ;; Taken from https://github.com/jwiegley/dot-emacs/blob/master/org-settings.el
      ;; org-agenda-sorting-strategy '((agenda habit-down time-up todo-state-up priority-down)
      ;;                               (todo priority-down category-keep)
      ;;                               (tags priority-down category-keep)
      ;;                               (search category-keep))

;;;;; Tags

      ;; List of always declared tags, function my-org-auto-tag matches and autoadds occuring words from it

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
        ("const")
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
        ("maybe")
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

;;;;; Misc

      org-cycle-separator-lines 0 ;; 0 - turns off empty lines between collapsed headers
      org-catch-invisible-edits 'smart  ;; Determins behaviour when you do invisible edits in (...)

      ;; org-bullets-bullet-list '(" ")    ;; no bullets, needs org-bullets package
      org-ellipsis "…"    ;; ⤵ folding symbol
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
      org-edit-src-content-indentation 0    ;; Disable the freaking autoindentation of Babel code blocks that driven me nuts
      org-agenda-window-setup 'only-window    ;; Delete all other windows, show agenda full frame

;;;;; Latex setup

      org-latex-pdf-process
      ;; `-shell-escape' is jailbreak to permit calls of external utils, enabled for `minted' Pygments code highlighting
      '(
        "%latex --shell-escape -etex --interaction=nonstopmode --output-directory=%o %f"
        "%bib %b"
        "%latex --shell-escape -etex --interaction=nonstopmode --output-directory=%o %f"
        "%latex --shell-escape -etex --interaction=nonstopmode --output-directory=%o %f"
        "%latex --shell-escape -etex --interaction=nonstopmode --output-directory=%o %f"
        "%latex --shell-escape -etex --interaction=nonstopmode --output-directory=%o %f"
        )
      org-latex-listings 'minted    ;; Use Python Pygments highlighter. Needs `org-latex-pdf-process' with `-shell-escape'
      org-latex-minted-options '(("breaklines" "true"))    ;; Allow exporting with word-wrap in code blocks

      ;; Works perfectly, generates fast, shows high-class, needs Emacs to support svg display
      org-preview-latex-default-process 'luadvisvgm  ;; luapdf2svg, lua2svg, imagemagick

      ;; Setting hyperref in LaTeX overrides this template, and has a side effect - adds epty space to the previews
      org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n colorlinks=true,\n linkcolor=blue,\n urlcolor=violet,\n filecolor=violet,\n bookmarksdepth=10}\n"

      org-latex-compiler "lualatex"

      org-latex-default-packages-alist
      '(
        ("AUTO" "inputenc" t ("pdflatex"))
        ("T1" "fontenc" t ("pdflatex"))
        ("" "graphicx" t)
        ("" "grffile" t ("pdflatex"))
        ("" "grffile" t ("xelatex"))
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "hyperref" nil)
        )

      org-latex-packages-alist    ; 2020-03-14: NOTE: On changing the org env, dvisvgm needs recompile previews and it fails due to some error in the latex environment
      '(
        ("" "tabularx" t)
        ("" "tabu" t)
        ("" "booktabs" t)
        )

      org-latex-classes                 ; 2020-03-14: NOTE: Expanding the Book subsections in depth
      '(
        ("article" "\\documentclass[11pt]{article}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ("report" "\\documentclass[11pt]{report}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("book" "\\documentclass[11pt]{book}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
         ("\\subsubparagraph{%s}" . "\\subsubparagraph*{%s}")
         ("\\subsubsubparagraph{%s}" . "\\subsubsubparagraph*{%s}")
         ("\\subsubsubsubparagraph{%s}" . "\\subsubsubsubparagraph*{%s}")
         ("\\subsubsubsubsubparagraph{%s}" . "\\subsubsubsubsubparagraph*{%s}")
         ("\\subsubsubsubsubsubparagraph{%s}" . "\\subsubsubsubsubsubparagraph*{%s}")
         ("\\subsubsubsubsubsubsubparagraph{%s}" . "\\subsubsubsubsubsubsubparagraph*{%s}")
         )

       )

      )

;;;; LSP

     (lsp
      ;; :variables
        ;; lsp-haskell-server-wrapper-function my-default-nix-wrapper
        ;; lsp-haskell-server-path 'ghcide
        ;; lsp-haskell-server-path 'haskell-language-server-wrapper
        ;; lsp-haskell-server-args "-d -l /tmp/hls.log"
      )

;;;; Haskell

     (haskell
      ;; :variables
        ;; haskell-process-wrapper-function my-default-nix-wrapper
        ;; haskell-process-type 'cabal-repl ; 2020-10-10: NOTE: cabal-new-repl is obsolete in favour of cabal-repl
        ;; haskell-completion-backend 'lsp
        ;; ;; haskell-completion-backend 'dante
        ;; haskell-enable-hindent 't
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
            ("~/" . 0)
            ("~/.mozilla/" . 0)
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
            ("Name" 25 magit-repolist-column-ident ())
            ("S" 1 my-magit-repolist-column-stashes ())
            ("G" 1 magit-repolist-column-dirty ())
            ("U" 25 my-magit-repolist-column-upstream-state ())
            ("Path" 99 magit-repolist-column-path ())
            )

          magit-repolist-column-flag-alist
          '(
            (magit-staged-files . "S")
            (magit-unstaged-files . "U")
            (magit-untracked-files . "F")
            )

          )


     auto-completion    ;; Completion seems not of quality and unrelevant for now

     python
     typescript
     plantuml

;;;; Tools
     ansible
     (dash
      :variables ; Dash macOS & Zeal offline documentation tools
      helm-dash-docset-newpath "~/.local/share/Zeal/Zeal/docsets"
      )

     docker
     pandoc

;;;; Shell

     (
      shell
      :variables
      shell-default-shell 'ansi-term  ;; Alternatives: 'eshell
      shell-default-term-shell "fish"
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
      :variables

      theming-modifications
      '(
        (
         spacemacs-dark

         (
          org-tag
          :foreground "gray18"
          :weight bold
          :height 0.8)

         (
          org-property-value
          :foreground "gray18"
          )
         (
          org-drawer
          :foreground "gray18"
          )

         (
          default
          :background
          "black"
          )
         (
          hl-line
          :background
          "grey4"
          )
         (
          helm-selection-line
          :background
          "grey5"
          )
         (
          line-number
          :foreground
          "grey10"
          :background
          "black"
          )
         (
          line-number-current-line
          :foreground
          "grey11"
          :background
          "grey4"
          )
         (
          border
          :foreground
          "black"
          :background
          "black"
          )
         (
          fringe
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
          )
         (haskell-operator-face
          :foreground
          "#00dede"
          )
         (font-lock-string-face
          :foreground
          "#356a00"
          )
         (elfeed-search-title-face
          :foreground
          "gray16"
          )
         (elfeed-search-tag-face
          :foreground
          "gray20"
          )
         (elfeed-search-feed-face
          :foreground
          "gray16"
          )
         (elfeed-search-date-face
          :foreground
          "black"
          )
         (mmm-default-submode-face
          :background
          "black"
          )
         )
        )

      )

;;;; WakaTime

     (wakatime
      :variables
      wakatime-api-key  "b5292c00-f691-4070-9a8c-2a3b61e6e360"
      wakatime-cli-path "wakatime"
      )

;;;; Elfeed

     (elfeed
      :variables
      rmh-elfeed-org-files (list (concat org-directory "elfeed.org"))
      elfeed-db-directory "~/.cache/elfeed/"
      elfeed-initial-tags '(unread)
      ;; elfeed-enable-web-interface t
      ;; httpd-port 8081
      ;; url-queue-timeout 30
      ;; elfeed-enable-goodies nil
      elfeed-goodies/powerline-default-separator 'wave
      elfeed-show-entry-switch 'switch-to-buffer
      )

;;;; PDF

     (pdf
      :variables
      pdf-view-display-size 'fit-page    ;; fit-page by default
      pdf-annot-activate-created-annotations t    ;; Automatically start annotating what is highlighted
      pdf-view-resize-factor 1.1    ;; Grain of the zooming step
;;;;; PDF view setup. Make PDF midnight black, coloring here since it recets setting of the layer
      pdf-view-midnight-colors '("#ffffff" . "#000000" )
      )

;;;; Other layers

     unicode-fonts

     github

     copy-as-format

     speed-reading

     ;; eww

     helpful    ;; Better help buffers

     (typography
      :variables
      typography-enable-typographic-editing nil
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
     ;; nix-sandbox                        ; 2020-10-27: NOTE: trying to fix HLS error, this is a pie in the sky
     haskell-snippets
     org-super-agenda
     shm ;; Structured Haskell mode
     outshine ;; For managing code with Outlines
     pretty-mode
     telega ;; Emacs Telega.el client TODO: Wait when https://github.com/syl20bnr/spacemacs/issues/12800 solved
     org-drill ;; Moved-out of org-mode at 9.3
     yequake    ;; Drop-down Quake-life frame
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
     "Inconsolata"
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
   dotspacemacs-line-numbers nil

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


  (custom-set-variables
   '(org-capture-templates
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

       ("D" "Haskell Draft" entry
        (file+headline ,(concat org-directory "/haskell/README.org") "Draft")
        "* %?"
        :empty-lines-before 1
        :empty-lines-after 1
        )

       ("w" "Word entry" entry
        (file ,(concat org-directory "/word/word.org"))
        "* %^{phrase}\n%?"
        :empty-lines-before 1
        :empty-lines-after 1
        )

       ("t" "TODO:" entry
        (id 8ec8520c-0d07-4d02-9700-f9f204df91b8)
        "* TODO: %^{Name} %?"
        :empty-lines-before 1
        :empty-lines-after 1
        )

       ("n" "Note" entry
        (file+headline ,(concat org-directory "/Notes.org") "Inbox")
        "* %^{Name}\n\n%?\n"
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
        )

       ("b" "Brain: add at the end" plain
        (function org-brain-goto-end)
        "* %i%?" :empty-lines 1
        :empty-lines-before 1
        :empty-lines-after 1
        )

       ("N" "NEXT:" entry
        (id ebc243d3-f040-49a0-940d-fbfd16b46edd)
        "* NEXT: %^{Name}\nSCHT: %t\n\n%?"
        :empty-lines-before 1
        :empty-lines-after 1
        )

       )
     )
   )

  ;; Enable time in the bar
  ;;(setq display-time-24hr-format t)
  (display-time-mode t)

  ;; 2019-02-20: Maybe TMP fix for '<s[TAB]' - when there PR closes the https://github.com/syl20bnr/spacemacs/issues/11798
  (require 'org-tempo)

  ;; Trigger autosave before quitting org-agenda
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

;;;; My functions

;;;;; my-insert-current-date

  (defun my-insert-current-date ()
    "Insert ISO date"
    (interactive)
    (insert (format-time-string "%Y-%m-%d"))
    )

;;;;; my-insert-note

  (defun my-insert-note ()
    "Insert a NOTE comment"
    (interactive)
    (progn
      (comment-dwim ())
      (insert (concat " " (format-time-string "%Y-%m-%d") ": NOTE: " )
              )
      (evil-append-line 0)
      )
    )

;;;;; my-helm-org-rifle-files

  (defun my-helm-org-rifle-files ()
    "Find the org entry by matching content"
    (interactive)
    (helm-org-rifle-files (append my-org-refile-additional-targets org-agenda-files))
    )

;;;;; my-org-toggle-tag-drill

  (defun my-org-toggle-tag-drill ()
    "Toggle :drill: tag"
    (interactive)
    (org-toggle-tag "drill")
    )

;;;;; my-safe-exit

  (defun my-safe-exit ()
    "Saving all files on exit"
    (interactive)
    (save-buffers-kill-terminal t)
    )

;;;;; my-archive-when-done

  ;; TODO: not used
  (defun my-archive-when-done ()
    "Archive current entry if it is marked as DONE (see `org-done-keywords')."
    (when (org-entry-is-done-p)
      (org-archive-subtree-default)))

;;;;; my-org-link-delete-link

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

;;;;; my-async-shell-command

  ;;;; Execute async shell operation without showing buffer result

  (defun my-async-shell-command
      (command)
    (progn
      (async-shell-command command)
      ;; Close buffer with force
      (evil-normal-state)
      (evil-normal-state)
      (bury-buffer "\*Async Shell Command\*")
      (spacemacs/delete-window)
      )
    )

;;;;; my-curren-org-file

  ;; Get my current org file name
  (defun my-current-org-file ()
    (delq nil
          (list
           (buffer-file-name (current-buffer)))
          )
    )

;;;;; my-local-org-refile

  ;; Refile only in current file
  (defun my-local-org-refile ()
    "Refile only in current org-file"
    (interactive)
    (let ((org-refile-targets
           '((my-current-org-file :maxlevel . 10))
           ))
      (org-refile)
      ))

;;;;; my-magit-repolist-column-upstream-state

  ;; Indicating upstream state differently
  (defun my-magit-repolist-column-upstream-state (_id)
    "Insert chars marking state of the current branch to upstream."
    (--when-let (magit-get-upstream-branch)
      (let ((upstream-status (magit-rev-diff-count "HEAD" (magit-get-upstream-branch)))
            )
        (magit--propertize-face
         (if (equal upstream-status '(0 0))
             " "
           (if (equal (car upstream-status) 0)
               "↓"
             (if (equal (cadr upstream-status) 0)
                 "↑"
               "⥯" ;; ⇅
               )
             )
           ) 'bold
         )
        )
      )
    )

;;;;; my-magit-repolist-column-stashes

  ;; Indicate stash state differently

  (defun my-magit-repolist-column-stashes (_id)
    "Insert chars of stashes."
    (let ((n (length (magit-list-stashes))))
      (magit--propertize-face
       (if (eq n '0)
           " "
         "✺"
         ) 'bold
       )
      )
    )

;;;;; my-org-agenda-day-list

  (defun my-org-agenda-day-list ()
    "Open agenda the way I want"
    (interactive)
    (org-agenda-list () () 1)
    (org-agenda-redo)
    )

;;;;; my-revert-buffer-force

  (defun my-revert-buffer-force ()
    "Silently force revert buffer."
    (interactive)
    (revert-buffer :noconfirm :ignore-auto)
    )

;;;;; my-local-org-haskell-book-publish

  (defun my-local-org-haskell-book-publish ()
    "Export current buffer in HTML, then in PDF"
    (interactive)
    (progn
      (save-buffer)
      (shell-command "fish -c 'book-prepare'")
      (sit-for 1)
      (my-revert-buffer-force)
      (org-html-export-to-html)
      (org-latex-export-to-pdf)
        ;; asynchronously & silent
      (my-async-shell-command "fish -c 'book-publish'")
      )
    )

;;;;; my-get-file-as-string

  (defun my-get-file-as-string (filePath)
    "Return filePath's file content."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))

;;;;; my-get-file-char-range-as-string

  (defun my-get-file-char-range-as-string (filePath beginChar endChar)
    "Return filePath's file content beggining from char (starts with 0) and ending on char."
    (with-temp-buffer
      (insert-file-contents filePath nil beginChar endChar)
      (buffer-string)))

;;;;; my-mantra

  (defun my-mantra ()
    "Mantra"
    (interactive)
    (find-file (concat org-directory "/mantra/mantra-en.org"))
    )

;;;;; my-default-nix-wrapper

  ;; Seems as a mandatory Nix env creation, see commented code for the alternative identity allowing version
  (defun my-default-nix-wrapper ()
    "Wraps the command and its args into the Nix wrapper, uses nix-sandbox pkg from nix-emacs repo"
    (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
    )
  ;; default-nix-wrapper (
  ;;   lambda (args) (
  ;;     append (
  ;;       append (list "nix-shell" "-I" "." "--command") (list (mapconcat 'identity args " "))
  ;;         ) (
  ;;       list (nix-current-sandbox)
  ;;       )
  ;;   )
  ;; )

;;;; Keybindings

;;;;; Global

  (spacemacs/declare-prefix "o" "custom")
  (spacemacs/set-leader-keys
    "od" 'my-insert-current-date
    "on" 'my-insert-note
    "or" 'my-helm-org-rifle-files
    "op" 'sp-splice-sexp-killing-around
    "oa" 'my-org-agenda-day-list
    "om" 'my-mantra
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
    "oP" 'my-local-org-haskell-book-publish
    )

  (add-to-list 'load-path "~/.spacemacs.d/lisp/org-protocol-capture-html/")

  (require 'org-protocol-capture-html)
  ;; (setq recentf-max-saved-items 30)

  ;; Just follow those symlink files
  (custom-set-variables
   '(vc-follow-symlinks t)
   )

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

  (add-hook 'org-font-lock-set-keywords-hook #'org-hidden-links-hook-function)

  ;; (add-hook 'haskell-mode-hook 'spacemacs/toggle-highlight-long-lines-on) ; 2020-10-15: NOTE: Recieved "This toggle is not supported with major mode: %S"

  (add-hook 'haskell-mode-hook #'direnv-update-environment) ;; If direnv configured

  (custom-set-variables
   '(
     org-super-agenda-groups
     '(;; Each group has an implicit boolean OR operator between its selectors.
       (:name "Maybe"
              :tag "maybe"
              :order 3)
       (:name "Main"
              :tag "work"
              :order 1)
       (:name "Habit"
              :tag "habit"
              :order 2)
       (:name "Const"
              :tag "const"
              :order 9)
       )
     )
   )

  (custom-set-variables
   '(
     org-agenda-prefix-format
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
   )

  (org-super-agenda-mode t)

;;;; Managing code with Outlines using =outshine=

  (add-hook 'outline-minor-mode-hook #'outshine-mode)

  ;; Enables outline-minor-mode for *ALL* programming buffers
  (add-hook 'prog-mode-hook #'outline-minor-mode)
  ;; (add-hook 'haskell-mode-hook #'outline-minor-mode)

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

  (defun display/init-pretty-fonts ()
    (use-package pretty-fonts
      :config
      ;; !! This is required to avoid segfault when using emacs as daemon !!
      (spacemacs|do-after-display-system-init

       (pretty-fonts-set-fontsets-for-fira-code)
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

;;;;; Python symbols

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

;;;;; Haskell symbols

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
             (" . "         . ?\◅) ;;●∘🞄
             (".."          . ?\‥)
             ("!!"          . ?\‼)
             ("[|"          . ?\⟦)
             ("|]"          . ?\⟧)
             ("where"       . ?\🕮) ;;📖🕮
             ("let"         . ?\📎) ;;📙📘📗📚
             ("Monoid"      . ?\⭙) ;;⊕𐃏𐀏⫘ ⦻ꕕ⭙⛒⨂
             ("mappend"     . ?\⨯)
             ("<>"          . ?\⨯) ;; ×⨯
             ("mempty"      . ?\𝟣) ;; ⥠
             ("id"          . ?\𝟙) ;; 𝟭𝟙
             ("Functor"     . ?\⮝) ;;╒ ⚯↗➚𝑭⇗
             ("fmap"        . ?\⮝) ;;╒ ⚯
             ("<$>"         . ?\⮞)
             ("Applicative" . ?\🅐) ;;⯮⟴💠⨂⇶⌾☉⎋𐌈🜨⚹❋✴ꔮ⎊
             ("<*>"         . ?\🅐)
             ;; ("ap"          . ?\⯮)
             ("Monad"       . ?\🎯) ;; 🤀ʘ◉☉⨀𐍈
             ("return"      . ?\𝝶)
             ("pure"        . ?\𝝶)
             ("join"        . ?\𝝻)
             (">>="         . ?\→)  ;;⮊🎯༻⍟ ➺ ➻ ➼ ➽⁂◉⦿ↂඏߘᔕ⌾⭖◎»⏩൰⭃Ꮻ𝕠
             ("=<<"         . ?\◄) ;; ⮈
             (">=>"         . ?\⫘)  ;;🞉⏛⚯
             ("<-"          . ?\←)  ;;⟵↢↩↵↤⟻⍟ ➺ ➻ ➼ ➽⁂◉⦿←◄
             ("++"          . ?\‡) ;;ⵜ
             ("$"           . ?\∫)  ;;Π
             ("()"          . ?\⬤)
             ("isUnique"    . ?\!)
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
             ("data"        . ?\𝗗)
             ("newtype"     . ?\𝗪)
             ("type"        . ?\𝗔)
             ("Right"       . ?\𝕽)
             ("Left"        . ?\𝕷)
             ;; ("*"           . ?\★)

             ))))

;;;; Line number

  (custom-set-variables
   '(display-line-numbers-width 5)
   '(display-line-numbers-grow-only t)    ;; Do not shrink the number line width
   '(display-line-numbers-width-start t)    ;; Calculate width of the column from the start
   )
  ;; (global-display-line-numbers-mode nil)

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

;;;;; Remove org comments from org document for use with export hook
;; https://emacs.stackexchange.com/questions/22574/orgmode-export-how-to-prevent-a-new-line-for-comment-lines
  (defun delete-org-comments (backend)
    (loop for comment in (reverse (org-element-map (org-element-parse-buffer)
                                      'comment 'identity))
          do
          (setf (buffer-substring (org-element-property :begin comment)
                                  (org-element-property :end comment))
                "")))

  ;; add to export hook
  (add-hook 'org-export-before-processing-hook #'delete-org-comments)

;;;;; Global short key for NEXT capture template

  (define-key global-map (kbd "<f5>")
    (lambda () (interactive) (org-capture nil "N")))

;;;;; Add paths to search private layers in

  ;; TODO: This does not work, I have symlinks into: ~/.emacs.d/private/
  (add-to-list 'dotspacemacs-configuration-layer-path "~/.spacemacs.d/lisp/private-layers/")
  (add-to-list 'dotspacemacs-configuration-layer-path "~/.spacemacs.d/lisp/private-layers/eww/")

;;;;; Use eww as default Emacs browser

  ;; (setq browse-url-browser-function 'eww-browse-url)

;;;;; FIXME: ('B' does not work): In elfeed 'b' is for eww, 'B' is for default browser

  ;; browse article in gui browser instead of eww
  (defun bjm/elfeed-show-visit-gui ()
    "Wrapper for elfeed-show-visit to use gui browser instead of eww"
    (interactive)
    (let ((browse-url-generic-program "firefox"))
      (elfeed-show-visit t)))

  ;; Trying to map Firefox browsing on B
  ;; (define-key elfeed-show-mode-map (kbd "B") 'bjm/elfeed-show-visit-gui)

  ;; (setq elfeed-show-entry-switch 'switch-to-buffer)
  ;; (setq elfeed-search-trailing-width 0)

;;;;; Autocommit after dictionary entry

  (defun my-org-capture-dictionary-finalize ()
    "Autocommit entry after capture to dictionary"
    (let
        (
          (key  (plist-get org-capture-plist :key))
          )

      (if org-note-abort
          (message "Template with key %s and description “%s” aborted" key desc)
        ;; Autocommit word/phrase added to dictionary.org with capture template
        (if (string-equal key "d")
            (progn
              (setq word (car (cdr (split-string (car (split-string (plist-get org-capture-plist :template) " :drill:")) "* "))))
              ;; asynchronously & silent
              (my-async-shell-command
               (concat "cd '" (f-parent (concat org-directory "/dictionary/dictionary.org")) "' && git add ./dictionary.org && git commit -m 'add " word "' &> /dev/null && git push &> /dev/null && exit")
               )
              )
          )

        ;;  2020-02-06: NOTE: Not finished. Currently commits with previous 'dictionary' capture data.
        (if (string-equal key "w")
            (progn
              (setq phrase (car (cdr (split-string (car (split-string (plist-get org-capture-plist :template) " :drill:")) "* "))))
              ;; asynchronously & silent
              (my-async-shell-command
               (concat "cd '" (f-parent (concat org-directory "/word/word.org")) "' && git add ./word.org && git commit -m 'add " word "' &> /dev/null && git push &> /dev/null && exit")
               )
              )
          )
        )
      )
    )

  (add-hook 'org-capture-after-finalize-hook #'my-org-capture-dictionary-finalize)

;;;;; Disable fringe around Emacs windows

  (set-fringe-mode 0)
  (global-vi-tilde-fringe-mode 'nil)

;;;;; After capture runs - go into insert state

  (add-hook 'org-capture-mode-hook #'evil-insert-state)


;;;; Modeline configuration

  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-version-control-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-major-mode-off)
  (spaceline-toggle-purpose-off)
  (spaceline-toggle-buffer-position-off)

;;;; Autosave configuration

  (custom-set-variables
   '(auto-save-interval 25)    ;; It is programming - autosave more aggressively
   )

;;;; misc

;;;;; Setting-up org-goto

  (custom-set-variables
   '(org-goto-interface 'outline-path-completion)
   '(org-outline-path-complete-in-steps nil)
   )

;;;;; Disable the long-line gradual scroll
  (custom-set-variables
   '(auto-window-vscroll nil)    ;; Allows to scroll partially trough wrapped long lines, but hangs the Emacs
   )

;;;;; Org-LaTeX process setup

  ;; Export with LuaTeX -> dvisvgm
  (add-to-list 'org-preview-latex-process-alist
               '(luadvisvgm :programs
                            ("lualatex" "dvisvgm")
                            :description "dvi > svg" :message "you need to install the programs: lualatex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
                            (1.7 . 1.5)
                            :latex-compiler
                            ("lualatex --output-format dvi --shell-escape --interaction=nonstopmode --output-directory=%o %f")
                            :image-converter
                            ("dvisvgm %f -n -b min -c %S -o %O"))
               )

;;;;; Config to use Structured Haskell mode (SHM)

;;;;;; Integrate keymap on `haskell-mode` into Spacemacs Evil

  (evil-define-key '(normal) haskell-mode-map
    (kbd "TAB") 'outshine-cycle  ;; Integration with the outshine
    (kbd "<backtab>") 'outshine-kbd-<backtab>    ;; Integration with the outshine
    ;; ">" 'org-shiftmetaright
    ;; "<" 'org-shiftmetaleft
    ")" 'shm/goto-parent-end
    )

;;;;;; Hooks for Structured Haskell

  ;; (add-hook 'haskell-mode-hook #'structured-haskell-mode)  ;; Toggle structured-haskell-mode

;;;;;; drafts

  ;; (eval-after-load "evil-maps"
  ;;   (dolist (map '(evil-normal-state-map))
  ;;     (define-key (eval map) "\C-j" nil)))

;;;;; Yequake config

  (custom-set-variables
   '(yequake-frames
     '(("org-capture"
        (buffer-fns . (yequake-org-capture))
        (width . 0.75)
        (height . 0.5)
        (alpha . 0.95)
        (frame-parameters . ((undecorated . t)
                             (skip-taskbar . t)
                             (sticky . t)))))))

;;;;; PDF view setup. Make PDF midnight black, coloring here since it recets setting of the layer
  (setq pdf-view-midnight-colors '("#ffffff" . "#000000" ))

;;;; Side-load org-sync patched fork

  (add-to-list 'load-path "~/.spacemacs.d/lisp/lisp/org-sync") ; 2020-09-19: NOTE: Adds support for the Magit GitHub forge

;;;;; Load the org-sync backends "I need"
  (mapc 'load
        '("org-sync" "org-sync-bb" "org-sync-github" "org-sync-redmine"))
;;;;; Load authkey for Org-sync to manage the remote

  (setq my-token-github-org-sync (my-get-file-char-range-as-string "/i/Personal/Technical/Files/Secure/token-rogue-github-org-sync" 0 40))
  (setq org-sync-github-auth `("Anton-Latukha" . ,my-token-github-org-sync))

;;;; Workaround of Tramp & git-gutter+-refresh wrong type argument

  ;; fixes to git-gutter+ when using tramp
  (with-eval-after-load 'git-gutter+
    ;; https://github.com/syl20bnr/spacemacs/issues/12860#issuecomment-602084919
    (defun git-gutter+-remote-default-directory (dir file)
      (let* ((vec (tramp-dissect-file-name file))
             (method (tramp-file-name-method vec))
             (user (tramp-file-name-user vec))
             (domain (tramp-file-name-domain vec))
             (host (tramp-file-name-host vec))
             (port (tramp-file-name-port vec)))
        (tramp-make-tramp-file-name method user domain host port dir)))

    (defun git-gutter+-remote-file-path (dir file)
      (let ((file (tramp-file-name-localname (tramp-dissect-file-name file))))
        (replace-regexp-in-string (concat "\\`" dir) "" file)))
    )

  ;; (add-hook 'org-mode-hook #'org-roam-mode)
  ;; (add-hook 'org-roam-mode #'org-roam--build-cache-async)

  (setq org-roam-directory org-directory)
  (spacemacs/declare-prefix-for-mode 'org-mode "r" "roam")
  (spacemacs/set-leader-keys-for-minor-mode 'org-roam-mode
    "r r" 'org-roam
    "r d" 'org-roam-today
    "r f" 'org-roam-find-file
    "r i" 'org-roam-insert
    "r v" 'org-roam-show-graph
    )

  (setq org-roam-directory org-directory)
  (setq org-roam-index-file (concat org-roam-directory "/index.org"))

;;;; Run Agenda as initial buffer

  (my-org-agenda-day-list)

  (setq initial-buffer-choice (lambda () (get-buffer "*Org Agenda*")))

;;;; misc
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)

;;;; 
  )


