;;; Layers configuration

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-configuration-layers
   '(
;;; Layers
;;;; Initial, misc

     ;; ----------------------------------------------------------------
     ;; Manually configured block
     ;;
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)

     version-control    ; Emacs support for different VCS (caution, can collide with separate Git control layers/configs

     sphinx    ; Documentation generator by Python

     transmission    ; Torrent client
     vagrant


     ;; == Great
     slack
     osx
     search-engine    ; Support of many search engines
     elfeed    ; Atom and RSS feeds. Elfeed, a web feeds client
     evernote
     spotify
     twitter

     floobits    ; Support for pair programming

     ;; For fun
     emoji    ; yes :satisfied:. But caution, it going to conflict with org-mode ':' symbol in time tracking
     games
     selectic    ; Typewriter typing sound
     xkcd    ; xkcd

     ;; Preferences
     ;;
     ;; Note: some layers require installed packages on system to work with, and enable some features. Look at documentation at URL mentioned at the end of code section.
     ;;

     spell-checking ; Requires aspell and `aspell` dictionary installed for your national languages (aka `aspell-en`)

     semantic    ; Collection of Emacs Development Environment Tools
     ipython-notebook

     ;; == International support
     unicode-fonts    ; Fonts for any ethnic languages
     keyboard-layout    ; Support for any keyboard layouts
     chinese

     ;; == Programming languages
     agda
     asm
     c-c++
     clojure
     common-lisp
     coq
     csharp
     d
     elixir
     erlang
     fsharp
     go
     groovy
     java
     lua
     ocaml
     perl5
     perl6
     php
     purescript
     racket
     ruby
     rust
     scala
     scheme
     semantic-web    ; Support for Turtle language
     swift
     windows-script

     ;; == Frameworks
     django
     react
     ruby-on-rails

     ;; == Domain Specific Languages (DSL)
     gpu    ; Languages to work with GPUs
     major-modes    ; Support of more rare case DSL languages, like Arduino, MATLAB, QML, Vala, Wolfram...

     ;; == Markup languages
     graphviz
     latex
     protobuf
     restructuredtext

     nginx
     node    ; Package manager

     pdf-tools
     puppet
     restclient
     salt    ; Configuration management tool
     )
   )
  )
