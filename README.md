# Spacemacs setup
<a name="top"></a>
<a href="http://spacemacs.org"><img src="https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg" alt="Made with Spacemacs"></a><a href="http://www.twitter.com/spacemacs"><img src="http://i.imgur.com/tXSoThF.png" alt="Twitter" align="right"></a><br>
***
<p align="center"><img src="https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/img/title2.png" alt="Spacemacs"/></p>
<p align="center">

This setup is both simple to use and very powerful.
It also incorporates all main features you need to have perfect ebb and flow in the company.

## Installation

### Emacs
Setup Emacs.

### Spacemacs distribution
_Note: do not launch Emacs until we get to 'Launch Emacs'. It would work in the end properly anyway. But it is cleaner not to have `~/.spacemacs`, and have `~/.spacemacs.d/init.el` instead. Or you can clean `~/.spacemacs`` afterwards)_

You need to setup [Spacemacs](https://github.com/syl20bnr/spacemacs) official way:
```shell
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
```

### Spacemacs config
```shell
git clone git@github.com:serokell/spacemacs-setup.git ~/.spacemacs.d
```

Now you've made the setup.

### Haskell packages
Emacs to work with Haskell requires next packages:
```
apply-refact hlint stylish-haskell hasktags hoogle
```
* With Stack: `stack install apply-refact hlint stylish-haskell hasktags hoogle`
* Arch Linux Pacman: `pacman -Syu haskell-refact hlint stylish-haskell stylish-haskell hasktags hoogle`

For Stack to work you also need to have `~/.local/bin` part in `PATH`.

### Creating branch
To foster expansion of this setup and repo, to make sharing of code and config features easier. And to backup and store your configuration in git repository, create branch in this repository.

```shell
git checkout -b <your_github_username>
# -b to create branch, and then checkout on it
```

Now you are on your branch.

### Launch Emacs

### (Optional)
If you need Vim tutorial, or Spacemacs tutorial, go and have them.

### Open properties
Pres `SPC f e d` in consequential order.
You opened main configuration of Spacemacs. This is your command post that controls all Emacs infrastructure.

### Look through file
Most of file is auto-generated and maintained automatically, by Emacs and Spacemacs.
The manual blocks wrapped in:
```lisp
     ;; ----------------------------------------------------------------
     ;; Manually configured block
     ;;
     ;; ----------------------------------------------------------------

```

### Properties configuration
Find `dotspacemacs-configuration-layers` block.
This is the main configuration block.

Read through it and enable layers you want to use in your setup.

In the end of manual block - there is a link to more less used layers and more information on all layers.
If you have suggestions to default setup, create issue/pull request to repository.

### Commit to branch
Store setup in your branch:
```shell
git push -u origin <your_github_username>
# -u to create branch on remote and then push
```

