## Spacemacs setup

This setup is both simple to use and very powerful.
It also incorporates all main features you need to have perfect ebb and flow in the company.

### Installation

#### Emacs
Setup Emacs.

#### Spacemacs distribution
You need to setup [Spacemacs](https://github.com/syl20bnr/spacemacs) official way, but do not launch it, until we do setup from company. (It would work eather way, but it is cleaner not to have `~/.spacemacs`, and have `~/.spacemacs.d/init.el` instead. Or you can clean `~/.spacemacs`` afterwards)

To install Spacemacs:
```shell
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
```

#### Spacemacs config from company
```shell
git clone git@github.com:serokell/spacemacs-setup.git ~/.spacemacs.d
```

Now you made setup.

#### Haskell packages
To work with Haskell in Emacs you need to have this packages installed:
```
apply-refact hlint stylish-haskell hasktags hoogle
```
* With Stack: `stack install apply-refact hlint stylish-haskell hasktags hoogle`
* Arch Linux Pacman: `pacman -Syu haskell-refact hlint stylish-haskell stylish-haskell hasktags hoogle`

And then also check PATH have `~/.local/bin` for Stack to work.

#### Creating branch
To foster develpment and polish of our setup, to make sharing of code easier. And to backup and store your configuration in git repository, create branch in this repository.

```shell
git checkout -b <your_github_username>
# -b to create branch, and then checkout on it
```

Now you are on your own branch.

#### Launch Emacs

#### (Optional)
If you need Vim tutorial, or Spacemacs tutorial, go and have them.

#### Open properties
Pres `SPC f e d` in consequential order.
You opened main configuration of Spacemacs. This is your command post that controls all Emacs infrastructure.

#### Look through file
Most of file is auto-generated and maintained automatically, by Emacs and Spacemacs.
The manual blocks wrapped in:
```lisp
     ;; ----------------------------------------------------------------
     ;; Manually configured block
     ;;
     ;; ----------------------------------------------------------------

```

#### Properties configuration
Find `dotspacemacs-configuration-layers` block.
This is the main configuration block.

Read through it and enable layers you want to use in your setup.

In the end of manual block - there is a link to more less used layers and more information on all layers.
If you have suggestions to default setup, create issue/pull request to repository.

#### Commit to branch
Store setup in your branch:
```shell
git push -u origin <your_github_username>
# -u to create branch on remote and then push
```

