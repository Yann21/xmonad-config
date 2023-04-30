# Custom XMonad configuration

[![github](https://img.shields.io/badge/git-github-lightgray.svg)](https://github.com/Yann21/xmonad-config) [![Issues](https://img.shields.io/badge/issues-github-green.svg)](https://github.com/Yann21/xmonad-config/issues)
# <img src="https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fupload.wikimedia.org%2Fwikipedia%2Fcommons%2F7%2F72%2FXmonad-logo.png&f=1&nofb=1" height="36.5" width="23.5" /> monad Configuration
Caveat emptor as always.


## Table of contents

* [General info](#version)
* [Getting started](#how-to-run)
* [Structure](#content)
* [TODO](#todo)


## Version

XMonad uses the build script at "$HOME/.xmonad/build" when recompiling
(`xmonad --recompile`)
Moving it to $HOME/.xmonad/xmonad86_64..
For more information https://sitr.us/2018/05/13/build-xmonad-with-stack.html


## How to install

```
Install stack
$ yay -S stack[-static]

Use stack to install xmonad and xmobar
This will send the executables ~/.local/bin, don't forget to add this to your path.
$ stack install xmonad xmobar xmonad-contrib
# or
$ apt instal xmonad xmonad-contrib xmobar

Build the project
$ git clone xmonad-config ~/.xmonad
$ cd ~/.xmonad
$ stack upgrade
$ stack build :my-xmonad

Replace the ~/.xmonad/xmonad_x86 executable:
$ make all
$ xmonad --recompile (&& xmonad-restart)
```


## Lightdm

```
Install the xmonad session file for lightdm
$ make install
```


## Module structure

```
src/
├── autostart.sh
├── Modules
│   ├── Keys.hs
│   ├── Layouts.hs
│   ├── MyTreeSelect.hs
│   └── Others.hs
└── xmonad.hs
```
