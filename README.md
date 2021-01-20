[![github](https://img.shields.io/badge/git-github-lightgray.svg)](https://github.com/Yann21/xmonad-config) [![Issues](https://img.shields.io/badge/issues-github-green.svg)](https://github.com/Yann21/xmonad-config/issues)
# <img src="https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fupload.wikimedia.org%2Fwikipedia%2Fcommons%2F7%2F72%2FXmonad-logo.png&f=1&nofb=1" height="36.5" width="23.5" /> monad Configuration
Caveat emptor as always.

## Table of contents
* [General info](#content-specification)
* [Get started](#how-to-run)
* [Structure](#content)
* [TODO](#todo)

## Content / Specification
* Arch Linux...
* XMonad...
* XMonad-contrib..
* XMobar...

## How to run
```
# Install stack
$ yay -S stack-static

# Use stack to install xmonad and xmobar
# This will send the executables ~/.local/bin, don't forget to add this to your path.
$ stack install xmonad xmobar xmonad-contrib

# Build the project
$ stack build :my-xmonad

# Replace the ~/.xmonad/xmonad_x86 executable:
$ xmonad --recompile (&& xmonad-restart)
```

## Content
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

## TODO?
* DynamicWorkspaces -> rename xmobar on the fly
* Pipe build output to xmessage
* Take a look inside xmonad-contrib
