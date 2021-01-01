[![github](https://img.shields.io/badge/git-github-lightgray.svg)](https://github.com/Yann21/xmonad-config) [![Issues](https://img.shields.io/badge/issues-github-green.svg)](https://github.com/Yann21/xmonad-config/issues)
# <img src="https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fupload.wikimedia.org%2Fwikipedia%2Fcommons%2F7%2F72%2FXmonad-logo.png&f=1&nofb=1" height="36.5" width="23.5" /> Xmonad Configuration

## Specification
* Arch Linux
* XMonad
* XMonad-contrib
* XMobar

## Run
1. Install stack: \
   `yay -S stack-static`
2. Use stack to install xmonad and xmobar: \
   `stack install xmonad xmobar xmonad-contrib`
3. Build the project: \
   `stack build :my-xmonad`
4. Replace the .xmonad/xmonad_x86 executable: \
   `xmonad --recompile`

## Content
```
src/
├── autostart.sh
├── Modules
│   ├── <a href="https://github.com/Yann21/xmonad-config/src/Modules/Keys.hs>Keys.hs</a>
│   ├── Layouts.hs
│   ├── MyTreeSelect.hs
│   └── Others.hs
└── xmonad.hs
```