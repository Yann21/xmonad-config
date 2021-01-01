## Xmonad Configuration

# Specification
* Arch Linux
* XMonad
* XMonad-contrib
* XMobar

# Run
1. Install stack: `yay -S stack-static`
2. Use stack to install xmonad and xmobar: `stack install {xmonad,xmobar,xmonad-contrib}`
3. Build the project: `stack build :my-xmonad`
4. Replace the .xmonad/xmonad_x86 executable: `xmonad --recompile`

# Content
src/
├── autostart.sh
├── Modules
│   ├── Keys.hs
│   ├── Layouts.hs
│   ├── MyTreeSelect.hs
│   └── Others.hs
└── xmonad.hs
