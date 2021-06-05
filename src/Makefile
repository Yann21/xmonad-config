all:
	stack build :my-xmonad && xmonad --recompile && xmonad --restart
install:
	if [ -d "/usr/share/xsessions" ]; then \
		sudo cp ../etc/xmonad.desktop /usr/share/xsessions; \
	else \
		echo "/usr/share/xsessions does not exist, aborting"; \
	fi
