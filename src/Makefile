
all:			## Build target and restart xmonad
	stack build :my-xmonad && xmonad --recompile && xmonad --restart

install:		## Install xmonad.desktop to /usr/share/xsessions
	if [ -d "/usr/share/xsessions" ]; then \
		sudo cp ../etc/xmonad.desktop /usr/share/xsessions \
	else \
		echo "/usr/share/xsessions does not exist, aborting" \
	fi

help:           ## Show this help.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##//'
