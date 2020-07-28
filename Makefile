install:
	./install.sh

add-ppa-alacritty:
	sudo add-apt-repository ppa:mmstick76/alacritty

install-alacritty:
	sudo apt install alacritty

add-ppa-regolith:
	sudo add-apt-repository ppa:regolith-linux/release

install-regolith:
	sudo apt install regolith-desktop \
	  i3xrocks-time \
	  i3xrocks-volume \
	  i3xrocks-battery

add-ppa: add-ppa-alacritty add-ppa-regolith

install-packages: install-alacritty install-regolith
