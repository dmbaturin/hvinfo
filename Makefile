PREFIX = /usr
BINDIR = bin/

all: hvinfo

hvinfo:
	gnatmake src/hvinfo.adb

clean:
	gnatclean hvinfo hypervisor_check

install:
	mkdir -p $(PREFIX)/$(BINDIR)
	cp hvinfo $(PREFIX)/$(BINDIR)
