PREFIX = /usr
BINDIR = bin

TARGET_DIR = $(PREFIX)/$(BINDIR)

GPRBUILD = gprbuild
GPRCLEAN = gprclean
GPRINSTALL = gprinstall
GNATPREP = gnatprep

INSTALL = install

all: src/config.ads src/hvinfo

src/config.ads: VERSION mkconfig.sh src/config.ads.in
	GNATPREP=$(GNATPREP) ./mkconfig.sh

.PHONY: src/hvinfo
src/hvinfo:
	$(GPRBUILD) -Phvinfo

clean:
	$(GPRCLEAN)

install:
	$(INSTALL) -d $(TARGET_DIR)
	$(INSTALL) src/hvinfo $(TARGET_DIR)
