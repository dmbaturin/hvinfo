PREFIX = /usr
BINDIR = bin

TARGET_DIR = $(PREFIX)/$(BINDIR)

GPRBUILD = gprbuild
GPRCLEAN = gprclean
GPRINSTALL = gprinstall
GNATPREP = gnatprep

INSTALL = install

all: src/hvinfo

src/hvinfo:
	./mkconfig.sh
	$(GPRBUILD) -Phvinfo

clean:
	$(GPRCLEAN)

install:
	$(INSTALL) -d $(TARGET_DIR)
	$(INSTALL) src/hvinfo $(TARGET_DIR)
