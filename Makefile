PREFIX = /usr
BINDIR = bin

TARGET_DIR = $(PREFIX)/$(BINDIR)

BUILD_DIR = build

GPRBUILD = gprbuild
GPRCLEAN = gprclean
GPRINSTALL = gprinstall
GNATPREP = gnatprep

INSTALL = install

all: src/config.ads hvinfo

src/config.ads: VERSION mkconfig.sh src/config.ads.in
	GNATPREP=$(GNATPREP) ./mkconfig.sh

.PHONY: src/hvinfo
hvinfo:
	mkdir -p $(BUILD_DIR)
	$(GPRBUILD) -Phvinfo

clean:
	$(GPRCLEAN)
	rm -rf $(BUILD_DIR)

install:
	$(INSTALL) -d $(TARGET_DIR)
	$(INSTALL) $(BUILD_DIR)/hvinfo $(TARGET_DIR)
