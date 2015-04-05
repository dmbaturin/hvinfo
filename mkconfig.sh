#!/bin/sh

HVINFO_LINUX=False
HVINFO_FREEBSD=False

VERSION=$(cat VERSION)

OS=$(uname)
case $OS in
    Linux)
        HVINFO_LINUX=True
        ;;
    FreeBSD)
        HVINFO_FREEBSD=True
        ;;
    *)
        echo "Operating system $OS is not supported"
        exit 1
esac

gnatprep -D LINUX=$HVINFO_LINUX \
         -D FREEBSD=$HVINFO_FREEBSD \
         -D VERSION=\"$VERSION\" \
         src/config.ads.in src/config.ads
