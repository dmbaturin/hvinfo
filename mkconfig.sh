#!/bin/sh
# Copyright (C) 2015 Daniil Baturin <daniil@baturin.org>
#
# This file is part of hvinfo.
#
# hvinfo is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# hvinfo is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with hvinfo.  If not, see <http://www.gnu.org/licenses/>.

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

$GNATPREP -D LINUX=$HVINFO_LINUX \
         -D FREEBSD=$HVINFO_FREEBSD \
         -D VERSION=\"$VERSION\" \
         src/config.ads.in src/config.ads
