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

CONFIG=config.def

# Create the gnatprep defs file
# Determine the arch
ARCH=$(uname -m)
X86=False
if [ ! $(expr $ARCH : 'i[356]86') -o $ARCH = 'amd64' -o $ARCH = 'x86_64' ]; then
    X86=True
fi

echo "X86 := $X86" > $CONFIG
