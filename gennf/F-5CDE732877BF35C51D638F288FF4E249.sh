#!/bin/sh
#
# Copyright 2005 Florian Lorenzen, Fabian Otto
#
# This file is part of gennf.
#
# gennf is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# gennf is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with gennf; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
# $Id: F-5CDE732877BF35C51D638F288FF4E249.sh,v 1.3 2006/04/06 08:47:13 florenz Exp $
#
# Script to upload PDF files to the gennf homepage at
# http://gennf.berlios.de/.
#

FILES="$@"

if [ -z "$FILES" ]; then
    echo "No files given. Abort."
    exit 1
fi

SERVER=shell.berlios.de
ROOT=/home/groups/gennf/htdocs

if [ -n "$BERLIOS_USER" ]; then
    BERLIOS_USER="$BERLIOS_USER@"
fi

for f in $FILES; do
    echo "Upload of $f."
    scp $f "$BERLIOS_USER$SERVER:$ROOT"
    fn=`echo $f | awk -F/ '{ print $NF; }'`
    ssh "$BERLIOS_USER$SERVER" "cd $ROOT; chmod 664 $fn"
done

