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
# $Id: F-456BD96CDEFE5086F68B54338511A946.sh,v 1.3 2006/04/04 14:37:59 florenz Exp $
#
# Simple packageing script for gennf.
#
# Usage: package.sh [tag] [version]
# 
# Checks out the given tag and builds a package with version
# as version string. If no tag is given, HEAD is checked out and
# used as version string.
# Environment variable BERLIOS_USER can be set to log into the
# BerliOS account to check out a Meta-CVS tree.
# The packageing takes place in the current working directory.
#

fail() {
    echo $@
    exit 1
}

if [ -n "${BERLIOS_USER}" ]; then
    BERLIOS_USER=${BERLIOS_USER}@
fi

MCVS=mcvs
CVSROOT=${BERLIOS_USER}cvs.gennf.berlios.de:/cvsroot/gennf
MODULE=gennf

DOC_DIR=doc
SRC_DIR=src
TESTS_DIR=tests

BASENAME=gennf
DOC_FILES="gennf-report.tex gennf.1 Makefile"

SRC_FILES="Makefile gennf.asd driver.in \
access.lisp checkpoint.lisp \
distribution.lisp merge.lisp backend-cvs.lisp \
command-line.lisp error.lisp \
merging.lisp backend.lisp configuration.lisp \
files.lisp miscellaneous.lisp \
branch.lisp debug.lisp gennf.lisp \
packages.lisp change.lisp directories.lisp \
mapping.lisp sbcl.lisp"

TESTS_FILES="test.sh"

TOPLEVEL_FILES="LICENSE INSTALL Makefile Makefile.include"

REPLACE_VERSION="INSTALL src/gennf.asd"

TAG=$1
VERSION=$2

if [ -z "$TAG" ]; then
    echo "No tag given, packageing HEAD."
    TAG=HEAD
fi

if [ -z "$VERSION" ]; then
    echo "No version string given, using $TAG."
    VERSION=$TAG
fi

VERSIONNAME=$BASENAME-$VERSION

echo "Checking out $BASENAME."
$MCVS -d $CVSROOT co -r $TAG $MODULE || fail "Unable to check out $MODULE."

echo "Creating package directory $VERSIONNAME and subdirectories."
mkdir $VERSIONNAME

mkdir $VERSIONNAME/$DOC_DIR
mkdir $VERSIONNAME/$SRC_DIR
mkdir $VERSIONNAME/$TESTS_DIR

echo "Copying files."
for f in $TESTS_FILES; do
    echo "$MODULE/$f -> $VERSIONNAME/$TESTS_DIR"
    cp $MODULE/$TESTS_DIR/$f $VERSIONNAME/$TESTS_DIR/
done
for f in $SRC_FILES; do
    echo "$MODULE/$f -> $VERSIONNAME/$SRC_DIR"
    cp $MODULE/$SRC_DIR/$f $VERSIONNAME/$SRC_DIR/
done
for f in $DOC_FILES; do
    echo "$MODULE/$f -> $VERSIONNAME/$DOC_DIR"
    cp $MODULE/$DOC_DIR/$f $VERSIONNAME/$DOC_DIR/
done
for f in $TOPLEVEL_FILES; do
    echo "$MODULE/$f -> $VERSIONNAME"
    cp $MODULE/$f $VERSIONNAME
done

echo "Replace %%VERSION%% by $VERSION."
for f in $REPLACE_VERSION; do
    sed "s/%%VERSION%%/$VERSION/" < $VERSIONNAME/$f > $VERSIONNAME/$f.sed
    mv $VERSIONNAME/$f.sed $VERSIONNAME/$f
done

echo "Create a tarball $VERSIONNAME.tar.gz"
tar cf - $VERSIONNAME | gzip -c >  $VERSIONNAME.tar.gz

echo "Delete checked out copy and package tree."
rm -rf $MODULE 
rm -rf $VERSIONNAME
