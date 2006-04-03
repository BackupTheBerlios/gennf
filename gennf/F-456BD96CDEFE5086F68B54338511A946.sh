#!/bin/sh

MCVS=mcvs
CVSROOT=florenz@cvs.gennf.berlios.de:/cvsroot/gennf
MODULE=gennf

DOC_DIR=doc
SRC_DIR=src
TESTS_DIR=tests

BASENAME=gennf
DOC_FILES="gennf-report.tex gennf.1 Makefile"
SRC_FILES="Makefile gennf.asd driver-template \
access.lisp checkpoint.lisp \
distribution.lisp merge.lisp backend-cvs.lisp \
command-line.lisp error.lisp \
merging.lisp backend.lisp configuration.lisp \
files.lisp miscellaneous.lisp \
branch.lisp debug.lisp gennf.lisp \
packages.lisp change.lisp directories.lisp \
mapping.lisp sbcl.lisp"
TESTS_FILES="test.sh"
TOPLEVEL_FILES="LICENSE INSTALL"

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
$MCVS -d $CVSROOT co -r $TAG $MODULE

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
tar cvzf $VERSIONNAME.tar.gz $VERSIONNAME

echo "Delete checked out copy and package tree."
rm -rf $MODULE 
rm -rf $VERSIONNAME