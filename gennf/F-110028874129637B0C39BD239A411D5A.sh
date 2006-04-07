#!/usr/local/bin/bash
#
# init.sh

. ./lib.sh


tell "Create Repositories."
mkdir $REPO1
mkdir $REPO2
cvs -d $REPO1 init
cvs -d $REPO2 init

tell "Setup module."
$GENNF setup $MODULE --root $REPO1 --symbolic-name find \
    --description "Finite-domain-constraint-solver"

tell "Create working directory."
mkdir $WORK

tell "Checkout gennf module."
cd $WORK
$GENNF checkout $MODULE --root $REPO1

tell "Populate gennf module with files."
cd $MODULE
for f in $FD_FILES; do
    tell $f
    cp $FD_DIR/$f .
done

tell "Add and commit files."
$GENNF add $FD_FILES
$GENNF commit

tell "Change some files and commit again."
for f in $MOD1_FILES; do
    tell $f
    cat $MOD1_DIR/$f > $f
    $GENNF commit
done

tell "Delete sandbox."
cd ..
rm -rf $MODULE
