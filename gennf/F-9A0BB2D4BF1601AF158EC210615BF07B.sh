#!/usr/local/bin/bash
#
# demo.sh

. ./lib.sh


tell "Create a branch."
$GENNF branch $MODULE --root-from $REPO1 --root-to $REPO2 --branch 1

tell "Make changes in branch and commit."
cd $WORK
$GENNF checkout $MODULE --root $REPO2
cd $MODULE
$GENNF move tests.lisp tests/tests.lisp
$GENNF move sudoku.lisp tests/sudoku.lisp
$GENNF move send-more-money.lisp tests/send-more-money.lisp
for f in $MOD2_FILES; do
    cat $MOD2_DIR/$f > $f
done
$GENNF commit

tell "Delete sandbox."
cd ..
rm -rf $MODULE

tell "Checkout branch from first repository, make changes and commit."
$GENNF checkout $MODULE --root $REPO1
cd $MODULE
$GENNF mv tests.lisp fd-test.lisp
$GENNF commit 

tell "Delete sandbox."
cd ..
rm -rf $MODULE

tell "Merge branch from second repository in first."
cd ..
$GENNF merge $MODULE --root-from $REPO2 --root-to $REPO1 \
    --branch-from 1 --branch-to 1
