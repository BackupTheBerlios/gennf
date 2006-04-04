#! /usr/local/bin/bash 
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
# $Id: F-24A443AA6A691B170F1B1DE11017E375.sh,v 1.7 2006/04/04 14:37:59 florenz Exp $
#
# test.sh
#

GENNF=/tmp/y/bin/gennf

function box()
{
    LINE=$1
    echo "--------------------------------------------------------------------------------"
    echo "| $LINE"
    echo "--------------------------------------------------------------------------------"
}

function die()
{
    WHAT=$1
    HOW=${2:-1}
    echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
    echo "@ $WHAT"
    echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
    exit $2
}

function gennf_co() 
{
    WHERE=$1
    MODULE=$2
    REPO=$3
    (
	cd $WHERE || die "could not 'cd' to $WHERE"
	box "gennf co $MODULE -r $REPO"
	$GENNF co $MODULE -r $REPO || die "error while: gennf co"
    ) || die "could not perform gennf_co" $?
}

function gennf_up() 
{
    WHERE=$1
    MODULE=$2
    REPO=$3
    (
	cd $WHERE || die "could not 'cd' to $WHERE"
	box " gennf update $MODULE -r $REPO"
	$GENNF update $MODULE -r $REPO || die "error while: gennf update"
    ) || die "could not perform gennf_update" $?
}

function gennf_add()
{
    WHERE=$1
    MODULE=$2
    WHAT=$3
    (
	cd $WHERE/$MODULE  || die "could not 'cd' to $WHERE"
	box " gennf add $WHAT"
	$GENNF add $WHAT || exit 2
    ) || die "could not perform gennf_add" $?

}

function gennf_ci()
{
    WHERE=$1
    MODULE=$2
    (
	cd $WHERE/$MODULE  || die "could not 'cd' to $WHERE"
	box " gennf ci"
	$GENNF ci || exit 2
    ) || die "could not perform gennf_ci" $?
}

function gennf_branch()
{

    MODULE=$1
    REPO_A=$2
    REPO_B=$3
    BRANCH=$4


    box " gennf branch $MODULE -f $REPO_A -t $REPO_B  -b $BRANCH"    


    $GENNF branch $MODULE  -f $REPO_A -t $REPO_B  -b $BRANCH || die "could not perform gennf_branch"
}

function gennf_merge()
{

    MODULE=$1
    REPO_A=$2
    REPO_B=$3
    BRANCH=$4
    DEST=$5

    box " gennf merge $MODULE -f $REPO_A -t $REPO_B  -b $BRANCH"    

    $GENNF merge $MODULE  -f $REPO_A -t $REPO_B -b $BRANCH -d $DEST || die "could not perform gennf_branch"
}

########################################
# 
########################################

set -e

MODULE=foo

BRANCH_NAME1=a1
BRANCH_NAME2=b1

REPO_A=/tmp/$BRANCH_NAME1
REPO_B=/tmp/$BRANCH_NAME2

### Directory-Structure.
rm -rf $BRANCH_NAME1 $BRANCH_NAME2
mkdir -p $BRANCH_NAME1 $BRANCH_NAME2

### Repos init

rm -rf $REPO_A $REPO_B
cvs -d $REPO_A init
cvs -d $REPO_B init

## creating module foo


box " gennf setup $MODULE -r $REPO_A"

$GENNF setup $MODULE -r $REPO_A

gennf_co $BRANCH_NAME1 $MODULE $REPO_A

(
    cd $BRANCH_NAME1/$MODULE
    touch main.c
    echo "/* comment */" > another.c
)

gennf_add $BRANCH_NAME1 $MODULE main.c

gennf_add $BRANCH_NAME1 $MODULE another.c

gennf_ci $BRANCH_NAME1 $MODULE 

gennf_branch $MODULE $REPO_A $REPO_B 1

gennf_co $BRANCH_NAME2 $MODULE $REPO_B

(
    cd $BRANCH_NAME2/$MODULE
    echo "branch b" > main.c
)

gennf_ci $BRANCH_NAME2 $MODULE 

(
    cd $BRANCH_NAME1/$MODULE
    echo "branch aaaaaaaaaaaaaa" > main.c
)

gennf_ci $BRANCH_NAME1 $MODULE

gennf_merge $MODULE $REPO_A $REPO_B 1 1

cat <<EOF
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@@@@@@ A conflict was generated and now has to be resolved
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
EOF

