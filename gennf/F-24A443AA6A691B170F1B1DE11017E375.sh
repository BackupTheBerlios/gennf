#! /usr/local/bin/bash 

GENNF=/home/ni/sigsegv/src/lisp/gennf/prototype/gennf
REPO_A=/tmp/a
REPO_B=/tmp/b
MODULE=foo

#export GENNF REPO_A REPO_B MODULE

set -e

### Directory-Structure.
rm -rf a b 
mkdir -p  a b 

### Repos init
rm -rf $REPO_A $REPO_B
cvs -d $REPO_A init
cvs -d $REPO_B init

function gennf_co() 
{
    WHERE=$1
    MODULE=$2
    REPO=$3
    (
	cd $WHERE || echo  "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	echo "****************************************"
	echo "gennf co $MODULE -r $REPO"
	echo "****************************************"
	$GENNF co $MODULE -r $REPO
    )
}

function gennf_add()
{
    WHERE=$1
    MODULE=$2
    WHAT=$3
    (
	cd $WHERE/$MODULE || echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	echo "****************************************"
	echo "gennf add $WHAT"
	echo "****************************************"
	$GENNF add $WHAT
    )

}

function gennf_ci()
{
    WHERE=$1
    MODULE=$2
    (
	cd $WHERE/$MODULE || echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	echo "****************************************"
	echo "gennf ci"
	echo "****************************************"
	$GENNF ci
    )
}

# function gennf_branch()
# {
# }

# function gennf_merge()
# {

# }

## creating module foo
echo "*** calling gennf: $GENNF setup $MODULE -r $REPO_A"
$GENNF setup $MODULE -r $REPO_A

gennf_co a $MODULE $REPO_A

(
    cd a/$MODULE
    touch main.c
)

gennf_add a $MODULE main.c


gennf_ci a $MODULE 


# brachning the repo
echo "*** calling gennf: $GENNF branch $MODULE -f /tmp/a -t /tmp/b -b 1"
$GENNF branch $MODULE -f /tmp/a -t /tmp/b -b 1

gennf_co b $MODULE $REPO_B

(
    cd b/$MODULE
    echo "branch b" > main.c
)

gennf_ci b $MODULE 

(
    cd a/$MODULE
    echo "branch aaaaaaaaaaaaaa" > main.
)

gennf_ci a $MODULE


# merging...
echo "*** $GENNF merge foo -f $REPO_A -t $REPO_B -b 1 -d 1"
$GENNF merge foo -f $REPO_A -t $REPO_B -b 1 -d 1
echo <<EOF
 A conflict was generated and now has to be resolved
EOF
