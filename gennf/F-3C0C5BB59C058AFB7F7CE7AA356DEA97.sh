# lib.sh

tell() {
    echo ">>>>>" $@
}

GENNF=gennf

FD_DIR=$PWD/data/fd.1
FD_FILES="fd.lisp sudoku.lisp send-more-money.lisp tests.lisp \
sudoku.lisp fd.asd README packages.lisp"

MOD1_DIR=$PWD/data/fd.2
MOD1_FILES="fd.lisp send-more-money.lisp"

MOD2_DIR=$PWD/data/fd.3
MOD2_FILES="fd.asd"

MODULE=fd
WORK=work

DESTDIR=$PWD
REPO1=$DESTDIR/repo1
REPO2=$DESTDIR/repo2
