#!/bin/sh

need_to_build_index ()
{
  ! cmp -s $1.idx $1.idx.old ||
    [ ! $1.ist -ot $1.ind ]
}

need_to_build_dvi ()
{
   ! cmp -s $1.toc $1.toc.old ||
     [ ! $1.latex -ot $1.dvi ] ||
     [ ! $1.ind -ot $1.dvi ]
}

build_index ()
{
  cp $1.idx $1.idx.old
  makeindex -s $1.ist $1.idx
}

build_dvi ()
{
  cp $1.toc $1.toc.old
  latex $1.latex
}

LATEXDOC=meta-cvs

while true ; do
  if need_to_build_dvi $LATEXDOC ; then
    build_dvi $LATEXDOC
    if need_to_build_index $LATEXDOC ; then
      build_index $LATEXDOC
    fi
  elif need_to_build_index $LATEXDOC ; then
    build_index $LATEXDOC
  else
    break
  fi
done
