#! /usr/bin/env bash

cd '/Users/michaelferon/Projects/rowave2/src/latex/'
tex_file='./plots.tex'
aux_out='./aux'
pdf_out='./aux/plots.pdf'
move='../plots/plots.pdf'

[ -d $aux_out ] && rm -r $aux_out
mkdir $aux_out
[ -f $move ] && rm $move

pdflatex -output-directory $aux_out $tex_file > /dev/null 2>&1
mv $pdf_out '../../plots'

