#!/bin/bash

# copy pdfs and name correctly
rm docs/ms_kleptomove_*.pdf

cp figures/fig_0*.png overleaf-kleptomove/figures

cd overleaf-kleptomove
pdflatex manuscript.tex
bibtex manuscript.aux
bibtex manuscript.aux
pdflatex manuscript.tex

cd ..

cp -p overleaf-kleptomove/manuscript.pdf docs/ms_kleptomove_`date -I`.pdf

# render docx
# pandoc overleaf-kleptomove/manuscript.tex --reference-doc=docs/template.docx --bibliography=overleaf-kleptomove/kleptomove.bib -o docs/ms_kleptomove_`date -I`.docx

# no refs
# pandoc overleaf-kleptomove/manuscript.tex --reference-doc=docs/template.docx -o docs/ms_kleptomove_`date -I`_no_refs.docx
