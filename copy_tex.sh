#!/bin/bash

# copy pdfs and name correctly
rm docs/ms_kleptomove_*.pdf
rm docs/supplement_kleptomove_*.pdf

cp -p overleaf-kleptomove/manuscript.pdf docs/ms_kleptomove_`date -I`.pdf
cp -p supplementary_material/supplement.pdf docs/supplement_kleptomove_`date -I`.pdf
cp figures/fig_0*.png overleaf-kleptomove/figures

# render docx
pandoc overleaf-kleptomove/manuscript.tex --reference-doc=docs/template.docx --bibliography=overleaf-kleptomove/kleptomove.bib -o docs/ms_kleptomove_`date -I`.docx
