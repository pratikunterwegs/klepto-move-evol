#!/bin/bash

# Rscript --slave -e 'bookdown::render_book("index.rmd", "bookdown::pdf_document2")'

# to render source files
# Rscript --slave -e 'lapply(list.files(pattern = "\\d{2}\\w+.Rmd"), function(x) knitr::purl(x, output = sprintf("R/%s", gsub(".{4}$", ".R", x))))'

# copy pdfs and name correctly
rm docs/ms_kleptomove_*.tex
rm docs/ms_kleptomove_*.pdf
rm docs/supplement_kleptomove_*.pdf
cp -p overleaf-kleptomove/manuscript.tex docs/ms_kleptomove_`date -I`.tex
cp -p overleaf-kleptomove/manuscript.pdf docs/ms_kleptomove_`date -I`.pdf
cp -p supplementary_material/supplement.pdf docs/supplement_kleptomove_`date -I`.pdf
cp figures/fig_0*.png overleaf-kleptomove/figures

# build atlastools manual
# Rscript --slave -e 'devtools::build_manual(pkg = "../atlastools", path = "docs/")'

# render docx
pandoc overleaf-kleptomove/manuscript.tex --reference-doc=docs/template.docx --bibliography=overleaf-kleptomove/kleptomove.bib -o docs/ms_kleptomove_`date -I`.docx
