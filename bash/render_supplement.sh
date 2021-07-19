#!/bin/bash
# remove old tex files and pdfs

rm docs/*supplement*.pdf

# render supplement
Rscript --vanilla --slave -e 'bookdown::render_book("supplement/")'

mv docs/supplementary_material.pdf docs/ms_kleptomove_supplementary_material_`date -I`.pdf
