# Source Code and Supplementary Material for _The joint evolution of movement and competition strategies_

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) 
UPDATE TO REAL PREPRINT -- [![DOI:10.1101/2020.12.15.422876](https://img.shields.io/badge/bioRxiv-doi.org/10.1101/2020.12.15.422876-red?style=flat-square)](https://www.biorxiv.org/content/10.1101/2020.12.15.422876v3)
UPDATE TO REAL ZENODO -- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4287462.svg)](https://doi.org/10.5281/zenodo.4287462)

This is the source code for the analyses and figures for a manuscript that reports on models the joint evolution of movement and competition strategies using massive, spatial, individual-based models.
This work was developed in the [Modelling Adaptive Response Mechanisms Group (Weissing Lab)](https://www.marmgroup.eu/) at the Groningen Institute for Evolutionary Life Science, at the University of Groningen.

## Contact and Attribution

Please contact [Pratik Gupte](p.r.gupte@rug.nl) or [Franjo Weissing (PI)](f.j.weissing@rug.nl) for questions about the associated manuscript.

The work can be cited as **DO BIBTEX KEY -- WIP**.

## Simulation Source Code

The simulation source code is provided at a different repository, [_Kleptomove_](github.com/pratikunterwegs/Kleptomove).

## Simulation Data

The simulation data are originally generated in the `data/` directory of the _Kleptomove_ repository, and manually copied to the `data_sim/` directory in this repository. These simulation data can be found at the University of Groningen Dataverse repository **HERE: ADD REPO HERE**.

## Analysis Functions

The simulation data are summarised by a series of `R` functions, and this repository is configured to be an `R` package that can be built (`devtools::build`) and installed (`devtools::install`).
These functions are in the directory `R/`, and are documented in the directory `man/`.

- `R/fun_functional_response.R`

    - `bin_vec` Function to bin numeric vectors.

    - `do_read_data` Function to get the cell specific, per-timestep value of a single variable in each of the generations queried. _Not used in this manuscript_.

    - `get_intake_variance` Get variance in intake per strategy. _Not used here._

    - `get_functional_response` Get the intake per strategy over cells, grouped by number of items and number of individuals.

- `R/fun_get_strategy_gen.R` contains `get_strategy_gen`, which gets the per-generation 'activity budget' from the data.

- `R/fun_potential_intake.R` A small function to get the probability of finding (not acquiring!) a food item on a cell with _N_ items.

- `R/fun_read_land.R` 

    - `read_landscape` Function to fully or partially read in a `.png` 'ecological snapshot' exported from the simulation, and return the individuals or items per cell; the `type` option allow for raw counts (`items`) or the two-dimensional gradient (`gradient`).

    - `get_layer_variance` Function to read in specific layers from an ecological snapshot, and get the variance in values.

- `R/fun_syndrome.R` contains a function, `get_pref_handler_by_strat` to determine how handler preference (or another decision making weight) varies by competition strategy.

- `R/fun_theme.R` defines a custom theme for the figures.

- `R/fun_weight_evo.R`

    - `prepare_extractor` A function to prepare the extractor program. _Not used here._

    - `get_generation_data` Gets data for a single generation from a single simulation replicate output.

    - `get_weights_prop` Get the frequency of hyperbolic tangent transformed weight values, from a single generation data object.

    - `get_weights_timeline` Get the frequency of weights per generation for a single simulation output.

    - `get_sim_weight_evol` Apply the `get_weights_timeline` to a folder holding a single simulation replicate output.

    - `get_agent_distribution` Get the average number of individuals per strategy per generation per cell from specific generations (in the range 991 -- 998), for a single simulation output. _Not used here._

## Analysis Source Code

The source code for the analyses reported here can be found in the directory `scripts/`, and is explained here:

- `scripts/00_landscape_item_count.R` Counts the number of items and individuals per cell from ecological snapshots from the simulation.

- `scripts/01_get_evo_equilibria.R` Get the activity budget over multiple generations.

- `scripts/02_get_weight_evolution.R` Get the evolution of movement and foraging strategy decision making weight evolution over multiple generations.

- `scripts/03_get_syndrome.R` Get the preference for handlers by strategy per generation.

- `scripts/04_get_clueless_plateaus.py` Get the proportion of each ecological snapshot landscape from which no better moves are possible.

- `scripts/05_agents_per_items.R` Get data on individuals per items from each generation per replicate.

- `scripts/06_intake_variance.R` Get data on variance on intake rates per generation.

- `scripts/07_matching_rule.R` Get correlations between individual abundances and items and cell productivity.

## Figure Source Code

The source code for the figures in this manuscript is in the directory `figure_scripts/`; one script per figure, numbered in the figure order. These scripts are not explained further.

## Main Text

The main text of the manuscript is written in LaTeX and is stored in the (private) submodule, `overleaf-kleptomove`. A dated version rendered as PDF can be found in the directory `docs/` -- `docs/ms_kleptomove_DATE.pdf`, where `DATE` is the date the manuscript was rendered.

## Supplementary Material

The supplementary material provided with this manuscript is generated from the `supplementary_material/` directory. A dated version rendered as PDF can be found in the directory `docs/` -- `docs/supplement_kleptomove_DATE.pdf`, where `DATE` is the date the manuscript was rendered.

- `supplementary_material/spm_01_landscapes.Rmd` Code for figures 1 -- 3.

- `supplementary_material/spm_02_weight_evolution.Rmd` Code for figures 4 -- 6.

- `supplementary_material/supplement.tex` The supplementary material source file.

- `supplementary_material/figures` Figure output for the supplementary material file.

## Other Directories

- `bash/` Some useful shell scripts for output rendering.
