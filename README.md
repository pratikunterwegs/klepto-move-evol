# Source Code and Supplementary Material for _The joint evolution of animal movement and competition strategies_

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4905475.svg)](https://doi.org/10.5281/zenodo.4905475)

This is the source code for the analyses and figures for a manuscript that reports on models the joint evolution of movement and competition strategies using massive, spatial, individual-based models.
This work was developed in the [Modelling Adaptive Response Mechanisms Group (Weissing Lab)](https://www.marmgroup.eu/) at the Groningen Institute for Evolutionary Life Science, at the University of Groningen.

## Contact and Attribution

Please contact Pratik Gupte or Franjo Weissing for questions about the associated manuscript.

```md
Name: Pratik Rajan Gupte
Email: pratikgupte16@gmail.com OR p.r.gupte@rug.nl
ORCID: https://orcid.org/0000-0001-5294-7819

Name: Franz J Weissing
Email: f.j.weissing@rug.nl
ORCID: https://orcid.org/0000-0003-3281-663X
```

Cite this repository archived on Zenodo as

Pratik Rajan Gupte, Christoph FG Netz, & Franz J Weissing. (2021). Source Code and Supplementary Material for "The Joint Evolution of Movement and Competition Strategies" (v1.0). Zenodo. https://doi.org/10.5281/zenodo.5112915.

```bibtex
@software{pratik_rajan_gupte_2021_5112915,
  author       = {Pratik Rajan Gupte and
                  Christoph FG Netz and
                  Franz J Weissing},
  title        = {{Source Code and Supplementary Material for "The 
                   Joint Evolution of Movement and Competition
                   Strategies"}},
  month        = jul,
  year         = 2021,
  note         = {{PRG was supported by an Adaptive Life Programme 
                   grant made possible by the Groningen Institute for
                   Evolutionary Life Sciences (GELIFES). C.F.G.N. and
                   F.J.W acknowledge funding from the European
                   Research Council (ERC Advanced Grant No. 789240).}},
  publisher    = {Zenodo},
  version      = {v1.0},
  doi          = {10.5281/zenodo.5112915},
  url          = {https://doi.org/10.5281/zenodo.5112915}
}
```

The Zenodo DOI https://doi.org/10.5281/zenodo.4904497 refers to all versions of this repository, and by default, to the latest version (on Zenodo) by default.

## Simulation Source Code

The simulation source code is provided at a different repository, [_Kleptomove_](https://github.com/pratikunterwegs/Kleptomove).
The _Kleptomove_ simulation is archived on Zenodo as https://zenodo.org/record/5887618, and can be cited as

Christoph FG Netz, & Pratik Rajan Gupte. (2022). Source Code for the "Kleptomove" Simulation (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.5887618.

```bibtex
@software{christoph_fg_netz_2022_5887618,
  author       = {Christoph FG Netz and
                  Pratik Rajan Gupte},
  title        = {Source Code for the "Kleptomove" Simulation},
  month        = jan,
  year         = 2022,
  note         = {{C.F.G.N. acknowledges funding from the European 
                   Research Council (ERC Advanced Grant No. 789240).
                   PRG was supported by an Adaptive Life Programme
                   grant made possible by the Groningen Institute for
                   Evolutionary Life Sciences (GELIFES).}},
  publisher    = {Zenodo},
  version      = {v1.0.0},
  doi          = {10.5281/zenodo.5887618},
  url          = {https://doi.org/10.5281/zenodo.5887618}
}
```

## Simulation Data

The simulation data are originally generated in the `data/` directory of the _Kleptomove_ repository, and manually copied to the `data/` directory in this repository.
Simulation data used in this study are available on the DataverseNL repository as a draft: https://dataverse.nl/privateurl.xhtml?token=1467641e-2c30-486b-a059-1e37be815b7c, and will be available at this persistent link after publication: doi.org/10.34894/JFSC41.

---

## Workflow: Replicating Basic Results

**Warning**: This is a relatively advanced computational study. Replicating it requires many interacting components. Please do _not_ expect it to work out of the box. A small amount of shell scripting in the terminal is required, or at least, useful.

A brief description of this workflow is:

### Preparing the C++ simulation _Kleptomove_

1. Download the _Kleptomove_ simulation from https://github.com/pratikunterwegs/Kleptomove. This can be done as

    ```sh
    git clone git@github.com:pratikunterwegs/Kleptomove.git
    ```

2. Build the C++ simulation in this repository using Visual Studio. The project was developed with Visual Studio 2019. Open the `kleptomove.sln` file with VS2019 and then _Build without debugging_. This should generate the `kleptomove.exe` file in the `bin/Release` subdirectory.

3.  Create a `data/` folder in the _Kleptomove_ directory. The _Kleptomove_ simulation code directory tree should then be:

  ```sh
  # running `tree` in the terminal should show, among other directories...

  Kleptomove
  ├───bin       
  │   ├───Debug
  │   ├───media
  │   │   └───Fonts
  │   ├───Release     # simulation executable is built here
  │   └───settings
  ├───cine            # simulation source code is here
  ├───cinema          # simulation GUI code is here
  ├───data            # simulation data is exported here
  ├───extract         # code for the compressed data extractor function
  └───tmp
  ```

  The _Kleptomove_ simulation is now ready to be run. The simulation dynamics can be visualised using the GUI, by running `kleptomove.exe` from the `bin/Release/` folder.

### Prepare to run simulation replicates

4. Get a copy of this repository _klepto-move-evol_ in one the following ways:
  - Download a compressed file from Zenodo. Zenodo hosts a few specific versions; we advise using the latest version (planned to be v1.0.1).
  - Download a compressed file from Github, ideally from the `master` branch; alternatively, under the _Releases_ tab, download a compressed file of a specific version.
  - Clone a copy of the repository from Github using (**we recommend this method!**):

    ```sh
    git clone git@github.com:pratikunterwegs/klepto-move-evol.git
    ```

    **Most users will find it sufficient to use the latest version of this repository**. If you wish to use code from a specific version, e.g. v1.0.1:

    - Check which versions are available using:

      ```sh
      git tag

      # example output:
      # v1.0
      # v1.0.1
      ```

    - Checkout (i.e., switch) to the version you want, creating a new branch:

      ```sh
      git checkout tags/v1.0 -b branch_name 
      
      # choose a logical branch name
      # a good example is v1.0_branch
      ```

    Cloning the repository has the benefit of preparing the directory tree in exactly the way we use it in our analyses. This should look like:

    ```sh
    # running `tree` should show
    klepto-move-evol
    ├───bash                # useful bash scripts to render supplement
    ├───data                # holds data
    │   ├───data_parameters # holds simulation landscape data
    │   ├───for_landscape   # landscape output for figure 5
    │   │   ├───sim_facultative_rep_001_gro_0.01
    │   │   │   └───depends
    │   │   ├───sim_foragers_rep_001_gro_0.01
    │   │   │   └───depends
    │   │   └───sim_obligate_rep_001_gro_0.01
    │   │       └───depends
    │   ├───results         # summary results from analyses
    │   └───sim_TYPE_rep_REPLICATE_gro_RMAX # simulation data replicates
    │       └───depends                     # there are multiple such folders
    │                                        # see template above for naming
    ├───docs                # folder to hold documents. contents not uploaded
    ├───figures             # figures for the main text
    ├───figure_scripts      # scripts to generate main text figures
    ├───man                 # documentation for functions defined in R/
    ├───overleaf-kleptomove # private submodule where we work on the manuscript
    │   └───figures
    ├───R                   # folder with functions that help in the analyses
    ├───renv                # restore R packages and versions used in analyses
    ├───scripts             # Rmd scripts to analyse data for main results
    └───supplement          # Rmd scripts to create the supplementary material
        ├───figures         # supplementary material figures
        ├───latex           # formatting options for supplement
        └───_bookdown_files # files generated automatically by `bookdown`
    ```

    This repository is organised to follow the structure of R packages (https://r-pkgs.org/). This allows us to write custom functions, document them, and load them locally when needed, using them as though it were actually a regular package.
    Files required by this setup are listed here:

    ```sh
    README.md           # the Readme you are now reading
    NAMESPACE           # the 'package' namespace
    DESCRIPTION         # the 'package' description including contact details
    kleptomove-ms.Rproj # the RStudio project file
    ```

5. Prepare shell scripts to run the _Kleptomove_ simulation using `scripts\xx_make_kleptomove_batchfile.R`. This creates a script which repeatedly calls the `kleptomove.exe` program with different parameter combinations, overwriting the default `bin/config.ini` file present in _Kleptomove_ as appropriate.

    This generates a `.bat` file with a time and data in the filename, for example `scripts/scripts\runs_2022-02-14_13_49_42.bat`. This, or similar files, should be copied into `Kleptomove/bin/Release/`, for it to access the simulation executable.

### Run simulation replicates

6. Run the `.bat` file from within `Kleptomove/bin/Release` (be sure to have built the `kleptomove.sln` first, to generate the `Kleptomove.exe` file!).

    The simulation should now run 3 (or some N) replicates of each parameter combination, with a terminal opening up showing progress.

    This step typically takes >24 hours, depending on the number of replicates and parameter combinations, and the the number of generations and timesteps within generations.

### Process simulation data

7. Copy the data folders generated in the `Kleptomove/data` folder into the `klepto-move-evol/data` folder.

8. Open the `.Rproj` file in RStudio, and in numbered sequence, run all the `.Rmd` files (not the `R` file to generate batch files!), and the lone Python file (Python 3.x required, 3.7 used). This process can take a few hours, especially if there are many simulation replicates or combinations of parameters.

  - We have included an `renv` lockfile (see https://rstudio.github.io/renv/articles/renv.html), which records the R packages we used and their versions. For full reproducibility, these can be used using

    ```r
    # from the R terminal
    renv::restore()
    ```

    This will install those packages with those specific versions. We do not anticipate exact versions to be important, but this is good practice.

  - We have also included a file listing the Python packages used, `python_requirements.txt`. Restore these packages using 

    ```sh
    # from the shell (not in Python!)
    pip install -r python_requirements.txt
    ```

### Make figures in the main text

9. Having opened the `.Rproj` file, run the `.Rmd` scripts in the `figure_scripts` folder, in numbered sequence, to make main text figures 1 - 6, and Supplementary Material Fig. S1.

### Render supplementary material

10. Run the `.Rmd` files in the `supplement` folder, in sequence, to generate supplementary figures. This **does not** include Figures in Sections 6 and 7.

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

The supplementary material provided with this manuscript is generated from the `supplement/` directory. A dated version rendered as PDF can be found in the directory `docs/` -- `docs/ms_kleptomove_supplementary_material_DATE.pdf`, where `DATE` is the date the manuscript was rendered.

- `supplement/spm_01_landscapes.Rmd` Code for figures 1.1 -- 1.3.

- `supplement/spm_02_weight_evolution.Rmd` Code for figures 2.1 -- 2.3.

- `supplement/figures` Figure output for the supplementary material file.

- `supplement/latex` LaTeX options for the supplementary material file.

Other files relate to formatting.

## Other Directories

- `bash/` Some useful shell scripts for output rendering.
