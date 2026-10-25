# Repository for the project *“The immune response to childhood vaccines is seasonal”*

This repository contains the R scripts used for data preparation, Bayesian modeling, sensitivity analyses, prediction, and figure generation for *“The immune response to childhood vaccines is seasonal”*, by Laura Andrea Barrero Guevara, Gaelle Féghali, Sarah Kramer, and Matthieu Domenech de Cellès.

## Quick start: synthetic demonstration
A self-contained synthetic-data demonstration is available in [`demo/`](demo/). It is the recommended starting point for readers who do not have access to the restricted clinical-trial data.

The demonstration fits (i) a baseline Gamma regression with a log link and (ii) a model with region-specific seasonal sine/cosine terms, both using brms. It compares their WAIC and produces weekly predictions of mean antibody response and probability of exceeding an arbitrary illustrative threshold. The 320 simulated records and eight artificial studies are not observations from the paper. The demonstration does not reproduce the paper's quantitative results or its complete censored multilevel analysis.

You can also use Rscript demo/run_demo.R in a terminal. The demo requires cmdstanr and a functioning CmdStan installation in addition to brms, loo, and posterior. It does not require access to the original data or an HPC cluster.

Validated demo execution: R 4.5.1 (Apple Silicon macOS, aarch64-apple-darwin20), with the cmdstanr backend; total elapsed time 0.9 minutes on one test run (including compilation in that session). The recorded demonstration results were WAIC 3006.868 (baseline) and 2734.023 (seasonal), so baseline minus seasonal WAIC was 272.8446. These values are from synthetic data deliberately constructed with seasonal patterns; they are not results from the manuscript. Times and results can differ across environments. See the [demo README](demo/README.md) for all expected outputs and limitations.

## Software environment and installation ##
The committed [`renv.lock`](renv.lock) records R 4.3.1, renv 1.0.2, and brms 2.20.1. These are the versions recorded for the archived project environment; they must not be confused with the separate R 4.5.1 demo test above. (R 4.3.1 was released on 2023-06-16.) Other package versions are recorded in the lockfile.

1. Clone or download this repository and open `code_exp_imm.Rproj` in RStudio (or start R in the repository root).
2. `renv` should automatically bootstrap itself by downloading and installing the appropriate version into the project library.
3. Run: `r renv::restore()`
4. For the demo, install/configure cmdstanr and CmdStan if they are not already available. This dependency is not included in the current renv.lock; see [`demo/README.md`](demo/README.md) for the additional commands.

Compilation requires a suitable C++ toolchain. The full study pipeline also uses cluster-oriented SLURM scripts; an HPC cluster is not required for the demo. Installation time depends heavily on existing libraries, compilation toolchains, and downloads; a universal installation time has not been established. The observed 0.9-minute demo runtime is not the time required to install all dependencies from scratch.

## Data access and full-analysis limitations ##
Individual-level randomized clinical-trial data were provided by GlaxoSmithKline through ClinicalStudyDataRequest.com subject to third-party data-access restrictions. These data cannot be redistributed in this repository. Reproducing the full paper analyses requires independent authorization to access the source data, as well as accompanying study-specific inputs.

## Full analysis workflow (restricted data required) ## 
The primary processing steps are organized as follows:
1. `01_metadata/`: check source-study files and gather metadata.
2. `02_dataprep/`: read and harmonize source data, assign geographic groupings, apply inclusion criteria, and construct the analysis datasets.
3. `03_mod/03_01_*`: define and fit the Bayesian regression models; SLURM scripts support cluster execution.
4. `03_mod/03_02_*`–`03_mod/03_04_*`: assess convergence and WAIC, run simulation-based checks, extract seasonal amplitude/phase, covariate effects, and comparative results.
5.` 03_mod/03_05_*` and `03_mod/03_06_*`: generate threshold predictions and simulated population vaccination-schedule scenarios.
6. `03_mod/03_07_*`: spline-based sensitivity analyses.
The paths and outputs depend on the preceding steps and unavailable restricted data; the list above is a workflow guide, not an automatically runnable script. Each .Rmd file describes its own inputs and outputs. For a fully executable example on public data, use [`demo/`](demo/).

## Repository overview ## 
Files named `fun_XX` correspond to R functions and are stored in the folders `fun_data`, `fun_mod`, and `fun_figures`. The main scripts are organized into the folders `01_metadata`, `02_dataprep`, and `03_mod`.

##### 01_metadata

This folder contains scripts to access data from the randomized controlled trials (RCTs) and explore their metadata.

- `01_01_gskstudies_savefolders`: Access requested studies, verify their receipt, and ensure that they are saved in the appropriate folders.
- `01_02_gskstudies_unzip`: Unzip study folders and save the extracted data frames as `.rds` files.
- `01_03_gskstudies_missingdataframes`: Check for missing data frames in each study.
- `01_04_metadata_gsk`: Tidy and explore metadata of GSK studies obtained directly from GSK.
- `01_05_metadata_clinicaltrialsgov`: Extract metadata from ClinicalTrials.gov into a SQLite database.
- `01_06_metadata_protocols`: Tidy metadata of GSK studies obtained directly from the available study protocols.
- `01_07_gskstudies_included`: Create a data frame of included studies and verify whether reasons for exclusion can be supported by other metadata sources.

##### 02_dataprep

This folder contains scripts to prepare the RCT data for analysis.

- `02_01_dgsk_read`: Read data from GSK studies, selecting relevant data frames (`pid`, `demo`, `dates`, `expo`, and `sero_cod`) and variables. This script includes checks to ensure that the data were tidied correctly.
- `02_02_dgsk_spatial`: Integrate spatial information, including locations and coordinates, into GSK studies. Spatial data are extracted from the GSK database (for both primary and follow-up studies), study-specific protocol data frames, and first-level administrative divisions.
- `02_03_dgsk_classification`: Define latitude-based classifications for each location and aggregate them to assign a dominant classification to each country in each study.
- `02_04_dgsk_ppat_inclusion`: Define study inclusion and exclusion for each pathogen based on the data and cross-check these decisions against protocol metadata.
- `02_05_dgsk_ppat_vacconc`: Assign vaccine concentrations by study, RCT group, and country for the included studies across pathogens.
- `02_06_dgsk_ppat`: Integrate and tidy all data by pathogen. This script combines processed participant, vaccination, and serology data with vaccine concentration, spatial, and climate data for all included studies, and then applies exclusions at the group and participant levels.
- `02_07_dgsk_datmod`: Prepare the main analysis data sets, applying further exclusions based on spatial data.
- `02_08_01_dgsk_datmod_sens`: Prepare data sets for sensitivity analyses, applying further exclusions based on spatial data.
- `02_08_01_dgsk_datmod_sens_2`: Prepare data sets for sensitivity analyses with two doses, applying further exclusions based on spatial data.
- `02_09_dgsk_descriptive`: Produce descriptive statistics and figures, including maps, histograms, and density plots, based on the final data sets. This script generates summaries of study participants, study locations, vaccination dates, immunogenicity measurements, and additional maps for selected pathogens.

##### 03_mod

This folder contains scripts for the statistical analyses.

- `03_01_01_mod_formulas`: Create lists of model formulas for each pathogen for use in Bayesian regression models with `brms`. This includes both univariate and multivariate model specifications.
- `03_01_02_mod_playground`: Run a single Bayesian regression model using `brms`. This script is intended as a playground to verify that a model can be executed successfully.
- `03_01_03_mod_slurm`: Generate SLURM shell scripts (`slurm.sh`) for fitting models on a computing cluster.
- `03_02_01_modres_rhats_waic`: Evaluate model convergence and WAIC.
- `03_02_02_modres_r2`: Evaluate the R² of the best-fitting models.
- `03_02_03_modres_coef`: Extract, inspect, and plot model coefficients.
- `03_03_01_simpred_simulate`: Assess model simulations using `posterior_predict`.
- `03_03_02_simpred_slurm`: Generate SLURM shell scripts for simulation-based model fitting on a computing cluster.
- `03_03_03_simpredres`: Evaluate statistical power and bias from simulations.
- `03_04_01_modres_ampphi`: Estimate and plot the amplitude and phase of the best-fitting models.
- `03_04_02_modres_covars`: Extract and plot covariate effects.
- `03_04_03_compare`: Compare model results across specifications.
- `03_05_01_modres_ve`: To check the predicted seroprotection from the models 
- `03_06_01_modresimm_population`: To estimate immunogenicity for different vaccination ages and all vaccination weeks, keeping posterior draws for the population scenarios
- `03_06_02_modresimm_population_scenarions`:  To estimate population mean immunogenicity under different vaccination ages and a targeted delay during the valley season
- `03_05_01_modres_splines`: Evaluate and plot results from spline-based sensitivity analyses.

## Functions and additional scripts ##

##### fun_data

This folder contains functions for data extraction and processing.

- `fun_data_coord1lv`: Extract first-level administrative divisions of a country and the longitude and latitude coordinates of their capital cities.
- `fun_data_kgc`: Extract the Köppen–Geiger climate classification for a spatial point.
- `fun_dgsk_exceptions`: Handle study-specific data-frame exceptions.
- `fun_dgsk_modpat`: Create data frames for mono-/multi-pathogen and mono-/multivariate models.
- `fun_dgsk_readdfs`: Read all or selected data frames from GSK studies.
- `fun_dgsk_tidydates`: Convert GSK date formats into R date formats.
- `fun_dgsk_tidydfs`: Tidy participant (`pid`), vaccination (`expo`), and immunogenicity (`sero`) data frames.
- `fun_dgsk_tidynames`: Tidy country, vaccine, and serology names.
- `fun_dgsk_vacconc`: Add vaccine concentrations, including study-specific exceptions.
- `fun_dgsk_vacconco`: Create a data frame of concomitant vaccines.
- `fun_dgsk_vacimm`: Process and tidy immunogenicity data relative to vaccine doses, including time differences, baseline values, and censoring status.

##### fun_figures

This folder contains functions for figure creation.

- `fun_breaks`: Set aesthetically useful upper, middle, and lower breaks in `ggplot`.
- `fun_figures`: Generate descriptive figures.
- `fun_figures_waic`: Plot WAIC results.
- `fun_limits`: Set aesthetically useful upper and lower plotting limits in `ggplot`.

##### fun_mod

This folder contains functions for model fitting and result analysis.

- `fun_ampphi`: Estimate amplitude from cosine and sine coefficients.
- `fun_checkgaps`: Check gaps in a 52-week cycle.
- `fun_formula`: Generate `brms` formulas.
- `fun_formula_add`: Add a new formula to a list while ensuring that neither the formula nor its name is duplicated.
- `fun_slurmsh`: Generate a SLURM shell script to run an R model-fitting job.
- `fun_slurmshsimpred`: Generate a SLURM shell script to run simulation-based R model-fitting jobs.
- `fun_venewdata`: Functions to obtain vaccine seroprotection new data.

## License ##

The source code in this repository is distributed under the
MIT License. See the [LICENSE](LICENSE) file for details.

The clinical trial data are subject to separate access
restrictions and are not covered by this license.



  
