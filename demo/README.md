# Minimal synthetic-data demonstration

This folder accompanies **“The immune response to childhood vaccines is seasonal.”** It provides a small, runnable example of the statistical workflow, without distributing restricted clinical trial data.

## Contents

- `synthetic_vaccine_data.csv`: 320 **entirely synthetic** records from 8 artificial studies (4 assigned to each of two illustrative latitude groups), with vaccination week, age, sex, and antibody titers.
- `run_demo.R`: fits a baseline and a region-specific seasonal Bayesian Gamma regression with a log link using `brms`, compares WAIC, and plots weekly predicted antibody concentrations and probabilities of exceeding an **arbitrary illustrative** threshold.

## Requirements

- R and packages `brms`, `loo`, `posterior` (plus their dependencies) and a working Stan/C++ toolchain. These packages are managed using the repository's `renv.lock`; run `renv::restore()` from the project root.
- No original clinical trial data or HPC cluster is required for **this demo**. The full paper pipeline uses additional files and cluster-oriented scripts.
- The lockfile currently records R 4.3.1, `brms` 2.20.1 and `renv` 1.0.2; verify these against the actual environment used for the final paper before claiming them as the tested versions.

## Run

1. Download or clone the repository and open `code_exp_imm.Rproj` in RStudio, or start a terminal in the repository root.
2. Install/restore dependencies if necessary: in R, run `renv::restore()`.
3. From the **repository root**, run:

   ```bash
   Rscript demo/run_demo.R
   ```

   Alternatively, from the R console in the repository root:

   ```r
   source("demo/run_demo.R")
   ```

The first run may need to compile two Stan models; subsequent runs may be faster. A working C++ compiler is needed. The exact runtime depends on the computer and was **not benchmarked** when preparing these files. The script records the measured runtime in `demo/output/runtime.txt`.

## Expected output

The script creates `demo/output/` containing:

- `waic_comparison.csv`: WAIC for the baseline and seasonal models, including a difference relative to baseline; lower WAIC indicates preferred predictive performance.
- `weekly_predictions.csv`: predicted mean synthetic antibody titers and probability of exceeding the illustrative cutoff for 52 weeks in each latitude group.
- `seasonal_mean_response.png`: fitted seasonal response curves for the two latitude groups, approximately opposite in phase because this pattern was **built into the synthetic data**.
- `demo_threshold_probability.png`: threshold-crossing curves for an **arbitrary cutoff of 65 synthetic units**. These **do not estimate vaccine effectiveness or clinically validated seroprotection**.
- `seasonal_model_summary.txt`, `session_info.txt`, `runtime.txt`.

The example is designed to favor a seasonal model, but actual WAIC values and posterior predictions will vary with the MCMC run. Check diagnostic warnings and convergence before interpreting results. The demo is intentionally simplified: it does **not** reproduce the paper's datasets, figures, censored likelihood, full multilevel structure, multiple assays, or complete vaccine-scheduling simulations.

## Data restrictions

The actual individual-level clinical trial data were obtained through ClinicalStudyDataRequest.com under access restrictions. They are **not** present here, and cannot be redistributed through this repository. Full reproduction of the manuscript analyses requires separately approved data access and project-specific input-path configuration.

## Verification status

This demo's files and CSV data structure have been checked for consistency, but the R model fitting and runtime have **not** been executed in this preparation environment. Before marking the Nature checklist's demo, expected-output, and runtime boxes as complete, run `Rscript demo/run_demo.R` successfully on a machine with R and a Stan toolchain, and record the measured time and operating system here.
