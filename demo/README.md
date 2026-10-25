# Minimal synthetic-data demonstration

This folder accompanies *“The immune response to childhood vaccines is seasonal.”* It provides a small, runnable example of part of the statistical workflow without distributing restricted clinical-trial data. **All 320 participant records are synthetic**, drawn from eight artificial studies (four per latitude group).

## Contents

- `synthetic_vaccine_data.csv`: invented age, sex, vaccination-week, latitude-group, study, and antibody-response values.
- `run_demo.R`: fits a baseline and a seasonal-by-region Bayesian Gamma regression with a log link using `brms`; compares WAIC; and produces weekly predictions of expected response and probability of exceeding an arbitrary cutoff.
- `output/`: created locally when the script runs; contains numerical summaries, figures, and runtime information. The user does not need pre-generated output files to run the demonstration.

## Requirements and installation

- R; the demo was **tested using R 4.5.1** on Apple Silicon macOS (`aarch64-apple-darwin20`).
- R packages `brms`, `loo`, `posterior`, and the **`cmdstanr` backend**; a working CmdStan installation and suitable C++ toolchain are required.
- The repository [`renv.lock`](../renv.lock) records an older project environment (R 4.3.1, `brms` 2.20.1, `renv` 1.0.2). **It does not currently record `cmdstanr`.** The demo test under R 4.5.1 is distinct from the archived full-analysis package environment. Exact package versions used in the successful demo run can be obtained from `demo/output/session_info.txt`, or the R commands below. Do not assume the demo was tested under R 4.3.1.

Open `code_exp_imm.Rproj` from the repository root. If necessary, restore the archived project R packages in the R console:

```r
renv::restore()
```

Install `cmdstanr` and CmdStan separately if they are not already installed in the active R project environment:

```r
install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
cmdstanr::check_cmdstan_toolchain()
cmdstanr::install_cmdstan(cores = 2)
cmdstanr::cmdstan_version()
```

CmdStan installation is a one-time setup. Installation time is **not** included in the demo's measured execution time. The full paper used additional software, inputs, and high-performance cluster scripts; no HPC cluster or restricted data are needed for this demonstration.

## Run

From the repository root, in the R console:

```r
source("demo/run_demo.R")
```

Or from a terminal in the repository root:

```bash
Rscript demo/run_demo.R
```

The working directory must be the repository root because the script reads `demo/synthetic_vaccine_data.csv` using a relative path. The script compiles and samples two Stan models (2 chains, 1,000 iterations per chain, 500 warmup iterations per chain) and writes results to `demo/output/`.

## Expected output

A successful run creates:

| File | Meaning |
|---|---|
| `output/waic_comparison.csv` | WAIC for baseline and seasonal models; difference is baseline WAIC minus seasonal WAIC. |
| `output/weekly_predictions.csv` | Weekly expected synthetic antibody titers and probabilities above an arbitrary cutoff, for both latitude groups. |
| `output/seasonal_mean_response.png` | Curves of mean synthetic response by vaccination week. |
| `output/demo_threshold_probability.png` | Probability of exceeding **65 arbitrary synthetic units**; not a validated protection threshold. |
| `output/seasonal_model_summary.txt` | `brms` summary of the seasonal model. |
| `output/session_info.txt` | R version, platform, and loaded package versions. |
| `output/runtime.txt` | Elapsed time from starting the script through saving outputs. |

### Verified test execution (2026-10-25)

The demonstration completed on **Apple Silicon macOS**, using **R 4.5.1** (`aarch64-apple-darwin20`) and `brms` with `backend = "cmdstanr"`. Both models compiled/sampled and the output files were generated.

The user-reported results were:

| Model | WAIC |
|---|---:|
| Baseline | 3006.868 |
| Seasonal by region | 2734.023 |

**Baseline minus seasonal WAIC = 272.8446** (rounded); measured elapsed time **0.9 minutes**, including compilation in that run. The synthetic data were constructed with seasonal responses, so a strong preference for the seasonal model was expected. Results and runtimes can vary across installations.

## Scope and restrictions

The illustration covers Gamma-log Bayesian modeling, seasonal interaction terms, model comparison, and Gamma upper-tail probabilities. It deliberately omits the manuscript's restricted participant data, left-censoring, comprehensive multilevel hierarchy, multiple assays/pathogens, full latitude schemes, and vaccination-schedule simulations. **The predicted threshold probabilities are not estimates of vaccine effectiveness or clinically validated seroprotection.** The demo does not independently reproduce the manuscript's findings.

The original data were supplied through [ClinicalStudyDataRequest.com](https://www.clinicalstudydatarequest.com/) under access restrictions. Researchers seeking full numerical reproduction must obtain the original datasets independently and configure the full pipeline's project-specific directories.
