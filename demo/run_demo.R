# Minimal synthetic-data demonstration for:
# "The immune response to childhood vaccines is seasonal"
#
# Run from the repository root:
#   Rscript demo/run_demo.R
#
# This demonstration uses invented antibody measurements, not GSK trial data.
# It illustrates the Gamma(log) regression, seasonal-by-region effects,
# WAIC model comparison, and Gamma-tail seroprotection calculations.
# It is NOT a reproduction of the manuscript's numerical estimates.

if (!file.exists("demo/synthetic_vaccine_data.csv")) {
  stop("Run this script from the repository root, where demo/synthetic_vaccine_data.csv exists.")
}
if (!requireNamespace("brms", quietly = TRUE) ||
    !requireNamespace("loo", quietly = TRUE) ||
    !requireNamespace("posterior", quietly = TRUE)) {
  stop("Missing packages. Open the R project and run renv::restore() first.")
}

set.seed(2026)
start_time <- Sys.time()
out_dir <- file.path("demo", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dat <- read.csv("demo/synthetic_vaccine_data.csv", stringsAsFactors = FALSE)
stopifnot(nrow(dat) == 320, all(dat$antibody_titer > 0),
          all(dat$vac_week >= 1 & dat$vac_week <= 52))
dat$study <- factor(dat$study)
dat$region <- factor(dat$region, levels = c("North", "South"))
dat$sex <- factor(dat$sex, levels = c("F", "M"))
dat$sin_week <- sin(2 * pi * dat$vac_week / 52)
dat$cos_week <- cos(2 * pi * dat$vac_week / 52)

# The main paper used a more detailed hierarchy, additional predictors,
# left-censoring, and several latitude classifications. This demo deliberately
# simplifies those features so it can run on a normal computer.
common_options <- list(
  data = dat,
  family = Gamma(link = "log"),
  chains = 2,
  cores = 2,
  iter = 1000,
  warmup = 500,
  seed = 2026,
  backend = "cmdstanr",
  refresh = 0
)

message("Fitting baseline model...")
fit_base <- do.call(brms::brm, c(
  list(formula = antibody_titer ~ age_weeks + sex + region + (1 | study)),
  common_options
))

message("Fitting seasonal-by-region model...")
fit_season <- do.call(brms::brm, c(
  list(formula = antibody_titer ~ age_weeks + sex + region +
         region:(sin_week + cos_week) + (1 | study)),
  common_options
))

# WAIC: a lower value indicates better expected predictive performance.
waic_base <- loo::waic(brms::log_lik(fit_base))
waic_season <- loo::waic(brms::log_lik(fit_season))
w_base <- unname(waic_base$estimates["waic", "Estimate"])
w_season <- unname(waic_season$estimates["waic", "Estimate"])
comparison <- data.frame(
  model = c("Baseline", "Seasonal by region"),
  WAIC = c(w_base, w_season),
  delta_WAIC_from_baseline = c(0, w_base - w_season)
)
write.csv(comparison, file.path(out_dir, "waic_comparison.csv"), row.names = FALSE)

# Posterior expected response for each vaccination week and latitude group.
newdat <- expand.grid(
  vac_week = 1:52,
  region = levels(dat$region),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
newdat$region <- factor(newdat$region, levels = levels(dat$region))
newdat$age_weeks <- 12
newdat$sex <- factor("F", levels = levels(dat$sex))
newdat$study <- factor(levels(dat$study)[1], levels = levels(dat$study))
newdat$sin_week <- sin(2 * pi * newdat$vac_week / 52)
newdat$cos_week <- cos(2 * pi * newdat$vac_week / 52)

# No random study effect is included in these illustrative predictions.
mu_draws <- brms::posterior_epred(fit_season, newdata = newdat, re_formula = NA)
newdat$predicted_mean <- colMeans(mu_draws)

# Illustrative ONLY: threshold 65 is an arbitrary assay-unit cutoff, not a
# validated serological correlate of clinical protection for any vaccine.
example_cutoff <- 65
shape_draws <- as.numeric(posterior::as_draws_df(fit_season)[["shape"]])
stopifnot(length(shape_draws) == nrow(mu_draws))
newdat$probability_above_demo_cutoff <- vapply(
  seq_len(ncol(mu_draws)),
  function(j) {
    mean(pgamma(
      q = example_cutoff,
      shape = shape_draws,
      rate = shape_draws / mu_draws[, j],
      lower.tail = FALSE
    ))
  },
  numeric(1)
)
write.csv(
  newdat[, c("vac_week", "region", "predicted_mean",
             "probability_above_demo_cutoff")],
  file.path(out_dir, "weekly_predictions.csv"),
  row.names = FALSE
)

north <- newdat[newdat$region == "North", ]
south <- newdat[newdat$region == "South", ]
png(file.path(out_dir, "seasonal_mean_response.png"), width = 950, height = 580)
plot(north$vac_week, north$predicted_mean, type = "l", lwd = 3,
     xlim = c(1, 52), ylim = range(newdat$predicted_mean),
     xlab = "Week of first vaccination", ylab = "Predicted mean antibody titer (synthetic units)",
     main = "Synthetic vaccine immunogenicity by vaccination week")
lines(south$vac_week, south$predicted_mean, lwd = 3, lty = 2)
legend("topright", legend = c("North", "South"), lty = c(1, 2), lwd = 3, bty = "n")

png(file.path(out_dir, "demo_threshold_probability.png"), width = 950, height = 580)
plot(north$vac_week, north$probability_above_demo_cutoff, type = "l", lwd = 3,
     xlim = c(1, 52), ylim = c(0, 1),
     xlab = "Week of first vaccination", ylab = "Probability above arbitrary demo cutoff",
     main = "Illustrative threshold-crossing probabilities (not clinical protection)")
lines(south$vac_week, south$probability_above_demo_cutoff, lwd = 3, lty = 2)
legend("topright", legend = c("North", "South"), lty = c(1, 2), lwd = 3, bty = "n")

capture.output(summary(fit_season), file = file.path(out_dir, "seasonal_model_summary.txt"))
capture.output(sessionInfo(), file = file.path(out_dir, "session_info.txt"))
elapsed_minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
writeLines(sprintf("Total demo runtime: %.1f minutes", elapsed_minutes),
           con = file.path(out_dir, "runtime.txt"))
message(sprintf("Finished. Results saved in %s (%.1f minutes).", out_dir, elapsed_minutes))
print(comparison)
