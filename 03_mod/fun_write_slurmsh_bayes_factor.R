################################################################################
# Write slurm sh file for each model
################################################################################

fun_write_slurmsh_bayes_factor <- function(pathogen = "varicella", dfkgc = "", time = "23:00:00") {
  script_content <- paste0(
    "#!/bin/bash -l\n",
    "\n#SBATCH -J R",
    "\n#SBATCH -o ./slurm/job.%j_out",
    "\n#SBATCH -e ./slurm/job.%j_err",
    "\n#SBATCH -D ./projects/_seas_imm/",
    "\n#SBATCH --ntasks=1",
    "\n#SBATCH --cpus-per-task=4",
    "\n#SBATCH --mem=120000MB",
    "\n#SBATCH --time=", time,
    "\n\nmodule purge",
    "\nconda activate r_env",
    "\n\n# avoid overbooking of the cores which might occur via NumPy/MKL threading",
    "\nexport OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-1}",
    "\n\nexport arg_pat=\"", pathogen, "\"",
    "\n\nexport arg_df=\"", dfkgc, "\"",
    "\n\nsrun R --quiet --no-save <fune_mod_bayes_factor.R")
  # Create a new shell script file with the argument name
  file_name <- paste0("../outputs/slurm_sh/run_mcom_", pathogen, ".sh")
  writeLines(script_content, file_name)
}

################################################################################
# End
################################################################################