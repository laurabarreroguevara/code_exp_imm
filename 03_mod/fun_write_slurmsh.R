################################################################################
# Write slurm sh file for each model
################################################################################

fun_write_slurmsh <- function(args_fo = args_fo, args_iter = args_iter, 
                              dat_name = "data_brms_1dose_ii.rds", pathogen = "varicella",
                              time = "23:00:00") {
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
    "\nexport arg_dat=\"", dat_name, "\"",
    "\nexport arg_fo=\"", args_fo, "\"",
    "\nexport arg_iter=\"", args_iter, "\"",
    "\n\nsrun R --quiet --no-save <fune_mod_fit.R")
  # Create a new shell script file with the argument name
  file_name <- glue("{dirs$outputs}/slurm_sh/run_{pathogen}_brms_{args_fo}.sh")
  writeLines(script_content, file_name)
}

################################################################################
# End
################################################################################