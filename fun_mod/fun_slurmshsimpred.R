####################################################################################################
# Function to generate SLURM shell script to execute an R model fitting job.
# This function constructs a shell script containing SLURM directives and environment setup commands 
# to run an R script (fune_sim_simpred.R) on a cluster. The script is tailored with resource 
# allocation settings, job parameters, and environment variables required by the R script.
####################################################################################################

fun_slurmshsimpred_sim <- function(args_fo = args_fo, dat_name = "data_brms_rota_vl_s08.rds", 
                              pathogen = "rota", time = "23:00:00") {
  
  # Arguments:
  # - args_fo:   (string) Formula name (e.g., bmvl_s08_rota_base).
  # - dat_name:  (string) The filename of the dataset to be used (e.g., "data_brms_rota_vl_s08.rds").
  # - pathogen:  (string) Identifier for the pathogen (e.g., "rota").
  # - time:      (string) Maximum allowed runtime for the job in HH:MM:SS format (max: "24:00:00").
  
  # Output:
  # The generated shell script is saved to a designated output directory and can be submitted to the 
  # SLURM workload manager for execution.
  
  # Build the shell script content with SLURM directives and setup commands.
  script_content <- paste0(
    "#!/bin/bash -l\n",
    "\n#SBATCH -J R_sim",
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
    "\n\nsrun R --quiet --no-save <fune_sim_simpred.R")
  
  # Construct the output file name for the shell script, incorporating the pathogen and model ID.
  file_name <- glue("{dirs$outputs}/slurmsh/runsimpred_sim_{args_fo}.sh")
  
  # Write the generated script content to the specified file.
  writeLines(script_content, file_name)
}

fun_slurmshsimpred_mod <- function(args_fo = args_fo, dat_name = "data_brms_rota_vl_s08.rds", 
                                   time = "23:00:00", nsim = 100) {
  
  # Arguments:
  # - args_fo:   (string) Formula name (e.g., bmvl_s08_rota_base).
  # - dat_name:  (string) The filename of the dataset to be used (e.g., "data_brms_rota_vl_s08.rds").
  # - time:      (string) Maximum allowed runtime for the job in HH:MM:SS format (max: "24:00:00").
  # - nsim:      (integer) Number of simulations (max: 100). 
  
  # Output:
  # The generated shell script is saved to a designated output directory and can be submitted to the 
  # SLURM workload manager for execution.
  
  # Build the shell script content with SLURM directives and setup commands.
  script_content <- paste0(
    "#!/bin/bash -l\n",
    "\n#SBATCH -J R_sim",
    "\n#SBATCH -o ./slurm/job.%j_out",
    "\n#SBATCH -e ./slurm/job.%j_err",
    "\n#SBATCH -D ./projects/_seas_imm/",
    "\n#SBATCH --ntasks=1",
    "\n#SBATCH --cpus-per-task=4",
    "\n#SBATCH --mem=120000MB",
    "\n#SBATCH --time=", time,
    "\n#SBATCH --array=1-", nsim,
    "\n\nmodule purge",
    "\nconda activate r_env",
    "\n\n# avoid overbooking of the cores which might occur via NumPy/MKL threading",
    "\nexport OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-1}",
    "\nexport arg_dat=\"", dat_name, "\"",
    "\nexport arg_fo=\"", args_fo, "\"",
    "\nexport arg_simx=${SLURM_ARRAY_TASK_ID}",
    "\n\nsrun R --quiet --no-save <fune_mod_fitsimpred.R")
  
  # Construct the output file name for the shell script, incorporating the pathogen and model ID.
  file_name <- glue("{dirs$outputs}/slurmsh/runsimpred_mod_{args_fo}.sh")
  
  # Write the generated script content to the specified file.
  writeLines(script_content, file_name)
}

####################################################################################################
# END
####################################################################################################