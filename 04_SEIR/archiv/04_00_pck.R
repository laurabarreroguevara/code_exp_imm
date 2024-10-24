# Packages
library(magrittr)
library(tidyverse)
library(glue)
library(pomp)
library(tidyverse)
library(glue)
library(patchwork)
library(foreach)
library(parallel)

# also needs pomp, mgcViz, itsadug

# Directory List
dirs = c()
dirs$processed = "../00_data/00_04_processed"
dirs$outputs = "../outputs/04_SE2I2R"

# Plotting
theme_set(theme_minimal())