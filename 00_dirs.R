####################################################################################################
# Load directories
####################################################################################################

dirs <- list()
# To Kramer's server "saved zipped" data 
dirs$server_save_kramer <- "/Volumes/public-dom/kramer/gsk_data"

# To Barrero Guevaras's server main 
#dirs$seas_imm_server  <- "/Volumes/public-dom/barrero-guevara/_seas_imm_server"
#dirs$seas_imm_server  <- "/Volumes/ExternalSSD_LABG_Work/_seas_imm_backup/"
dirs$seas_imm_server  <- "/Volumes/SANDISK SSD/_seas_imm_backup/"

# To Barrero Guevaras's server "saved zipped" data 
dirs$server_save_bg   <- paste0(dirs$seas_imm_server, "/gsk_raw/gsk_data_save")
# To Barrero Guevaras's server "working unzipped" data 
dirs$server_bg        <- paste0(dirs$seas_imm_server, "/gsk_raw/gsk_data")
dirs$server_vacc_con  <- paste0(dirs$seas_imm_server, "/gsk_raw/vacc_con_data")

# To data 
dirs$raw              <- paste0(dirs$seas_imm_server, "/data/raw")
dirs$processed        <- paste0(dirs$seas_imm_server, "/data/processed")

dirs$computer <- "/Volumes/SANDISK SSD/_seas_imm_pappos"

# To outputs
#dirs$outputs          <- paste0(dirs$seas_imm_server, "/outputs")
dirs$outputs           <- paste0(dirs$computer, "/outputs")
#dirs$modfit           <- paste0(dirs$seas_imm_server, "/outputs/modfit")
dirs$modfit           <- paste0(dirs$computer, "/outputs/modfit")
#dirs$modres           <- paste0(dirs$seas_imm_server, "/outputs/modres")     
dirs$modres           <- paste0(dirs$computer, "/outputs/modres")    
#dirs$rhats            <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_rhats")
dirs$rhats            <- paste0(dirs$computer, "/outputs/modres/modres_rhats")
#dirs$waic             <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_waic")
dirs$waic             <- paste0(dirs$computer, "/outputs/modres/modres_waic")
#dirs$coef             <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_coef")
dirs$coef             <- paste0(dirs$computer, "/outputs/modres/modres_coef")
dirs$ceffects         <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_ceffects")
dirs$cespa            <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_cespa")
#dirs$pred             <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_pred")
dirs$pred             <- paste0(dirs$computer, "/outputs/modres/modres_pred")
#dirs$samples          <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_samples")
dirs$samples          <- paste0(dirs$computer, "/outputs/modres/modres_samples")
dirs$simcosfit        <- paste0(dirs$seas_imm_server, "/outputs/simcosfit")
dirs$simcosres        <- paste0(dirs$seas_imm_server, "/outputs/simcosres")
dirs$simcos_rhats     <- paste0(dirs$seas_imm_server, "/outputs/simcosres/simcosres_rhats")
dirs$simcos_coef      <- paste0(dirs$seas_imm_server, "/outputs/simcosres/simcosres_coef")
dirs$simcos_samples   <- paste0(dirs$seas_imm_server, "/outputs/simcosres/simcosres_samples")
#dirs$simpredfit        <- paste0(dirs$computer, "/outputs/simpredfit")
dirs$simpredfit        <- paste0(dirs$computer, "/outputs/simpredfit")
#dirs$simpredres        <- paste0(dirs$seas_imm_server, "/outputs/simpredres")
dirs$simpredres        <- paste0(dirs$computer, "/outputs/simpredres")
#dirs$simpred_rhats     <- paste0(dirs$seas_imm_server, "/outputs/simpredres/simpredres_rhats")
dirs$simpred_rhats     <- paste0(dirs$computer, "/outputs/simpredres/simpredres_rhats")
#dirs$simpred_coef      <- paste0(dirs$seas_imm_server, "/outputs/simpredres/simpredres_coef")
dirs$simpred_coef      <- paste0(dirs$computer, "/outputs/simpredres/simpredres_coef")
#dirs$simpred_samples   <- paste0 (dirs$seas_imm_server, "/outputs/simpredres/simpredres_samples")
dirs$simpred_samples   <- paste0(dirs$computer, "/outputs/simpredres/simpredres_samples")
dirs$climfit           <- paste0(dirs$seas_imm_server, "/outputs/climfit")
dirs$climres        <- paste0(dirs$seas_imm_server, "/outputs/climres")
dirs$clim_rhats     <- paste0(dirs$seas_imm_server, "/outputs/climres/climres_rhats")
dirs$clim_coef      <- paste0(dirs$seas_imm_server, "/outputs/climres/climres_coef")
dirs$clim_samples   <- paste0(dirs$seas_imm_server, "/outputs/climres/climres_samples")

# To local
dirs$figures <- "_figures"

# For pomp
dirs$pomp <- ""

####################################################################################################
# END
####################################################################################################