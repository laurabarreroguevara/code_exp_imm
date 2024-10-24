####################################################################################################
# Function to add vaccine concentrations that have exceptions 
####################################################################################################

fun_dgsk_vacconc <- function(dat = dat, pat = pat) {
  # Arguments:
  # - dat: (data frame) A data frame with at least three columns: study, rct_group, and country.
  # - pat: (string) Vaccine pathogen.
  # Output:
  # - dat: (data frame) The input data frame with vaccine concentrations.
  
  # Rotavirus vaccine concentrations ---------------------------------------------------------------
  if(pat == "rota") {
    dat <- dat %>%
    mutate(vac_conc = case_when(
      study == "101555" & str_detect(rct_group, "placebo") ~ 0,
      study == "101555" & str_detect(rct_group, "vaccine") ~ 6.5,
      study == "102247" & str_detect(rct_group, "placebo") ~ 0,
      study == "102247" & str_detect(rct_group, "hrv") ~ 6.5,
      study == "102248" & str_detect(rct_group, "placebo") ~ 0,
      study == "102248" & str_detect(rct_group, "hrv") ~ 6.5,
      study == "103477" & str_detect(rct_group, "placebo") ~ 0,
      study == "103477" & str_detect(rct_group, "hrv") ~ 6.5,
      study == "103478" & (str_detect(rct_group, "rvc20a41pl")|is.na(rct_group)) ~ 0,
      study == "103478" & str_detect(rct_group, "rvc019a43") ~ 6.0,
      study == "103792" & str_detect(rct_group, "placebo") ~ 0,
      study == "103792" & str_detect(rct_group, "hrv") ~ 6.5,
      study == "103992" & str_detect(rct_group, "placebo") ~ 0,
      study == "103992" & str_detect(rct_group, "hrv +|hrv -") ~ 6.5,
      study == "104021" & str_detect(rct_group, "placebo|engerix") ~ 0,
      study == "104021" & str_detect(rct_group, "rotarix") ~ 6.5,
      study == "104480" & str_detect(rct_group, "prola006a|rvc20a41pl") ~ 0,
      study == "104480" & str_detect(rct_group, "drola009a|rvc021a44") ~ 6.0,
      study == "105722" & rct_group == "placebo" ~ 0,
      study == "105722" &  str_detect(rct_group, "hrv") ~ 6.5,
      study == "106260" & rct_group == "placebo" ~ 0,
      study == "106260" & rct_group == "hrv" ~ 6.0,
      study == "107077" & rct_group == "hrv lyo" ~ 6.5,
      study == "107077" & rct_group == "hrv liq" ~ 6.4,
      study == "107625" & str_detect(rct_group, "placebo") ~ 0,
      study == "107625" & str_detect(rct_group, "hrv") ~ 6.0,
      study == "109216" & str_detect(rct_group, "placebo/placebo/placebo") ~ 0,
      study == "109216" & str_detect(rct_group, "hrv") ~ 6.5,
      study == "112269" & str_detect(rct_group, "placebo") ~ 0,
      study == "112269" & str_detect(rct_group, "hrv") ~ 6.0,
      study == "113518" & str_detect(rct_group, "placebo") ~ 0,
      study == "113518" & str_detect(rct_group, "rotavirus") ~ 6.0,
      study == "113808" & (str_detect(rct_group, "placebo")|is.na(rct_group)) ~ 0,
      study == "113808" & str_detect(rct_group, "hrv") ~ 6.0,
      study == "444563-003" & str_detect(rct_group, "placebo") ~ 0,
      study == "444563-003" & str_detect(rct_group, "4.1") ~ 4.1,
      study == "444563-003" & str_detect(rct_group, "4.7") ~ 4.7,
      study == "444563-003" & str_detect(rct_group, "5.8") ~ 5.8,
      study == "444563-004" & str_detect(rct_group, "placebo") ~ 0,
      study == "444563-004" & str_detect(rct_group, "4.7") ~ 4.7,
      study == "444563-005" & str_detect(rct_group, "placebo") ~ 0,
      study == "444563-005" & str_detect(rct_group, "5.2") ~ 5.2,
      study == "444563-005" & str_detect(rct_group, "6.4") ~ 6.4,
      study == "444563-007" & str_detect(rct_group, "drvc006a46pl") ~ 0,
      study == "444563-007" & str_detect(rct_group, "drvc005a46") ~ 4.7,
      study == "444563-007" & str_detect(rct_group, "drvc010a48") ~ 5.2,
      study == "444563-007" & str_detect(rct_group, "drvc013a48") ~ 6.1,
      study == "444563-013" & str_detect(rct_group, "3 pl") ~ 0,
      study == "444563-013" & str_detect(rct_group, "3 vacc|2 vacc") ~ 6.5,
      study == "444563-014" & str_detect(rct_group, "hrv placebo") ~ 0,
      study == "444563-014" & str_detect(rct_group, "hrv10e5.2") ~ 5.0, # In protocol 5.0
      study == "444563-020" & str_detect(rct_group, "drvc201a48pl") ~ 0,
      study == "444563-020" & str_detect(rct_group, "drvc200a48") ~ 5.6,
      study == "444563-020" & str_detect(rct_group, "drvc202a48") ~ 6.6,
      study == "444563-020" & str_detect(rct_group, "drvc203a48") ~ 7.0,
      study == "444563-021" & str_detect(rct_group, "a48pl") ~ 0,
      study == "444563-021" & str_detect(rct_group, "drvc010a48") ~ 5.0,
      study == "444563-021" & str_detect(rct_group, "drvc202a48") ~ 5.2,
      study == "444563-033" & str_detect(rct_group, "placebo rota") ~ 0,
      study == "444563-033" & str_detect(rct_group, "rvc019|rvc018|rvc021") ~ 6.5,
      .default = vac_conc))
  }
  
  # Varicella vaccine concentrations ---------------------------------------------------------------
  if(pat == "vzv") {
    dat <- dat %>%
    mutate(vac_conc = case_when(
      study %in% c("110058") & is.na(rct_group) ~ 0,
      study == "100388" & str_detect(rct_group, "amjrb194az") ~ 0,
      study == "100388" & str_detect(rct_group, "amrva004") ~ 3.7,
      study == "100388" & str_detect(rct_group, "amrva005|avarb049") ~ 4.1,
      study == "100388" & str_detect(rct_group, "amrva006|avarb021|avarb038") ~ 4.2,
      study == "103388" & str_detect(rct_group, "priorix") ~ 0,
      study == "103388" & str_detect(rct_group, "memuru-oka") ~ 3.3,
      study == "104020" & str_detect(rct_group, "lot a") ~ 4.4,
      study == "104020" & str_detect(rct_group, "lot b") ~ 3.5,
      study == "104020" & str_detect(rct_group, "varilrix") ~ 3.3,
      study == "104389" & str_detect(rct_group, "lot a") ~ 3.7,
      study == "104389" & str_detect(rct_group, "lot b") ~ 4.1,
      study == "104389" & str_detect(rct_group, "lot c|varilrix") ~ 4.2,
      study == "109705" & rct_group == "varilrix" ~ 4.4,
      study == "109705" & rct_group == "varilrix hsa free" ~ 4.3,
      study == "110058" & str_detect(rct_group, "mmrv") ~ 4.1,
      study == "110058" & str_detect(rct_group, "proquad") ~ 3.99,
      study == "208136-006" & str_detect(rct_group, "mjrv121a42") ~ 4.0,
      study == "208136-006" & str_detect(rct_group, "mjr162a42") & 
        str_detect(country, "Germany|Estonia") ~ 4.2,
      study == "208136-006" & str_detect(rct_group, "mjr162a42") & 
        str_detect(country, "Belgium") ~ 4.5,
      study == "208136-007" & str_detect(rct_group, "mjrv134a42") ~ 4.1,
      study == "208136-007" & str_detect(rct_group, "mjrv135a42") ~ 4.5,
      study == "208136-007" & str_detect(rct_group, "mjrv136a43") ~ 4.6,
      study == "208136-007" & str_detect(rct_group, "va235a41") & 
        str_detect(country, "Belgium") ~ 4.4,
      study == "208136-007" & str_detect(rct_group, "va235a41") & 
        str_detect(country, "Belgium", negate = T) ~ 4.1,
      study == "208136-013" & str_detect(rct_group, "memuru-oka", negate = TRUE) ~ 0,
      study == "208136-013" & str_detect(rct_group, "memuru-oka") ~ 3.3,
      study == "208136-016" & str_detect(rct_group, "mjr318a4") ~ 0,
      study == "208136-016" & str_detect(rct_group, "mjrv133a") ~ 3.3,
      study == "208136-016" & str_detect(rct_group, "mjrv136a") ~ 4.6,
      study == "208136-038" & str_detect(rct_group, "mjrv159a48|mjrv160a48") ~ 3.7,
      study == "208136-038" & str_detect(rct_group, "mjrv163a48") ~ 3.8,
      study == "208136-038" & str_detect(rct_group, "mjr622a44") ~ 4.5,
      .default = vac_conc)) 
  }
  
  # Measles vaccine concentrations -----------------------------------------------------------------
  if(pat == "measl") {
    dat <- dat %>%
    mutate(vac_conc = case_when(
      study %in% c("110058", "111870", "115650") & is.na(rct_group) ~ 0,
      study == "100388" & str_detect(rct_group, "amrva004az") ~ 3.8,
      study == "100388" & str_detect(rct_group, "amrva005az") ~ 3.6,
      study == "100388" & str_detect(rct_group, "amrva006az|amjrb194az|varilrix") ~ 3.7,
      study == "104020" & str_detect(rct_group, "lot a") ~ 3.5,
      study == "104020" & str_detect(rct_group, "lot b") ~ 3.1,
      study == "104020" & str_detect(rct_group, "priorix") ~ 3.0,
      study == "104389" & str_detect(rct_group, "lot a") ~ 3.8,
      study == "104389" & str_detect(rct_group, "lot b") ~ 3.6,
      study == "104389" & str_detect(rct_group, "lot c") ~ 3.7,
      study == "104389" & str_detect(rct_group, "varilrix") ~ 4.0,
      study == "110058" & str_detect(rct_group, "mmrv") ~ 4.1,
      study == "110058" & str_detect(rct_group, "proquad") ~ 3.0,
      study == "111870" & str_detect(rct_group, "mmr ii lot 1") ~ 4.0,
      study == "111870" & str_detect(rct_group, "mmr ii lot 2") ~ 3.9,
      study == "111870" & str_detect(rct_group, "mmr ii lot 3") ~ 3.8,
      study == "111870" & str_detect(rct_group, "amjrb721a lot 1") ~ 3.8,
      study == "111870" & str_detect(rct_group, "dmjra002a lot 2") ~ 4.1,
      study == "111870" & str_detect(rct_group, "dmjra003a lot 3") ~ 4.0,
      study == "115649" & str_detect(rct_group, "not treated|screen failure|not assigned") ~ 0,
      study == "115649" & str_detect(rct_group, "inv_mmr_min") ~ 3.1,
      study == "115649" & str_detect(rct_group, "inv_mmr_med") ~ 3.4,
      study == "115649" & str_detect(rct_group, "com_mmr") ~ 3.0, 
      study == "115650" & str_detect(rct_group, "inv mmr") ~ 4.5,
      study == "115650" & str_detect(rct_group, "com mmr") ~ 3.0,
      study == "208136-006" & str_detect(rct_group, "mjrv121a42") ~ 3.5,
      study == "208136-006" & str_detect(rct_group, "mjr162a42") ~ 3.8,
      study == "208136-007" & str_detect(rct_group, "mjrv134a42") ~ 4.4,
      study == "208136-007" & str_detect(rct_group, "mjrv135a42") ~ 4.1,
      study == "208136-007" & str_detect(rct_group, "mjrv136a43") ~ 3.7,
      study == "208136-007" & str_detect(rct_group, "priorix") & 
        str_detect(country, "Belgium") ~ 3.9,
      study == "208136-007" & str_detect(rct_group, "priorix") & 
        str_detect(country, "Belgium", negate = T) ~ 4.2,
      study == "208136-013" & str_detect(rct_group, "memuru-oka", negate = TRUE) ~ 0,
      study == "208136-013" & str_detect(rct_group, "memuru-oka") ~ 3.0,
      study == "208136-016" & str_detect(rct_group, "mjrv133a") ~ 3.2,
      study == "208136-016" & str_detect(rct_group, "mjrv136a") ~ 3.7,
      study == "208136-016" & str_detect(rct_group, "priorix") ~ 4.2,
      study == "208136-038" & str_detect(rct_group, "mjrv159a48") ~ 3.7,
      study == "208136-038" & str_detect(rct_group, "mjrv160a48") ~ 3.8,
      study == "208136-038" & str_detect(rct_group, "mjrv163a48") ~ 3.9,
      study == "208136-038" & str_detect(rct_group, "mjr622a44") ~ 4.0,
      study == "209762-136" & str_detect(rct_group, "1") ~ 3.6,
      study == "209762-136" & str_detect(rct_group, "2") ~ 3.8,
      study == "209762-136" & str_detect(rct_group, "3") ~ 3.5,
      study == "209762-136" & str_detect(rct_group, "4") ~ 3.0,
      .default = vac_conc)) 
  }
  
  # Mumps vaccine concentrations -------------------------------------------------------------------
  if(pat == "mumps") {
    dat <- dat %>%
    mutate(vac_conc = case_when(
      study %in% c("110058", "111870", "115650") & is.na(rct_group) ~ 0,
      study == "100388" & str_detect(rct_group, "amrva004az") ~ 5,
      study == "100388" & str_detect(rct_group, "amrva006az") ~ 4.8,
      study == "100388" & str_detect(rct_group, "varilrix|amjrb194az|amrva005az") ~ 4.9,
      study == "103388" & str_detect(rct_group, "memuru-oka") ~ 4.4,
      study == "103388" & str_detect(rct_group, "priorix") ~ 3.7,
      study == "104020" & str_detect(rct_group, "fresh") ~ 4.8,
      study == "104020" & str_detect(rct_group, "aged") ~ 4.4,
      study == "104020" & str_detect(rct_group, "priorix") ~ 3.7,
      study == "104389" & str_detect(rct_group, "lot a") ~ 5,
      study == "104389" & str_detect(rct_group, "lot b") ~ 4.9,
      study == "104389" & str_detect(rct_group, "lot c") ~ 4.8,
      study == "104389" & str_detect(rct_group, "priorix") ~ 4.6,
      # study == "105908" & str_detect(rct_group, "priorix") ~ 3.7,
      # study == "105908" & str_detect(rct_group, "mmrv") ~ 4.4,
      study == "109995" & str_detect(rct_group, "mmrv") ~ 4.4,
      study == "109995" & str_detect(rct_group, "priorix") ~ 3.7,
      study == "110058" & str_detect(rct_group, "mmrv") ~ 5.3,
      study == "110058" & str_detect(rct_group, "proquad") ~ 4.3,
      study == "110876" & str_detect(rct_group, "mmrv") ~ 4.4,
      study == "110876" & str_detect(rct_group, "priorix") ~ 3.7,
      study == "111870" & str_detect(rct_group, "mmr ii|amjrb721a lot 1") ~ 4.8,
      study == "111870" & str_detect(rct_group, "dmjra002a lot 2") ~ 4.1,
      study == "111870" & str_detect(rct_group, "dmjra003a lot 3") ~ 3.7,
      study == "115648" & str_detect(rct_group, "not treated|not assigned") ~ 0,
      study == "115648" & str_detect(rct_group, "inv_mmr") ~ 4.3,
      study == "115648" & str_detect(rct_group, "com_mmr") ~ 4.1,
      study == "115649" & str_detect(rct_group, "not treated|screen failure|not assigned") ~ 0,
      study == "115649" & str_detect(rct_group, "inv_mmr_min") ~ 4.1,
      study == "115649" & str_detect(rct_group, "inv_mmr_med") ~ 4.3,
      study == "115649" & str_detect(rct_group, "com_mmr") ~ 4.1,
      study == "115650" & str_detect(rct_group, "inv mmr") ~ 5.7,
      study == "115650" & str_detect(rct_group, "com mmr") ~ 4.1,
      study == "208136-007" & str_detect(rct_group, "mjrv134a42") ~ 4.4,
      study == "208136-007" & str_detect(rct_group, "mjrv135a42") ~ 4.6,
      study == "208136-007" & str_detect(rct_group, "mjrv136a43") ~ 4.3,
      study == "208136-007" & str_detect(rct_group, "priorix") & 
        str_detect(country, "Belgium") ~ 4.4,
      study == "208136-007" & str_detect(rct_group, "priorix") & 
        str_detect(country, "Belgium", negate = T) ~ 4.5,
      study == "208136-013" & str_detect(rct_group, "memuru-oka", negate = TRUE) ~ 0,
      study == "208136-013" & str_detect(rct_group, "memuru-oka") ~ 3.7,
      study == "208136-016" & str_detect(rct_group, "mjrv133a") ~ 3.4,
      study == "208136-016" & str_detect(rct_group, "mjrv136a") ~ 4.3,
      study == "208136-016" & str_detect(rct_group, "priorix") ~ 4.5,
      study == "208136-038" & str_detect(rct_group, "mjrv159a48") ~ 4.8,
      study == "208136-038" & str_detect(rct_group, "mjrv160a48") ~ 4.6,
      study == "208136-038" & str_detect(rct_group, "mjrv163a48") ~ 4.9,
      study == "208136-038" & str_detect(rct_group, "mjr622a44") ~ 4.7,
      study == "209762-136" & str_detect(rct_group, "1") ~ 4.8,
      study == "209762-136" & str_detect(rct_group, "2") ~ 4.7,
      study == "209762-136" & str_detect(rct_group, "3") ~ 4.5,
      study == "209762-136" & str_detect(rct_group, "4") ~ 4.3,
      study == "209762-151" & str_detect(rct_group, "mmrii") ~ 4.3,
      study == "209762-151" & str_detect(rct_group, "priorix") ~ 3.7,
      .default = vac_conc)) 
  }
  
  # Rubella vaccine concentrations -----------------------------------------------------------------
  if(pat == "rubel") {
    dat <- dat %>%
    mutate(vac_conc = case_when(
      study %in% c("110058", "111870", "115650") & is.na(rct_group) ~ 0,
      study == "100388" & str_detect(rct_group, "amrva004az") ~ 3.8,
      study == "100388" & str_detect(rct_group, "amrva005az|amrva006az") ~ 3.7,
      study == "100388" & str_detect(rct_group, "varilrix|amjrb194az") ~ 4.0,
      study == "104020" & str_detect(rct_group, "lot a") ~ 3.7,
      study == "104020" & str_detect(rct_group, "lot b") ~ 3.7,
      study == "104020" & str_detect(rct_group, "priorix") ~ 3.0,
      study == "104389" & str_detect(rct_group, "lot a") ~ 3.8,
      study == "104389" & str_detect(rct_group, "lot b|lot c") ~ 3.7,
      study == "104389" & str_detect(rct_group, "priorix") ~ 4.2,
      study == "110058" & str_detect(rct_group, "mmrv") ~ 4,
      study == "110058" & str_detect(rct_group, "proquad") ~ 3,
      study == "111870" & str_detect(rct_group, "mmr ii lot 1") ~ 4.2,
      study == "111870" & str_detect(rct_group, "mmr ii lot 2") ~ 4,
      study == "111870" & str_detect(rct_group, "lot 3") ~ 4.1,
      study == "111870" & str_detect(rct_group, "amjrb721a|dmjra002a") ~ 3.9,
      study == "115649" & str_detect(rct_group, "not treated|screen failure|not assigned") ~ 0,
      study == "115649" & str_detect(rct_group, "inv_mmr_min") ~ 2.9,
      study == "115649" & str_detect(rct_group, "inv_mmr_med") ~ 3.2,
      study == "115649" & str_detect(rct_group, "com_mmr") ~ 3.0,
      study == "115650" & str_detect(rct_group, "inv mmr") ~ 4.4,
      study == "115650" & str_detect(rct_group, "com mmr") ~ 3.0,
      study == "208136-006" & str_detect(rct_group, "mjrv121a42") ~ 3.6,
      study == "208136-006" & str_detect(rct_group, "mjr162a42") & 
        str_detect(country, "Belgium") ~ 3.9,
      study == "208136-006" & str_detect(rct_group, "mjr162a42") & 
        str_detect(country, "Belgium", negate = T) ~ 4,
      study == "208136-007" & str_detect(rct_group, "mjrv134a42") ~ 3.9,
      study == "208136-007" & str_detect(rct_group, "mjrv135a42") ~ 3.5,
      study == "208136-007" & str_detect(rct_group, "mjrv136a43") ~ 3.8,
      study == "208136-007" & str_detect(rct_group, "priorix") & 
        str_detect(country, "Belgium") ~ 3.9,
      study == "208136-007" & str_detect(rct_group, "priorix") & 
        str_detect(country, "Belgium", negate = T) ~ 4,
      study == "208136-013" & str_detect(rct_group, "memuru-oka", negate = TRUE) ~ 0,
      study == "208136-013" & str_detect(rct_group, "memuru-oka") ~ 3.0,
      study == "208136-016" & str_detect(rct_group, "mjrv133a") ~ 3.2,
      study == "208136-016" & str_detect(rct_group, "mjrv136a") ~ 3.8,
      study == "208136-016" & str_detect(rct_group, "priorix") ~ 4,
      study == "208136-038" & str_detect(rct_group, "mjrv") ~ 4,
      study == "208136-038" & str_detect(rct_group, "mjr622a44") ~ 4.3,
      study == "209762-136" & str_detect(rct_group, "1") ~ 3.8,
      study == "209762-136" & str_detect(rct_group, "2") ~ 3.9,
      study == "209762-136" & str_detect(rct_group, "3") ~ 3.9,
      study == "209762-136" & str_detect(rct_group, "4") ~ 3.0,
      .default = vac_conc)) 
  }
  
  # Hav vaccine concentrations ---------------------------------------------------------------------
  if(pat == "hav") {
    dat <- dat
  }
  
  # Polio vaccine concentrations -------------------------------------------------------------------
  if(pat == "polio") {
    dat <- dat %>%
    mutate(vac_conc = case_when(
      study == "101858" & str_detect(rct_group, "menomune") ~ 0,
      study == "101858" & str_detect(rct_group, "ipv") ~ 40,
      study == "107007" & str_detect(rct_group, "6-10-14") ~ 6.0,
      study == "107007" & str_detect(rct_group, "2-4-6") ~ 40,
      study == "107058" & str_detect(rct_group, "7-11|12-23|24") ~ 0,
      study == "107058" & str_detect(rct_group, "< 6 months") ~ 40,
      study == "112679" & rct_group == "_na" ~ 0,
      study == "112679" & str_detect(rct_group, "chinese opv") ~ 6.0,
      study == "112679" & str_detect(rct_group, "poliorix") ~ 40,
      study == "444563-020" & str_detect(rct_group, "opv") ~ 6.0,
      study == "444563-020" & str_detect(rct_group, "ipv") ~ 40,
      study == "759348-003" & str_detect(rct_group, "s126zj") ~ 6.0,
      study == "759348-003" & str_detect(rct_group, "ipv") ~ 40,
      .default = vac_conc))
  }
  
  # Hib vaccine concentrations ---------------------------------------------------------------------
  if(pat == "hib") {
    dat <- dat %>%
    mutate(vac_conc = case_when(
      study %in% c("104977") & is.na(rct_group) ~ 0,
      study == "100478" & str_detect(rct_group, "mix men ac-hib") ~ 2.5,
      study == "100478" & str_detect(rct_group, "mix hib vaccine") ~ 10,
      study == "100480" & str_detect(rct_group, "mix men ac-hib") ~ 2.5,
      study == "100480" & str_detect(rct_group, "mix hib vaccine") ~ 10,
      study == "101222" & str_detect(rct_group, "hibkft") ~ 2.5,
      study == "101222" & str_detect(rct_group, "hiberix") ~ 10,
      study == "101223" & str_detect(rct_group, "hib -") ~ 2.5,
      study == "101223" & str_detect(rct_group, "hiberix") ~ 10,
      study == "101858" & str_detect(rct_group, "menomune_na") ~ 0,
      study == "101858" & str_detect(rct_group, "hib-mency") ~ 2.5,
      study == "101858" & str_detect(rct_group, "act-hib") ~ 10,
      study == "103974" & str_detect(rct_group, "hib-menc lot") ~ 5,
      study == "103974" & str_detect(rct_group, "pediacel") ~ 10,
      study == "104489" & str_detect(rct_group, "hbv/hib kft") ~ 2.5,
      study == "104489" & str_detect(rct_group, "hiberix") ~ 10,
      study == "104733" & str_detect(rct_group, "hib-menac") ~ 2.5,
      study == "104733" & str_detect(rct_group, "hiberix") ~ 10,
      study == "104977" & str_detect(rct_group, "hib 2.5|hib kft") ~ 2.5,
      study == "104977" & str_detect(rct_group, "hib 10") ~ 10,
      study == "105910" & str_detect(rct_group, "hib", negate = TRUE) ~ 0,
      study == "105910" & str_detect(rct_group, "hib") ~ 10,
      study == "107005" & str_detect(rct_group, "menc-tt|menc-crm") ~ 10,
      study == "107005" & str_detect(rct_group, "prevenar|hib-menc") ~ 5,
      study == "107058" & str_detect(rct_group, "7-11|12-23|24") ~ 0,
      study == "107058" & str_detect(rct_group, "< 6 months") ~ 10,
      study == "112157" & str_detect(rct_group, "hib ") ~ 10,
      study == "112157" & str_detect(rct_group, "hib-menc") ~ 5,
      study == "113615" & str_detect(rct_group, "hib ") ~ 10,
      study == "113615" & str_detect(rct_group, "hib-menc") ~ 5,
      study == "208108-091" & str_detect(rct_group, "hib 2.5") ~ 2.5,
      study == "208108-091" & str_detect(rct_group, "norm thio") ~ 10,
      study == "217744-097" & str_detect(rct_group, "sep hib") ~ 5,
      study == "217744-097" & str_detect(rct_group, "mix hib") ~ 10,
      study == "402764-004" & str_detect(rct_group, "men-ac-hib") ~ 5,
      study == "402764-004" & str_detect(rct_group, "hiberix") ~ 10,
      study == "711202-001" & str_detect(rct_group, "dch007a48") ~ 5,
      study == "711202-001" & str_detect(rct_group, "dch007a48", negate = T) ~ 10,
      study == "759346-001" & str_detect(rct_group, "2.5/5/5") ~ 2.5,
      study == "759346-001" & str_detect(rct_group, "5/5/5") ~ 5,
      study == "759346-001" & str_detect(rct_group, "hiberix") ~ 10,
      study == "759346-007" & str_detect(rct_group, "menac-hib") ~ 2.5,
      study == "759346-007" & str_detect(rct_group, "hiberix") ~ 10,
      study == "759348-003" & str_detect(rct_group, "dtpw-hbv") ~ 2.5,
      study == "759348-003" & str_detect(rct_group, "dtpa-hbv-ipv") ~ 10,
      study == "792014-001" & str_detect(rct_group, "10-10-5|10-10-10") ~ 5,
      study == "792014-001" & str_detect(rct_group, "5-5-2.5") ~ 2.5,
      study == "792014-001" & str_detect(rct_group, "act-hib") ~ 10,
      study == "792014-003" & str_detect(rct_group, " 5-5-5|10-10-5|menc-hib") ~ 5,
      study == "792014-003" & str_detect(rct_group, "5-5-2.5") ~ 2.5,
      study == "792014-003" & str_detect(rct_group, "sep hib") ~ 10,
      study == "811936-001" & str_detect(rct_group, "menc-hib") ~ 5,
      study == "811936-001" & str_detect(rct_group, "mix hib") ~ 10,
      .default = vac_conc)) 
  }
  
  # Diphteria vaccine concentrations ---------------------------------------------------------------
  if(pat == "dipht") {
    dat <- dat %>%
    mutate(vac_conc = case_when(
      #study %in% c("101858", "208136-013") ~ 25,
      study == "101858" & str_detect(rct_group, "menomune_na") ~ 0,
      study == "101858" & str_detect(rct_group, "dtpa") ~ 25,
      study == "103974" & str_detect(rct_group, "pediacel") ~15,
      study == "103974" & str_detect(rct_group, "infanrix") ~ 25,
      study == "104021" & str_detect(rct_group, "csl triple") ~ 30,
      study == "104021" & str_detect(rct_group, "tritanrix|dtpw hbv kft") ~ 7.5,
      study == "104489" & str_detect(rct_group, "triple antigen") ~ 30,
      study == "104489" & str_detect(rct_group, "dtpwgod|dtpwcsl") ~ 7.5,
      study == "107058" & str_detect(rct_group, "7-11|12-23|24") ~ 0,
      study == "107058" & str_detect(rct_group, "< 6 months") ~ 25,
      study == "114056" & str_detect(rct_group, "7-11|12-23") ~ 0,
      study == "114056" & str_detect(rct_group, "8-11 weeks") ~ 7.5,
      study == "213501-018" & str_detect(rct_group, "dtpw-hbv") ~ 7.5,
      study == "213501-018" & str_detect(rct_group, "local dtpw") ~ 0,
      study == "444563-005" & country == "United States" ~ 25,
      study == "444563-005" & country == "Canada" ~ 15,
      study == "444563-020" & str_detect(rct_group, "opv") ~ 7.5,
      study == "444563-020" & str_detect(rct_group, "ipv") ~ 25,
      study == "759348-003" & str_detect(rct_group, "dtpa") ~ 25,
      study == "759348-003" & str_detect(rct_group, "dtpw") ~ 7.5,
      .default = vac_conc)) 
  }
  
  # Tetanus vaccine concentrations -----------------------------------------------------------------
  if(pat == "tetan") {
    dat <- dat %>%
    mutate(vac_conc = case_when(
      study == "101858" & str_detect(rct_group, "menomune_na") ~ 0,
      study == "101858" & str_detect(rct_group, "dtpa") ~ 10,
      study == "103974" & str_detect(rct_group, "pediacel") ~ 5,
      study == "103974" & str_detect(rct_group, "infanrix") ~ 25,
      study == "104021" & str_detect(rct_group, "csl triple") ~ 36,
      study == "104021" & str_detect(rct_group, "tritanrix|dtpw hbv kft") ~ 3.25,
      study == "104489" & str_detect(rct_group, "triple antigen") ~ 36,
      study == "104489" & str_detect(rct_group, "dtpwgod|dtpwcsl") ~ 3.25,
      study == "107058" & str_detect(rct_group, "7-11|12-23|24") ~ 0,
      study == "107058" & str_detect(rct_group, "< 6 months") ~ 10,
      study == "114056" & str_detect(rct_group, "7-11|12-23") ~ 0,
      study == "114056" & str_detect(rct_group, "8-11 weeks") ~ 3.25,
      study == "213501-018" & str_detect(rct_group, "dtpw-hbv") ~ 3.25,
      study == "213501-018" & str_detect(rct_group, "local dtpw") ~ 0,
      study == "444563-005" & country == "United States" ~ 10,
      study == "444563-005" & country == "Canada" ~ 5,
      study == "444563-020" & str_detect(rct_group, "opv") ~ 3.25,
      study == "444563-020" & str_detect(rct_group, "ipv") ~ 10,
      study == "759348-003" & str_detect(rct_group, "dtpa") ~ 10,
      study == "759348-003" & str_detect(rct_group, "dtpw") ~ 3.25,
      .default = vac_conc)) 
  }
  
  # Pertussis vaccine concentrations ---------------------------------------------------------------
  if(pat == "pertu") {
    dat <- dat %>%
    mutate(vac_conc = case_when(
      study == "101858" & str_detect(rct_group, "menomune_na") ~ 0,
      study == "101858" & str_detect(rct_group, "dtpa") ~ 25,
      study == "101222" & str_detect(rct_group, "dtpw") ~ 20,
      study == "101222" & str_detect(rct_group, "tritanrix") ~ 15,
      study == "103974" & str_detect(rct_group, "pediacel") ~ 20,
      study == "103974" & str_detect(rct_group, "infanrix") ~ 25,
      study == "104021" & str_detect(rct_group, "csl triple|dtpw hbv kft") ~ 20,
      study == "104021" & str_detect(rct_group, "tritanrix") ~ 15,
      study == "107058" & str_detect(rct_group, "7-11|12-23|24") ~ 0,
      study == "107058" & str_detect(rct_group, "< 6 months") ~ 25,
      study == "114056" & str_detect(rct_group, "7-11|12-23") ~ 0,
      study == "114056" & str_detect(rct_group, "8-11 weeks") ~ 15,
      study == "213501-018" & str_detect(rct_group, "dtpw-hbv") ~ 15,
      study == "213501-018" & str_detect(rct_group, "local dtpw") ~ 0,
      study == "444563-005" & country == "United States" ~ 25,
      study == "444563-005" & country == "Canada" ~ 20,
      study == "444563-020" & str_detect(rct_group, "opv") ~ 15,
      study == "444563-020" & str_detect(rct_group, "ipv") ~ 25,
      study == "759348-003" & str_detect(rct_group, "dtpa") ~ 25,
      study == "759348-003" & str_detect(rct_group, "dtpw") ~ 15,
      .default = vac_conc))
  }
  
  # Hbv vaccine concentrations ---------------------------------------------------------------------
  if(pat == "hbv") {
    dat <- dat %>%
    mutate(vac_conc = case_when(
      study == "104489" & str_detect(rct_group, "triple antigen") ~ 0,
      study == "104489" & str_detect(rct_group, "dtpw") ~ 10,
      study == "105539" & country %in% c("Sweden", "Slovakia")  ~ 10,
      study == "105539" & country %in% c("Denmark", "Norway")  ~ 0,
      .default = vac_conc))
  }
  
  # Meningococcus vaccine concentrations -----------------------------------------------------------
  if(pat == "men") {
    dat <- dat %>%
    mutate(vac_conc = case_when(
      study == "100478" & str_detect(rct_group, "151005a2") ~ 0,
      study == "100478" & str_detect(rct_group, "men ac") ~ 2.5,
      study == "100480" & str_detect(rct_group, "hib1690a47\\) hib") ~ 0,
      study == "100480" & str_detect(rct_group, "men ac-hib") ~ 2.5,
      study == "100480" & str_detect(rct_group, "meningitec") ~ 10,
      study == "101858" & str_detect(rct_group, "mix diluent for act-hib") ~ 0,
      study == "101858" & str_detect(rct_group, "dcyh003a48") ~ 5,
      study == "101858" & str_detect(rct_group, "menomune_na") ~ 50,
      study == "103974" & str_detect(rct_group, "^hib-menc") ~ 5,
      study == "103974" & str_detect(rct_group, "^meningitec") ~ 10,
      study == "104733" & str_detect(rct_group, "hiberix") ~ 0,
      study == "104733" & str_detect(rct_group, "hib-menac") ~ 2.5,
      study == "106445" & str_detect(rct_group, "^hib-menc") ~ 5,
      study == "106445" & str_detect(rct_group, "^hiberix") ~ 10,
      study == "107005" & str_detect(rct_group, "prevenar|hib-menc") ~ 5,
      study == "107005" & str_detect(rct_group, "menc-crm|menc-tt") ~ 10,
      study == "107017" & rct_group %in% c("antipyretic", "non antipyretic") ~ 0,
      study == "107017" & str_detect(rct_group, "menacwy") ~ 5,
      study == "112157" & str_detect(rct_group, "menc-tt|menc-crm", negate = T) ~ 0,
      study == "112157" & str_detect(rct_group, "menc-tt") ~ 5,
      study == "112157" & str_detect(rct_group, "menc-crm") ~ 10,
      study == "113615" & str_detect(rct_group, "menc-tt") ~ 5,
      study == "113615" & str_detect(rct_group, "menjugate") ~ 10,
      study == "217744-097" & str_detect(rct_group, "hib-menc") ~ 5,
      study == "217744-097" & str_detect(rct_group, "neisvac|meningitec") ~ 10,
      study == "347414-017" & str_detect(rct_group, "hiberixtm") ~ 0,
      study == "347414-017" & str_detect(rct_group, "hib_menc") ~ 10,
      study == "402764-004" & str_detect(rct_group, "hiberix") ~ 0,
      study == "402764-004" & str_detect(rct_group, "men-ac-hib") ~ 5,
      study == "711202-001" & str_detect(rct_group, "dch007a48") ~ 5,
      study == "711202-001" & str_detect(rct_group, "dch007a48", negate = T) ~ 10,
      study == "759346-001" & str_detect(rct_group, "separate hiberix") ~ 0,
      study == "759346-001" & str_detect(rct_group, "2.5/5/5") ~ 2.5,
      study == "759346-001" & str_detect(rct_group, "5/5/5") ~ 5,
      study == "759346-001" & str_detect(rct_group, "meningitec") ~ 10,
      study == "759346-007" & str_detect(rct_group, "mix hib") ~ 0,
      study == "759346-007" & str_detect(rct_group, "mix menac-hib") ~ 2.5,
      study == "759348-001" & str_detect(rct_group, "mix menc-tt|meningitec", negate = T) ~ 0,
      study == "759348-001" & str_detect(rct_group, "mix menc-tt|meningitec") ~ 10,
      study == "792014-001" & str_detect(rct_group, "dcyh|menjugate", negate = T) ~ 0,
      study == "792014-001" & str_detect(rct_group, "dcyh001a48|dcyh003a48") ~ 5,
      study == "792014-001" & str_detect(rct_group, "dcyh002a48|menjugate") ~ 10,
      study == "792014-003" & str_detect(rct_group, "5-5|menc-hib") ~ 5,
      study == "792014-003" & str_detect(rct_group, "10-10|menjugate") ~ 10,
      study == "811936-001" & str_detect(rct_group, "menc-hib") ~ 5,
      study == "811936-001" & str_detect(rct_group, "meningitec") ~ 10,
      .default = vac_conc)) 
  }
  
  # Streptococcus vaccine concentrations -----------------------------------------------------------
  if(pat == "strep") {
  # Add vaccine data
    dat <- dat %>%
    mutate(vac_conc = case_when(
      study %in% c("110521", "115992") & is.na(rct_group) ~ 0,
      study == "101858" & rct_group == "menomune_na" ~ 0,
      study == "101858" & rct_group != "menomune_na" ~ 4,
      study == "103488" & rct_group == "11pn-pd" ~ 1,
      study == "103488" & str_detect(rct_group, " a| b| c| d|form f| g| i") ~ 1,
      study == "103488" & str_detect(rct_group, " e| h") ~ 3,
      study == "103488" & str_detect(rct_group, "prevnar") ~ 4,
      study == "105553" & str_detect(rct_group, "10pn-pd-dit") ~ 1,
      study == "105553" & str_detect(rct_group, "prevenar") ~ 4,
      study == "105554" & str_detect(rct_group, "10pn-pd-dit") ~ 1,
      study == "105554" & str_detect(rct_group, "prevenar") ~ 4,
      study == "106208" & str_detect(rct_group, "hav") ~ 0,
      study == "106208" & str_detect(rct_group, "10pn-pd-dit") ~ 1,
      study == "107005" & str_detect(rct_group, "10pn") ~ 1,
      study == "107005" & str_detect(rct_group, "prevenar") ~ 4,
      study == "107007" & str_detect(rct_group, "^10pn") ~ 1,
      study == "107007" & str_detect(rct_group, "^prevenar") ~ 4,
      study == "110521" & str_detect(rct_group, "zilbrix") ~ 0,
      study == "110521" & str_detect(rct_group, "10pn") ~ 1,
      study == "111188" & str_detect(rct_group, "^hib") ~ 0,
      study == "111188" & str_detect(rct_group, "^10pn") ~ 1,
      study == "112157" & str_detect(rct_group, "10pn", negate = T) ~ 0,
      study == "112157" & str_detect(rct_group, "10pn") ~ 1,
      study == "112640" & str_detect(rct_group, "10pn", negate = T) ~ 0,
      study == "112640" & str_detect(rct_group, "10pn") ~ 1,
      study == "113994" & str_detect(rct_group, "^10pn") ~ 1,
      study == "113994" & str_detect(rct_group, "prevenar") ~ 4.4,
      study == "115992" & str_detect(rct_group, "sss") ~ 1,
      study == "115992" & str_detect(rct_group, "pss|pps") ~ 4.4,
      study == "217744-078" & str_detect(rct_group, "prevnar", negate = T) ~ 0,
      study == "217744-078" & str_detect(rct_group, "prevnar") ~ 4,
      study == "347414-017" & str_detect(rct_group, "11pn", negate = T) ~ 0,
      study == "347414-017" & str_detect(rct_group, "11pn") ~ 1,
      study == "347414-020" & str_detect(rct_group, "undeca-pn-pd") ~ 1,
      study == "347414-020" & str_detect(rct_group, "prevnar") ~ 4,
      study == "347414-023" & str_detect(rct_group, "^hiberix") ~ 0,
      study == "347414-023" & str_detect(rct_group, "sp10|sp015") ~ 1,
      study == "347414-023" & str_detect(rct_group, "sp016") ~ 3,
      study == "759348-001" & str_detect(rct_group, "^meningitec") ~ 0,
      study == "759348-001" & str_detect(rct_group, "sp1008a2") ~ 1,
      study == "759348-001" & str_detect(rct_group, "^prevnar") ~ 4,
      study == "759348-001" & str_detect(rct_group, "dsp018|dsp020") ~ 5,
      study == "759348-001" & str_detect(rct_group, "dsp017|dsp019") ~ 10,
      study == "759348-003" & str_detect(rct_group, "^prevnar") ~ 4,
      study == "759348-003" & str_detect(rct_group, "dsp018|dsp020") ~ 5,
      study == "759348-003" & str_detect(rct_group, "dsp017|dsp019") ~ 10,
      study == "759348-007" & str_detect(rct_group, "sp100|dsp1019|dsp1018") ~ 1,
      study == "759348-007" & str_detect(rct_group, "dsp1016|dsp1017") ~ 3,
      study == "759348-007" & str_detect(rct_group, "^prevnar") ~ 4,
      study == "792014-001" & str_detect(rct_group, "menjugate") ~ 0,
      study == "792014-001" & str_detect(rct_group, "sep prevnar") ~ 4,
      .default = vac_conc)) 
  }
  
  # Return -----------------------------------------------------------------------------------------
  return(dat)
}

####################################################################################################
# End 
####################################################################################################