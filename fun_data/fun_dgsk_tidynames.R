####################################################################################################
# Function to tidy up country, vaccine, and serology names
####################################################################################################

fun_dgsk_tidynames <- function(x, study = "105555", tidynames = "country") {
  # Arguments:
  # - x: (data frame) A data frame containing distinct names (one of: country names, vaccine names, 
  # or serology names) used in the study.
  # - study_id: (string) The unique identifier for the study being processed. 
  # - tidynames: (string) Specifies which type of names to tidy, must be one of: "country", 
  # "vaccine", or "seroname".
  # Output:
  # - x: (data frame) A dictionary of original and tidied-up names corresponding to the selected 
  # category (country, vaccine, or seroname).
  
  # Tidy Country Names =============================================================================
  if(tidynames == "country") {
  
    x %>%
      mutate(study = study) %>%
      # Standardize country names to title case
      mutate(country_tidy = str_to_title(cty_nam)) %>%
      # Map abbreviations and alternate names to standardized country names
      mutate(country_tidy = case_when(
        country_tidy %in% c("Cri") ~ "Costa Rica",
        country_tidy %in% c("Cze", "Czechia") ~ "Czech Republic",
        country_tidy %in% c("Est") ~ "Estonia",
        country_tidy %in% c("Fin") ~ "Finland",
        country_tidy %in% c("Deu") ~ "Germany",
        country_tidy %in% c("Jpn") ~ "Japan",
        country_tidy %in% c("Mys") ~ "Malaysia",
        country_tidy %in% c("Mex") ~ "Mexico",
        country_tidy %in% c("Pri") ~ "Puerto Rico",
        country_tidy %in% c("Russian Federation") ~ "Russia",
        country_tidy %in% c("Korea", "Korea Republic Of", "Kor", "Korea  Republic of") ~ "South Korea",
        country_tidy %in% c("Esp") ~ "Spain",
        country_tidy %in% c("Twn") ~ "Taiwan",
        country_tidy %in% c("Tha") ~ "Thailand",
        country_tidy %in% c("Usa") ~ "United States",
        country_tidy %in% c("Uk - Cmd") ~ "United Kingdom",
        country_tidy %in% c("Viet Nam") ~ "Vietnam",
        country_tidy %in% c("") ~ NA,
        .default = country_tidy)) %>%
      select(study, cty_nam, country_tidy) 
  
  # Tidy Vaccine Names =============================================================================
  } else if (tidynames == "vaccine") {
  
  x %>%
    mutate(study = study,
           vaccine_o = vaccine) %>%
    # Convert vaccine names to lowercase for standardization
    mutate(vaccine = tolower(vaccine)) %>%
    # Map original vaccine names to standardized names
    mutate(vaccine_tidy = case_when(
      # dtpa combinations
      vaccine %in% c("dtpa vaccine", "dtp", "infanrix", "dpta") ~ "dtpa",
      vaccine %in% c("dtpa-hib", "infanrix hib", "infanrix/hib vaccine") ~ "dtpa_hib",
      vaccine %in% c("kinrix", "infanrix - ipv", "dtpa-ipv") ~ "dtpa_ipv",
      vaccine %in% c("dtpa_ipv + hib vaccine","dtpa-ipv-hib", "pediacel", 
                     "dtpa-ipv/hib", "pentacel", "dtpa_ipv_hib") ~ "dtpa_hib_ipv",
      vaccine %in% c("dtpa-hbv-ipv", "infanrix penta", "pediarix", "dtpa-hbv-ipv licensed",
                     "dtpa-hbv-ipv form a", "dtpa-hbv-ipv form b") ~ "dtpa_hbv_ipv",
      vaccine %in% c("dtpa_hbv_ipv_hib", "dtpa_hbv_ipv+hib", "infanrix hexa",
                     "dtpa-hbv-ipb/hib", "dtpa-hbv-ipv/hib",
                     "dtpa-ipv-hib+hep", "dtpa+hbv+hib+ipv") ~ "dtpa_hbv_hib_ipv",
      vaccine %in% c("dtpa-hbv-ipv/hib-menc-tt") ~ "dtpa_hbv_hib_ipv_men",
      # dtpw combinations
      vaccine %in% c("dtpw csl", "triple antigen") ~ "dtpw",
      vaccine %in% c("tritanrix-hepb", "dtpw-hbv kft", "dtpw-hbv") ~ "dtpw_hbv",
      vaccine %in% c("dtpw-hbv/hib", "dtpw-hbv-hib", "tritanrix-hepb mixed with hiberix", 
                     "dtpw-hbv mixed with hib conjuguate", "dtpw_hbv+hib", "dtpw-hbv/hib kft",
                     "tritanrix-hepb mixed with hib_ads_th 2.5", "dtpw god - hbv/hib kft",
                     "dtpw csl - hbv/hib kft", "dtpw-hbv/hib 2.5", "dtpw-hepb/hib 10",
                     "dtpw_hbv_hib") ~ "dtpw_hbv_hib",
      vaccine %in% c("dtpw_hbv/menac_hib vaccine", 
                     "tritanrix-hepb/hib-menac vaccine") ~ "dtpw_hbv_hib_men",
      # hav
      vaccine %in% c("havrix", "havrix vaccine") ~ "hav",
      # hbv
      vaccine %in% c("engerix", "hbv", "engerix-b thiomersal-free(dens001c)", "hep.b",
                     "hepb", "hbv vaccination") ~ "hbv",
      # hib 
      vaccine %in% c("hiberix", "hib", "prp vaccine", "acthib", "hib titer", 
                     "unconjugated hib vaccine", "pedvaxhib") ~ "hib",
      # hib_men
      vaccine %in% c("hib-menc", "men c-hib", "menac-hib", "menac-hib ads", 
                     "hib-mency", "hib-mency-tt") ~ "hib_men",
      # hib_strep
      vaccine %in% c("11pn-pd/hib", "undeca_pn_pd/hiberix") ~ "hib_strep11",
      # hib_men_strep
      vaccine %in% c("undeca_pn_pd/hib-menc") ~ "hib_men_strep11",
      # men 
      vaccine %in% c("meningitec vaccine","menacwy-tt", "menjugate", "neisvac-c vaccine",
                     "meningc", "meningb", "1/5 mencevax ac", "1/5 mencevax acwy", 
                     "menc-tt vaccine", "mencevax acw vaccine", "mencevax acwy vaccine",
                     "menomune", "mency_hib", "menc crm", "neis vac-c vaccine") ~ "men",
      # men_strep
      vaccine %in% c("11pn pd di / menc vaccine") ~ "men_strep11",
      # mmr 
      vaccine %in% c("memuru", "mmr ii vaccine", "mmr vaccine", "priorix", "mmr_merck") ~ "mmr",
      # mmr_vzv
      vaccine %in% c("memuru-oka", "mmrv", "mmrv f", "mmrv new ws", "priorix tetra", 
                     "proquad") ~ "mmr_vzv",
      # polio ipv
      vaccine %in% c("ipv", "ipol", "imovax polio vaccine", "poliorix") ~ "ipv",
      # polio opv
      vaccine %in% c("opv", "oral polio vaccine", "oral polio vaccune", "oral polio vaqccine",
                     "oral p[olio vaccine", "oral polio  vaccine", "oral poliovaccine",
                     "oral poliuo vaccine", "oral poli vaccine", "pral polio vaccine") ~ "opv",
      # rota 
      vaccine %in% c("rotavirus", "hrv", "rotarix", "rota",
                     "(1064_1066) rotavirus vaccine lyophilized formulation",
                     "(1065) rotavirus vaccine liquid formulation # lot a",
                     "(1065) rotavirus vaccine liquid formulation # lot b",
                     "(1065) rotavirus vaccine liquid formulation # lot c") ~ "rota",
      # strep 
      vaccine %in% c("prevnar", "strep") ~ "strep7",
      vaccine %in% c("10pn-pd-dit vaccine", "10pn-pd-dit", "synflorix",
                     "10pn-pd-dit-dply-phtd 10", "10pn-pd-dit-dply-phtd 30") ~ "strep10",
      vaccine %in% c("11pn pd di vaccine", "undeca-pn-pd", "11 pn pd dit vaccine",
                     "11pn pd vaccine", "undeca_pn", "11pn-pd") ~ "strep11",
      vaccine %in% c("prevenar 13", "prevnar 13 vaccine", "13pn") ~ "strep13",
      vaccine %in% c("pneumovax", "23 valent ps vaccine") ~ "strep23",
      # vzv 
      vaccine %in% c("varilrix", "varilrix hsa free", "varivax vaccine", "varicella", 
                     "varivax") ~ "vzv",
      # Placebo and no vaccine 
      vaccine %in% c("placebo", "hrv placebo", "opv placebo") ~ "placebo",  
      vaccine %in% c("study vaccine", "", "any co-administered vaccine") ~ NA,
      .default = vaccine)) %>%
    mutate(vaccine = vaccine_o) %>%
    select(study, vaccine, vaccine_tidy) 
  
  # Tidy Serology Names ============================================================================
  } else if (tidynames == "seroname") {
  
  x %>%
    mutate(study = study,
           seroname_o = seroname,
           method_o = method) %>%
    # Convert serology names and methods to lowercase and remove leading/trailing whitespace
    mutate(across(all_of(c("seroname", "method")), tolower)) %>%
    mutate(across(all_of(c("seroname", "method")), trimws)) %>%
    # Map original method names to standardized names
    mutate(method_tidy = case_when(
      method %in% c("bac", "sba", "bact.assay", "bact. assay", "bact") ~ "bac",
      method %in% c("clia") ~ "clia",
      method %in% c("ecl") ~ "ecl",
      method %in% c("elisa", "el", "eli", "elisa_1_dil", "whole-cell elisa", "elisa(mep)", 
                    "elisa frash", "elisa 22f pva", "elisa 22f", "elisa_22f_pva", "elisa (ktl)", 
                    "elisa_1dil","elisa22fpva") ~ "elisa",
      method %in% c("elisa_ch") ~ "elisa_ch",
      method %in% c("elisa_ppd") ~ "elisa_ppd",
      method %in% c("elispot") ~ "elispot",
      method %in% c("fama") ~ "fama", 
      method %in% c("inhem") ~ "hemolysis",
      method %in% c("ifa", "ifi", "if") ~ "ifa",
      method %in% c("opa", "opsono", "ops", "opsono h60(3)", "opsono h60(2)", "opsono(new)",
                    "opaplex", "opsono(cdc)", "opsono(gsk)", "opsono(ktl)") ~ "opa",
      method %in% c("mopa") ~ "mopa", 
      method %in% c("pcr") ~ "pcr",
      method %in% c("prnt", "neutra", "prn", "neu", "neutra(mep)", "neutralisation",
                    "neutralization") ~ "prnt",
      method %in% c("prnt_unenhanced") ~ "prnt_unenhanced",
      method %in% c("quant") ~ "qpcr",
      method %in% c("", "--------") ~ NA,
      .default = method)) %>%  
    # Map original serology names to standardized names
    mutate(seroname_tidy = case_when(
      # dtp
      # Diphtheria 
      seroname %in% c("anti-dipht", "anti-d antibody (iu/ml)", "anti-diphtheria", "anti-d (elisa)",
                      "anti-dipht (mep)", "anti-dipht(mep)", "c dipht.tox ab.igg", "c dipht.tox ab",
                      "anti-d toxin neutra", "anti-d antibody", "anti-d", "igg diphteria", 
                      "anti-d toxin", "anti-diphteria", "anti- diphtheria", "c dipht. tox igg",
                      "c dipht.tox igg", "anti-diphteriaigg", "anti-dipht igg", "anti-dipth",
                      "anti-=diphtheria", "anti-diptheria") ~ "anti_dipht",
      # Tetanus
      seroname %in% c("anti-tet", "anti-t antibody (iu/ml)", "anti-tetanus", "anti-tet (mep)", 
                      "anti-tet(mep)", "c tetani.tox ab.igg", "anti-t antibody", "anti-tetanusigg", 
                      "c tetani.tox igg", "anti-t", "anti_tet", "c tetani. tox igg", 
                      "anti-tet igg") ~ "anti_tetan",
      # Pertussis 
      seroname %in% c("anti-pt", "anti-pt antibody (iu/ml)", "anti-pt (mep)", "anti-bp antibody",
                      "anti-pt(mep)", "b pert.pt ab.igg", "	anti-bpt (mep)", "anti-ptigg", 
                      "anti-pt antibody", "anti-bpt (mep)", "b pert ab.igg", "b pert igg", 
                      "anti-bpt", "anti_pt", "anti-pt igg", "b pert.pt igg", 
                      "anti_bpt", "anti_b.pertussis") ~ "anti_pertu_pt",
      seroname %in% c("anti-fha", "anti-fha antibody (iu/ml)", "anti-fha (mep)", "anti-fhaigg",
                      "anti-fha(mep)", "b pert.fha ab.igg", "anti-fha antibody", "b pert.fha igg",
                      "anti_fha") ~ "anti_pertu_fha",
      seroname %in% c("anti-prn", "anti-prn antibody (iu/ml)", "anti-prn(mep)", "prnigg",
                      "b pert.prn ab.igg", "anti-prn antibody", "anti_prn", "anti-prn igg",
                      "anti-prnigg", "b pert.prn igg") ~ "anti_pertu_prn",
      # hav 
      seroname %in% c("anti-hav", "hav ab", "anti-hav antibody (miu/ml)") ~ "anti_hav",
      # hbv 
      seroname %in% c("anti-hbs", "hbv.s ab", "anti-hbs (mep)", "anti-hbs(mep)", "igtot-anti-hbs", 
                      "anti-hbs (in-house) antibody", "anti-hbs (clia) antibody", 
                      "anti-hbs (in-house)", "anti_hbs", "anti-hbs (eia) ped", "anti-hbs (elisa)",
                      "anti-hbs (clia)", "hbs ag", "igtotanti-hbs", "anti-hbs*eia",
                      "anti-hbs antibody") ~ "anti_hbv",
      seroname %in% c("anti-hbc", "hbv.c ab") ~ "anti_hbv_core",
      # hib 
      seroname %in% c("anti-prp", "anti-prp (mep)", "anti-prp(mep)", "hib.prp ab", "hib prp ab",
                      "anti-prp antibody", "anti-prp (elisa)", "anti-prpiggtot",
                      "anti_prp") ~ "anti_hib",
      # mmr 
      # Measles
      seroname %in% c("anti-measles", "measles ab.igg",
                      "anti-measles elisa", "measles", "me igg", "anti-measles antibody",
                      "anti-measles antibody (miu/l)", 
                      "anti-measles antibody (miu/ml)") ~ "anti_measl",
      # Mumps
      seroname %in% c("anti-mumps", "mumps ab.igg", "mu igg", "anti-mumps prn", "anti-mumps elisa",
                      "anti-mumps(prnt)", "anti-mumps (prnt)", "mu mu90 ab", "anti-mumps antibody",
                      "mumps (old)", "mumps (ppd)", "mumps (new)", 
                      "anti-mumps (ppd) antibody", "anti-mumps prn antibody", 
                      "anti-mumps (ppd) antibody (eu/ml)", 
                      "anti-mumps (prnt) antibody (ed50)") ~ "anti_mumps",
      # Rubella
      seroname %in% c("anti-rubella", "rub igg", "rubella ab.igg", "anti-rub", 
                      "anti-rubella antibody", "anti-rubella elisa", "rubella", 
                      "anti-rubella antibody (iu/ml)") ~ "anti_rubel",
      # men 
      seroname %in% c("anti-psa", "n men a l10 3125 ab", "rsba-mena", "n men.psa igg", 
                      "n men a ab", "sba-mena", "n men a ab", "sba-men a", "sba-mena l10") ~ "anti_men_a",
      seroname %in% c("rsba-menc (hpa)", "anti-psc", "anti-menc ps elisa", "anti_psc",
                      "n men.psc ab.igg", "hsba-menc", "n men c l3v c11 ab",
                      "n men c l3v c11 ab", "rsba-menc", "iggsba-menc",
                      "sba-menc", "n men.psc igg", "anti - psc", "rsba-menc2", "hsba-menc (updated)",
                      "hsba-menc (original)", "n men c ab", "sba-menc (old)", "igsba-menc", 
                      "igsba-menc19", "sba-menc 11", "sba-menc 4para", 
                      "sba-menc(re-test)", "sba_menc", "n men c ab", "sba-men c", 
                      "sba-menc11", "nmenc ab av") ~ "anti_men_c",
      seroname %in% c("anti-psy", "n men y l3v s1975 ab", "rsba-meny", "hsba-meny", 
                      "n men y ab", "sba-meny") ~ "anti_men_y",
      seroname %in% c("anti-psw-135", "rsba-menw-135", "n men w l3v mp012400", 
                      "n men.psw igg", "n men w135 ab") ~ "anti_men_w135",
      # polio 
      seroname %in% c("anti_polio 1", "anti-polio 1", "polio 1 (ed50)", "anti-polio1(mep)", 
                      "anti-polio 1 (mep)", "anti-polio1", "polio 1 ab", "anti-poliovirus type 1", 
                      "polio 1", "anti_polio1", "anti-polio 1 antibody", 
                      "anti-poliovirus 1") ~ "anti_polio_1",
      seroname %in% c("anti_polio 2", "anti-polio 2", "polio 2 (ed50)", "anti-polio2(mep)", 
                      "anti-polio 2 (mep)", "anti-polio2", "polio 2 ab", "anti-poliovirus type 2",
                      "polio 2", "anti_polio2", "anti-polio 2 antibody",
                      "anti-poliovirus 2") ~ "anti_polio_2",
      seroname %in% c("anti_polio 3", "anti-polio 3", "polio 3 (ed50)", "anti-polio3(mep)", 
                      "anti-polio 3 (mep)", "anti-polio3", "polio 3 ab", "anti-poliovirus type 3",
                      "polio 3", "anti_polio3", "anti-polio 3 antibody", 
                      "anti-poliovirus 3") ~ "anti_polio_3",
      # rota 
      seroname %in% c("rotavirus iga ab (u/ml)", "rota iga", "rota ab.iga", "anti-rota iga", 
                      "iga-rota", "anti-rotavirus iga antibody", "iga-rota(ser,eli)", 
                      "rota iga(ward)", "rota iga (ward)", "rota iga neutra ward",
                      "rota iga neutra", "rota ag (ward)", "rota ag") ~ "anti_rota_iga",
      seroname %in% c("rota iga(rix)", "rota iga (rix)") ~ "anti_rota_iga_rix",
      seroname %in% c("rota - igg(ward)", "rota - igg") ~ "anti_rota_igg",
      seroname %in% c("igtot wa(g1) rota", "igtot ds1(g2) rota", "igtot p(g3) rota", 
                      "igtot va70(g4) rota", "igtot wi61(g9) rota", 
                      "igtot rix4414(g1) ro") ~ "anti_rota_igtot",
      # strep 
      seroname %in% c("opsono-1", "anti-1", "s pneu 01/37 bh ab", "s pneu.ps01 igg", "s pneu 01 ab",
                      "s pneu.ps01 ab.igg", "anti-pnps 1 antibody (µg/ml)", "anti-pneumo 1",
                      "anti-1 antibody", "anti_1", "opsono_1", "s pneu.ps01 ab. igg", "mopa-1",
                      "anti-pnps 1 antibody (ecl)", "opa-1", "anti - 1") ~ "anti_strep_01",
      seroname %in% c("anti-pnps 3 antibody (µg/ml)", "anti-3 antibody", "anti_3", "anti-3", 
                      "opsono_3", "opsono-3", "anti-pnps 3 antibody (ecl)", "mopa-3", "opa-3",
                      "anti-pneumo 3", "anti - 3") ~ "anti_strep_03",
      seroname %in% c("anti-s.pneu-4", "opsono-4", "anti-4", "s pneu 04/2656 bh ab", "mopa-4",
                      "s pneu.ps04 igg", "s pneu 04 ab", "s pneu.ps04 ab.igg", "opa-4", "anti - 4",
                      "anti-pnps 4 antibody (µg/ml)", "igs/s.pneumonia-4", "anti-pneumo 4",
                      "igg/s.pneumo-4", "anti-4 antibody", "igg/s.pneumonia-4", "igg/s pneumonia-4",
                      "igg/s. pneumonia-4", "anti_4", "opsono_4", "anti-serotype 4", "s penu 04 ab",
                      "s pneu.ps04 ab. igg", "anti-pnps 4 antibody (ecl)") ~ "anti_strep_04",
      seroname %in% c("opsono-5", "anti-5", "s pneu 05 ambrose ss", "s pneu.ps05 igg", "mopa-5",
                      "s pneu 05 ab", "s pneu.ps05 ab.igg", "anti-pnps 5 antibody (µg/ml)", "opa-5",
                      "anti-5 antibody", "anti_5", "opsono_5", "s pneu.ps05 ab. igg", "anti - 5",
                      "anti-pneumo 5", "anti-pnps 5 antibody (ecl)", 
                      "s pneu 05 ambrose ssi ab") ~ "anti_strep_05",
      seroname %in% c("opsono-6a", "anti-6a", "s pneu.ps06a ab.igg", "s pneu 06a cdc ab", "opa-6a",
                      "s pneu.ps06a igg", "s pneu 06a ab", "anti-pnps 6a antibody (µg/ml)", 
                      "anti-6a antibody", "anti_6a", "opsono_6a", "anti-pneumo 6a", "mopa-6a",
                      "s pneu.ps06a ab. igg", "anti-pnps 6a antibody (ecl)") ~ "anti_strep_06a",
      seroname %in% c("anti-s.pneu-6b", "s pneu.ps06b ab.igg", "opsono-6b", "anti-6b", "mopa-6b",
                      "s pneu 06b ab", "s pneu.ps6b ab.igg", "s pneu.ps06b igg", "opa-6b",
                      "s pneu 06b/ds2212/94", "anti-pnps 6b antibody (µg/ml)", "anti-pneumo 6b",
                      "igs/s.pneumonia-6b", "igg/s.pneumo-6b", "s pneu 6b ab", "s pneu.06b ab",
                      "anti-6b antibody", "igg/s.pneumonia-6b", "igg/s pneumonia-6b", "anti - 6b",
                      "igg/s. pneumonia-6b", "anti_6b", "opsono_6b cdc", "s pneu 06b/ds2212/94 cdc",
                      "opsono_6b", "s pneu.ps06b ab. igg", "anti-serotype 6b", "anti-pn6b igg",
                      "anti-pnps 6b antibody (ecl)", "anti-pn6b igg2", 
                      "anti-pn6b igg1") ~ "anti_strep_06b",
      seroname %in% c("opsono-7f", "anti-7f", "s pneu.ps7f ab.igg", "s pneu 07f/46 bh ab", "opa-7f",
                      "s pneu.ps07f igg", "s pneu 07f ab", "anti-pnps 7f antibody (µg/ml)", 
                      "s pneu.ps07f ab.igg", "anti-7f antibody", "anti_7f", "anti-pneumo 7f",
                      "opsono_7f", "s pneu.ps07f ab. igg", "mopa-7f", "anti - 7f",
                      "anti-pnps 7f antibody (ecl)") ~ "anti_strep_07f",
      seroname %in% c("anti-s.pneu-9v", "s pneu .ps09v ab.igg", "opsono-9v", "anti-9v", "mopa-9v",
                      "s pneu.ps9v ab.igg", "s pneu 09v/112 161/9", "s pneu.ps09v igg", "opa-9v",
                      "s pneu 09v ab", "anti-pnps 9v antibody (µg/ml)", "anti-pneumo 9v",
                      "igs/s.pneumonia-9v","igg/s.pneumo-9v", "s pneu.ps09v ab.igg", "anti - 9v",
                      "s pneu.ps09v ab.igg", "anti-9v antibody", "igg/s.pneumonia-9v", 
                      "igg/s pneumonia-9v", "igg/s. pneumonia-9v", "anti_9v", "anti-serotype 9v",
                      "opsono_9v", "s pneu.ps09v ab. igg", "s pneu 09v/112 161/95 ssi",
                      "anti-pnps 9v antibody (ecl)") ~ "anti_strep_09v",
      seroname %in% c("anti-s.pneu-14", "opsono-14", "anti-14", "s pneu.ps14 ab.igg", "mopa-14",
                      "s pneu 14/58 bh ab", "s pneu.ps14 igg", "s pneu 14 ab", "opa-14",
                      "anti-pnps 14 antibody (µg/ml)", "igs/s.pneumonia-14", "anti-pneumo 14",
                      "igg/s.pneumo-14", "anti-14 antibody", "igg/s.pneumonia-14", "anti - 14",
                      "igg/s pneumonia-14", "igg/s. pneumonia-14", "anti_14", "anti-serotype 14",
                      "opsono_14", "s pneu.ps14 ab. igg", 
                      "anti-pnps 14 antibody (ecl)") ~ "anti_strep_14",
      seroname %in% c("anti-s.pneu-18c", "opsono-18c", "anti-18c", "s pneu.ps18c ab.igg", "opa-18c",
                      "s pneu 18c/4593/40 s", "s pneu.ps18c igg", "s pneu 18c ab", "mopa-18c", 
                      "anti-pnps 18c antibody (µg/ml)","igs/s.pneumonia-18c", "anti-serotype 18c",
                      "igg/s.pneumo-18c", "anti-18c antibody", "igg/s.pneumonia-18c", "opa-18c",
                      "igg/s pneumonia-18c", "igg/s. pneumonia-18c", "anti_18c", "opsono-18c cdc",
                      "opsono_18c", "opsono_18c cdc", "s pneu.ps18c ab. igg", "anti-pneumo 18c",
                      "anti-pnps 18c antibody (ecl)", "s pneu 18c/4593/40 ssi ab", 
                      "anti - 18c") ~ "anti_strep_18c",
      seroname %in% c("opsono-19a", "anti-19a", "s pneu.ps19a ab.igg", "s pneu 19a/db18 ktl",
                      "s pneu.ps19a igg", "s pneu 19a ab", "anti-pnps 19a antibody (µg/ml)", 
                      "anti-19a antibody", "s pneu 19a  ab", "anti_19a", "mopa-19a", "opa-19a",
                      "opsono_19a", "s pneu.ps19a ab. igg", "anti-pneumo 19a",
                      "anti-pnps 19a antibody (ecl)", "s pneu 19a/db18 ktl ab",
                      "s pneu 19a/db18 ktl  ab") ~ "anti_strep_19a",
      seroname %in% c("anti-s.pneu-19f", "s pneu.ps19f ab.igg", "opsono-19f", "anti-19f", "opa-19f",
                      "s pneu 19f/2737 bh a", "s pneu.ps19f igg", "s pneu 19f ab", "mopa-19f",
                      "anti-pnps 19f antibody (µg/ml)", "igs/s.pneumonia-19f", "anti-serotype 19f",
                      "igg/s.pneumo-19f", "anti-19f antibody", "igg/s.pneumonia-19f", "anti - 19f",
                      "igg/s pneumonia-19f", "igg/s. pneumonia-19f", "anti_19f", "anti-pneumo 19f",
                      "opsono_19f", "opsono_19f cdc", "s pneu.ps19f ab. igg",
                      "anti-pnps 19f antibody (ecl)", "s pneu 19f/2737 bh ab") ~ "anti_strep_19f",
      seroname %in% c("anti-s.pneu-23f", "opsono-23f", "anti-23f", "s pneu.ps23f ab.igg", "opa-23f",
                      "s pneu 23f mac-ssi a", "s pneu.ps23f igg", "s pneu 23f ab", "mopa-23f",
                      "anti-pnps 23f antibody (µg/ml)", "igs/s.pneumonia-23f", "anti-serotype 23f",
                      "igg/s.pneumo-23f", "anti-23f antibody", "igg/s.pneumonia-23f", "anti - 23f",
                      "igg/s pneumonia-23f", "igg/s. pneumonia-23f", "anti_23f", "anti-pneumo 23f",
                      "opsono_23f", "s pneu.ps23f ab. igg", "s pneu 23f mac-ssi ab",
                      "anti-pnps 23f antibody (ecl)") ~ "anti_strep_23f",
      # vzv 
      seroname %in% c("anti-var gp", "anti-varicella", "anti-var", "igg-varicella",
                      "vzv igg", "vzv", "vzv ab. igg or tgg", "vzv-igg", "vzv ab.igg", 
                      "anti-vzv", "anti-vzv ifa", "anti-vzv antibody", 
                      "vzv ab. igg or tgg (miu/ml)", "anti-vzv antibody (miu/ml)") ~ "anti_vzv",
      # others
      seroname %in% c("anti-pd", "hi.pd ab.igg", "hi nthi.pd ab", "anti-proteine d", "anti-phtd",
                      "anti-pd antibody", "hi .pd ab. igg", "hi nthi pd ab", "anti_pd",
                      "pd enzymatic inhibition") ~ "anti_protd",
      seroname %in% c("ply hemolysis inhibition", "anti-ply") ~ "anti_protply",
      seroname %in% c("hiv pcr testing", "hiv elisa testing", "hiv dna") ~ "scr_hiv",
      seroname %in% c("hiv antibodies") ~ "anti_hiv",
      seroname %in% c("s aureus dna") ~ "scr_saureus",
      seroname %in% c("s pyogenes dna") ~ "scr_spyogenes",
      seroname %in% c("p aeruginosa dna") ~ "scr_paeruginosa",
      seroname %in% c("m catarrhalis dna") ~ "scr_mcatarrhalis",
      seroname %in% c("s pneu dna") ~ "scr_spneumoniae",
      seroname %in% c("cd4 (cells/mm3)") ~ "scr_cd4cellsconc",
      seroname %in% c("cd4 (%)") ~ "scr_cd4cellsperc",
      seroname %in% c("( )", "  ( )", "", "code:", "viral load", "hi dna", "hi nthi ag", "nthi ag",
                      "elispot b", "vl quant") ~ NA,
      .default = seroname)) %>%
    mutate(seroname = seroname_o,
           method = method_o) %>%
    select(study, method, method_tidy, seroname, seroname_tidy, unit, cut_off)
  } else {
    print("Invalid argument for tidynames.")
  }
}

####################################################################################################
# END
####################################################################################################

