
fun_datagsk_tidy_pid <- function(x, study_id = "study") {
  # demo df 
  df_demo <- x$demo %>%
    mutate(study = study_id) %>%
    select(study, pid, age_unit, age, date_ref, sex) %>%
    # Tidy NAs
    mutate(across(everything(), ~ na_if(.x, "")))  %>%
    # Estimate date of birth from reference date and age             
    mutate(date_age = fun_gskdate_to_rdate(date_ref),
           date_birth = case_when(
             age_unit == "M" ~ date_age - (as.numeric(age)*30),
             age_unit == "W" ~ date_age - (as.numeric(age)*7),
             .default = NA)) %>%
    # Select variables 
    select(study, pid, age, age_unit, sex, date_birth, date_age) %>%
    # Arrange by study and PID
    arrange(study, as.numeric(pid)) 
  
  # pid df
  df_pid <- x$pid %>%
    mutate(study = study_id) %>%
    select(study, pid, center, cty_nam, treatmnt, elig_ma) %>%
    # Join with demography data 
    left_join(df_demo) %>%
    # Tidy NAs
    mutate(across(all_of(c("center", "cty_nam", "treatmnt")), ~ na_if(.x, "")))  %>%
    # Tidy center: Eliminate leading white spaces
    mutate(center = gsub("^\\s+", "", center)) %>%
    # Study 209762-136 did not followed up/enrolled 610 individuals
    filter(!(is.na(center))) %>%
    select(study, pid, center, cty_nam, treatmnt, elig_ma, age, age_unit, sex, date_birth, date_age)
  
  return(df_pid)
}

fun_datagsk_tidy_expo_all <- function(x, study_id = "study") {
  
  # expo_all df
  df_expo_all <- x$expo %>%
    mutate(study = study_id) %>%
    select(study, pid, vaccine, p_rdat, group_nb, v_adm) %>%
    # Tidy vaccination dates: reformat 
    mutate(date_expo = fun_gskdate_to_rdate(p_rdat)) %>%
    # Tidy vaccine names 
    left_join(vac_df) %>%
    mutate(vaccine = vaccine_tidy) %>%
    # Select variables
    select(study, pid, vaccine, group_nb, v_adm, date_expo)
  
  return(df_expo_all)
}

fun_datagsk_tidy_sero_all <- function(x, study_id = "study") {
   
  # date sero df 
  df_dat_sero <- x$dates %>%
    mutate(study = study_id) %>%
    select(study, pid, raw_date, activity, type_lst) %>%
    # Select only dates for blood and serology
    filter(type_lst %in% c("BL", "SER")) %>%
    # Tidy blood sampling date: reformat 
    mutate(raw_date = na_if(raw_date, ""))  %>%
    mutate(date_sero = fun_gskdate_to_rdate(raw_date)) %>%
    # select variables
    select(study, pid, activity, date_sero)

  # sero_all df 
  df_sero_all <- x$sero %>%
    mutate(study = study_id) %>%
    select(study, pid, activity, num_res, seroname, method) %>%
    # Rename methods and antibody measurement 
    left_join(imm_df) %>%
    mutate(seroname = seroname_tidy,
           method = method_tidy) %>%
    # Join with date and week
    full_join(df_dat_sero) %>% 
    filter(!is.na(seroname)) %>%
    select(study, pid, method, seroname, num_res, date_sero, activity)
  
  return(df_sero_all)
}

# Exceptions =======================================================================================

fun_datagsk_tidy_exceptions <- function(files_pat, study_id = "study") {
  
  # Many datasets have ELIG but no ELIG_MA:
  if(all((names(files_pat$pid) == "elig_ma") == FALSE) == TRUE &
     all((names(files_pat$pid) == "elig") == FALSE) == FALSE) {
    files_pat$pid <- files_pat$pid %>%
      rename(elig_ma = elig) 
  }
  
  # Many datasets have COMPO_UN but no SERONAME:
  if(all((names(files_pat$sero) == "seroname") == FALSE) == TRUE &
     all((names(files_pat$sero) == "compo_un") == FALSE) == FALSE) {
    files_pat$sero <- files_pat$sero %>%
      rename(seroname = compo) 
  }
  
  if(study_id == "208136-018") {
    # Adding city name 
    files_pat$pid <- files_pat$pid %>%
      mutate(cty_nam = "Singapore")
    # Date of vaccination in dates file and adding vaccine information 
    # (GP 1 = MMRV and GP 2 = VARICELLA + MMR)
    files_pat$expo <- files_pat$expo %>%
      left_join(select(files_pat$dates, pid, vc_1, vc_2) %>%
                  pivot_longer(cols = c(vc_1, vc_2), names_to = "activity", values_to = "p_rdat") %>%
                  mutate(activity = case_when(activity == "vc_1" ~ "10", activity == "vc_2" ~ "30")) %>%
                  filter(!is.na(p_rdat))) %>%
      group_by(pid, activity) %>%
      mutate(rn = row_number()) %>%
      ungroup() %>%
      mutate(vaccine = case_when(group_nb == "2" ~ "mmrv",
                                 group_nb == "1" & rn == 1 ~ "mmr", 
                                 group_nb == "1" & rn == 2 ~ "varicella", 
                                 .default = vaccine)) 
    # Date of serology in dates file
    files_pat$dates <- files_pat$dates %>%
      select(pid, bs_1, bs_2, bs_3) %>%
      pivot_longer(cols = c(bs_1, bs_2, bs_3), names_to = "activity", values_to = "raw_date") %>%
      mutate(activity = case_when(activity == "bs_1" ~ "10", 
                                  activity == "bs_2" ~ "20", 
                                  activity == "bs_3" ~ "40"),
             type_lst = "BL") %>%
      filter(!is.na(raw_date))
    # Adding serology method
    files_pat$sero <- files_pat$sero %>%
      mutate(method = NA) 
  }
  
  if(study_id == "208136-022") {
    # Adding city name 
    files_pat$pid <- files_pat$pid %>%
      mutate(cty_nam = "Brazil")
    # Date of vaccination in dates file
    files_pat$expo <- files_pat$expo %>%
      left_join(select(files_pat$dates, pid, vc_1) %>%
                  pivot_longer(cols = c(vc_1), names_to = "activity", values_to = "p_rdat") %>%
                  mutate(activity = case_when(activity == "vc_1" ~ "10")) %>%
                  filter(!is.na(p_rdat)))
    # Date of serology in dates file
    files_pat$dates <- files_pat$dates %>%
      select(pid, bs_1, bs_2) %>%
      pivot_longer(cols = c(bs_1, bs_2), names_to = "activity", values_to = "raw_date") %>%
      mutate(activity = case_when(activity == "bs_1" ~ "10", 
                                  activity == "bs_2" ~ "20"),
             type_lst = "BL") %>%
      filter(!is.na(raw_date))
    # Adding serology method
    files_pat$sero <- files_pat$sero %>%
      mutate(method = NA) 
  }
  
  if(study_id == "209762-147") {
    # Adding city name 
    files_pat$pid <- files_pat$pid %>%
      mutate(cty_nam = "Singapore")
    # Date of vaccination in dates file
    files_pat$expo <- files_pat$expo %>%
      left_join(select(files_pat$dates, pid, vc_1) %>%
                  pivot_longer(cols = c(vc_1), names_to = "activity", values_to = "p_rdat") %>%
                  mutate(activity = case_when(activity == "vc_1" ~ "10")) %>%
                  filter(!is.na(p_rdat)))
    # Date of serology in dates file
    files_pat$dates <- files_pat$dates %>%
      select(pid, bs_1, bs_2) %>%
      pivot_longer(cols = c(bs_1, bs_2), names_to = "activity", values_to = "raw_date") %>%
      mutate(activity = case_when(activity == "bs_1" ~ "10", 
                                  activity == "bs_2" ~ "20"),
             type_lst = "BL") %>%
      filter(!is.na(raw_date))
    # Adding serology method
    files_pat$sero <- files_pat$sero %>%
      mutate(method = NA) 
  }
  
  if(study_id == "209762-136") {
    # Treatmnt in group_ef
    files_pat$pid <- files_pat$pid %>%
      mutate(treatmnt = group_ef)
    # Date of serology in dates file (not explicit)
    files_pat$dates <- files_pat$dates %>%
      mutate(type_lst = case_when(activity == "10" & type_lst == "VIS" ~ "BL",
                                  activity == "20" & type_lst == "VIS" ~ "BL",
                                  activity == "30" & type_lst == "VIS" ~ "BL",
                                  activity == "40" & type_lst == "VIS" ~ "BL",
                                  .default = type_lst))
  }
  
  if(study_id == "103388") {
    # Country in demo file
    files_pat$pid <- files_pat$pid %>%
      select(-cty_nam) %>%
      left_join(select(files_pat$demo, pid, cty_nam))
  }
  
  if(study_id == "110058") {
    # Eliminate method "Anti-Var gp (ELU/ML)" from study 110058 (just leave ELISA method with no NAs)
    files_pat$sero <- files_pat$sero %>%
      filter(seroname != "Anti-Var gp")
  }
  
  return(files_pat)
}



















































