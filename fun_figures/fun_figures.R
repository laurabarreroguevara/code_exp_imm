####################################################################################################
# Functions for descriptive plots
####################################################################################################

## Figure 1a.1: Histogram showing the number of participants per study for each pathogen
fun_figpar <- function(x, xlim = c(0, 22500)) {
  x %>%
  # Obtain number of participants per study 
  group_by(pat, pat_tidy, study) %>%
  summarise(tot = n(), .groups = "drop") %>%
  # Obtain proportion of participants per pathogen
  group_by(pat, pat_tidy) %>%
  arrange(pat, tot) %>%
  mutate(tot_prop = tot/sum(tot)) %>%
  # Plot
  ggplot(aes(y = fct_rev(pat_tidy), x = tot, group = reorder(study,tot), fill = tot)) +
  geom_bar(stat = "identity", color = "white", size = 0.1) + 
  geom_label(data = distinct(x, pat_tidy, study, pid) %>%
               group_by(pat_tidy) %>%
               summarise(study_tot = length(unique(study)), tot = n()), 
             aes(x = tot + 800, label = as.character(study_tot), y = fct_rev(pat_tidy)), 
             fill = "white", color = "black", inherit.aes = FALSE, size = 2) + 
  scale_x_continuous("Total vaccinated individuals", expand = c(0, 0), limits = xlim) +
  scale_y_discrete("") + 
  scale_fill_viridis_c("Vaccinated individuals\nper study", option = "rocket", trans = "log10") + 
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        panel.grid.major.y = element_line())
}

## Figure 1a.2: Plot showing age at first vaccination (in months) by pathogen
fun_figage <- function(x, xbreaks = c(0, 2, 4, 6, 12, 18, 24, 36, 48, 60, 72, 84),
                       xlim = c(0, 24)) {
  x %>%
  # Obtain age in weeks 
  mutate(age = round((age*7)/30, digits = 0)) %>%
  # Obtain number of individuals per age  
  group_by(pat_tidy, pat, age) %>%
  summarise(tot = n(), .groups = "drop") %>%
  group_by(pat_tidy, pat) %>%
  mutate(prop = tot/sum(tot)) %>%
  # Plot
  ggplot(aes(x = age, y = fct_rev(pat_tidy), size = prop)) +
  geom_point(alpha = 1, color = "black") + 
  scale_size("Proportion individuals\nper age") +
  scale_x_continuous("Age at first vaccination (Months)",
                     breaks = xbreaks, limits = xlim) +
  scale_y_discrete("") + 
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_line())
}

## Figure 1a.3: Stacked bar plot showing the sex ratio by pathogen
fun_figsex <- function(x) {
  x %>%
  ggplot(aes(y = fct_rev(pat_tidy), fill = sex)) +
    geom_bar(color = "white", position="fill") + 
    scale_x_continuous("Sex proportion", breaks = c(0, 0.5, 1), expand = c(0,0)) + 
    scale_fill_viridis_d("Sex", option = "rocket", begin = 0.6, end = 0.9, breaks = c("M", "F")) + 
    guides(fill = guide_legend(ncol = 1)) + 
    theme(legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(color = "white"),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()) +
    facet_grid("space"~.)
}

## Figure 1b.1: Time-series plots of vaccination weeks per region
fun_figvacc <- function(x) {
  x %>%
    # Obtain total participants per week and region
    group_by(pat, pat_tidy, vac_week_v1, reg_6) %>%
    summarise(tot = n(), .groups = "drop") %>%
    group_by(pat, pat_tidy) %>%
    complete(vac_week_v1, reg_6, fill = list(tot = 0)) %>%
    # Plot
    ggplot(aes(x = vac_week_v1, y = tot, color = reg_6)) +
    geom_step() +
    facet_grid(pat_tidy~., scales = "free_y") +
    scale_x_continuous("Week of\nvaccination", breaks = c(1, 18, 35, 52), expand = c(0,0)) +
    scale_y_continuous("Count", breaks = fun_breaks, limits = fun_limits, expand = c(0,0)) +
    scale_color_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
    ggtitle("Count") +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          legend.position = "none",
          panel.spacing.y = unit(0.7, "lines"),
          plot.title = element_text(size = 11),
          axis.title.y = element_blank())
}

# Figure 1b.2: Density plots of immunogenicity results per region
fun_figimm <- function(x) {
  x %>%
    ggplot(aes(x = imm_val_s1, fill = reg_6, color = reg_6)) +
    geom_vline(aes(xintercept = imm_cutoff_s1), color = "grey") + 
    geom_density(alpha = 0.1, stat = "ecdf") + 
    facet_grid(pat_tidy~., scales = "free_y") +
    scale_x_log10("Immunogenicity", expand = c(0,0),
                  breaks = c(1, 100, 10000),
                  labels = c("1", expression(10^2), expression(10^4))) + 
    scale_y_continuous("Density", expand = c(0,0), breaks = fun_breaks, limits = fun_limits) + 
    scale_color_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
    scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
    ggtitle("Density") +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          panel.spacing.y = unit(0.7, "lines"),
          legend.position = "none",
          plot.title = element_text(size = 11),
          axis.title.y = element_blank(),
          plot.margin = margin(t = 5.5, r = 0, b = 5.5, l = 5.5)) 
}

# Figure 1b.3: Maps of study locations for each subgroup of pathogens
fun_figmap <- function(x, x_agg) {
  bind_rows(lapply(unique(x$pat), function(pat_x) {
    mutate(world, 
           pat = pat_x,
           dat = case_when(region %in% (filter(x, pat == pat_x))$country ~ TRUE, .default = FALSE)
    )})) %>%
    # Add tidy names for the pathogens 
    left_join(tidy_names_pat, by = join_by(pat)) %>%
    # Plot 
    ggplot(aes(x = long, y = lat, group = group, fill = dat)) +
    facet_grid(pat_tidy~.) +
    geom_polygon() +
    geom_point(data = filter(x_agg, pathogen %in% x$pat),
               aes(x = lon_med, y = lat_med, color = clu_tidy, size = tot, group = 1, fill = NA), 
               alpha = 0.4) + 
    geom_point(data = filter(x_agg, pathogen %in% x$pat),
               aes(x = lon_med, y = lat_med, color = clu_tidy, size = tot, group = 1, fill = NA), 
               shape = 1) +
    scale_x_continuous("Longitude", expand = c(0, 0), labels = fun_lablon) + 
    scale_y_continuous("Latitude", breaks = c(-23, 0, 23), limits = c(-60, 80), expand = c(0, 0),
                       labels = fun_lablat) +
    scale_size_continuous(breaks = c(50, 500, 5000), limits = c(1,5000)) + 
    scale_color_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
    scale_fill_manual(values = c("grey90", "grey75")) + 
    guides(
      colour = guide_legend(title = "Latitudinal \nclassification", order = 1, nrow = 2, byrow = TRUE,
                            keyheight = unit(8, "pt")),
      size = guide_legend(title = "Vaccinated \nindividuals", order = 2, keyheight = unit(8, "pt"),
                          override.aes = list(alpha = 1)),
      shape = "none", fill = "none") +
    ggtitle("Latitude (°)") +
    theme(legend.position = "top",
          strip.background = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(size = 11),
          axis.title.y = element_blank(),
          plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 0)) + 
    coord_quickmap()
}

####################################################################################################
# END
####################################################################################################
