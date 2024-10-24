####################################################################################################
# Purpose of script: Function to plot the conditional effects and predictions of models 
# with clusters
####################################################################################################

library(colorspace)

fun_mod_pl <- function(mod = brm_s0$bm1ds0_fs_cc_kgc_1.rds,
                       mod_save = "bm1ds0_fs_cc_kgc_1.rds",
                       dat = dat) {
                        
clu_name <- str_subset(names(mod$data), "kgc|reg|prec|tavg|dayl|clilat")
mod_save <- str_remove(mod_save, ".rds")

n_vars <- length(unique((mod$data %>%
  rename(clu = all_of(clu_name)))$clu))

if(n_vars > 8) wn_plot = 12
if(n_vars <= 8) wn_plot = 6

# Histogram
pl1_his <- mod$data %>%
  rename(clu = all_of(clu_name)) %>%
  mutate(clu = as.character(clu)) %>%
  ggplot(aes(x = pi_vac_week_v1, color = clu, fill = clu)) + 
  geom_histogram(color = "white") +
  scale_y_continuous("Individuals") + 
  scale_x_continuous("Week") + 
  scale_fill_discrete_divergingx(palette = "Temps") + 
  facet_wrap(~clu, ncol = 8) + 
  theme_classic() + 
  theme(legend.position = "none",
        strip.background = element_blank())

# Weekly data
pl1_dat <- dat %>%
  mutate(study_country = glue("{study} ({country})")) %>% 
  rename(clu = all_of(clu_name)) %>%
  mutate(clu = as.character(clu)) %>%
  group_by(vac_week_v1, clu) %>%
  summarise(mean_imm = mean(imm_val_s1),
            sd_imm = sd(imm_val_s1),
            tot = n(),
            tot_s = length(unique(study))) %>%
  ggplot(aes(x = vac_week_v1, y = log(mean_imm), size = tot, color = tot_s)) + 
  geom_point() + 
  scale_y_continuous("IgA anti-rota (log)") + 
  scale_x_continuous("Week", breaks = c(1,18,35,52), limits = c(1,52)) + 
  scale_color_viridis_c("Number of studies", breaks = c(2,4,6,8,10)) +
  scale_size_continuous("Individuals", breaks = c(10, 50, 100, 150, 200)) + 
  guides(color = guide_legend(ncol = 1)) + 
  facet_wrap(~clu, scales = "free", ncol = 8) + 
  theme_classic() + 
  theme(legend.position = "right",
        legend.justification = "bottom",
        strip.background = element_blank())

# Conditional effects 
ce_dat <- conditional_effects(mod, resolution = 52, effects = glue("pi_vac_week_v1:{clu_name}"))[[1]] %>%
  select(pi_vac_week_v1, starts_with(clu_name), estimate__, lower__, upper__) %>%
  left_join(dat) %>%
  mutate(estimate__ = case_when(is.na(imm_val_s1) ~ NA, .default = estimate__),
         lower__ = case_when(is.na(imm_val_s1) ~ NA, .default = lower__),
         upper__ = case_when(is.na(imm_val_s1) ~ NA, .default = upper__)) %>%
  mutate(study_country = glue("{study} ({country})")) %>% 
  #mutate(imm_val_s1 = case_when(imm_val_s1 < 20 ~ 20, .default = imm_val_s1)) %>%
  rename(clu = clu_name) %>%
  mutate(clu = as.character(clu)) %>%
  group_by(vac_week_v1, clu) %>%
  mutate(mean_imm = mean(imm_val_s1),
         sd_imm = sd(imm_val_s1),
         tot = n(),
         tot_s = length(unique(study)))

#if(str_detect(mod_save, "fs") == TRUE) {
pl1_ceff <- ce_dat %>%
  ggplot(aes(x = vac_week_v1, y = log(mean_imm), color = clu, fill = clu)) + 
  geom_ribbon(aes(ymin = log(lower__), ymax = log(upper__)), alpha = 0.3, color = "white") + 
  geom_line(aes(y = log(estimate__))) + 
  scale_y_continuous("IgA anti-rota (log)") + 
  scale_x_continuous("Week", breaks = c(1,18,35,52), limits = c(1,52)) + 
  scale_size_continuous(breaks = c(10,50,100,150,200)) + 
  scale_fill_discrete_divergingx(palette = "Temps") + 
  scale_color_discrete_divergingx(palette = "Temps") + 
  facet_wrap(~clu, scales = "free", ncol = 8) + 
  theme_classic() + 
  theme(legend.position = "none",
        strip.background = element_blank())
#} 

# if(str_detect(mod_save, "cs") == TRUE) {
#   pl1_ceff <- ce_dat %>%
#     ggplot(aes(x = vac_week_v1, y = log(mean_imm), color = clu, fill = clu)) + 
#     geom_ribbon(aes(ymin = log(lower__), ymax = log(upper__)), alpha = 0.3, color = "white") + 
#     geom_line(aes(y = log(estimate__))) + 
#     scale_y_continuous("IgA anti-rota (log)") + 
#     scale_x_continuous("Week", breaks = c(1,18,35,52), limits = c(1,52)) + 
#     scale_size_continuous(breaks = c(10,50,100,150,200)) + 
#     scale_fill_discrete_divergingx(palette = "Temps") + 
#     scale_color_discrete_divergingx(palette = "Temps") + 
#     facet_wrap(~clu, ncol = 6) + 
#     theme_classic() + 
#     theme(legend.position = "none",
#           strip.background = element_blank())
#   
#   } 

#Predictions
pred <- predict(mod)

pl1_pred <- dat %>%
  mutate(pred_m = pred[,1],
         q2.5 = pred[,3],
         q97.5 = pred[,4]) %>%
  mutate(study_country = glue("{study} ({country})")) %>%
  rename(clu = clu_name) %>%
  mutate(clu = as.character(clu)) %>%
  group_by(vac_week_v1, clu) %>%
  summarise(mean_pred_m = mean(pred_m),
            mean_imm = mean(imm_val_s1),
            sd_imm = sd(imm_val_s1),
            tot = n(),
            tot_s = length(unique(study))) %>%
  ggplot(aes(x = vac_week_v1, y = log(mean_pred_m), size = tot)) +
  geom_point(aes(y = log(mean_imm)), color = "grey", alpha = 0.5) +
  geom_point(color = "red", alpha = 0.5) +
  scale_y_continuous("IgA anti-rota (log)") +
  scale_x_continuous("Week", breaks = c(1,18,35,52), limits = c(1,52)) +
  scale_size_continuous("Individuals", breaks = c(10, 50, 100, 150, 200)) +
  facet_wrap(~clu, scales = "free", ncol = 8) +
  theme_classic() +
  theme(legend.position = "right",
        legend.justification = "bottom",
        strip.background = element_blank())

pl_all <- (pl1_his / pl1_ceff) | (pl1_dat / pl1_pred)
ggsave(filename = glue("{dirs$outputs}/_figures/{mod_save}.pdf"), plot = pl_all,  width = 20, height = wn_plot)

return(pl_all)

}
####################################################################################################
# Purpose of script: Function to extract lon lat from cities of first-level divisions
####################################################################################################