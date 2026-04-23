####################################################################################################
# Function to plot waic
####################################################################################################

# WAIC plot
fun_figwaic <- function(x) {
  x %>%
    # Create dummy variable for the order 
    mutate(mod_name_f = tidytext::reorder_within(mod_name_tidy, -waic_diff, pat_tidy)) %>%
    ggplot(aes(x = waic_diff, y = mod_name_f, color = mod_name_tidy)) +
    geom_vline(aes(xintercept = 2), color = "grey") + 
    geom_point(size = 2) +
    geom_segment(aes(x = 0, xend = waic_diff)) + 
    scale_x_continuous(expression(Delta*WAIC~"from best model")) +
    tidytext::scale_y_reordered() + 
    scale_color_viridis_d("Model", option = "rocket", end = 0.8) + 
    facet_nested_wrap(mod_name_tidy_best+pat_tidy~., ncol = 1, scales = "free", 
                      strip.position = "right") + 
    theme(legend.position = "top",
          strip.background = element_blank(),
          strip.text =  element_blank(),
          axis.title.y = element_blank())
}

# Predictions plot 
fun_figpred <- function(x) {
  filter(x, type == "1draw") %>%
    ggplot(aes(x = value, group = name_draw, color = clu_tidy)) + 
    geom_vline(aes(xintercept = imm_cutoff_s1), color = "grey") + 
    geom_line(stat = "ecdf", alpha = 0.1) + 
    geom_line(data = filter(x, type == "2data"), stat = "ecdf", linetype = "dashed") +  
    facet_nested_wrap(mod_name_tidy_best+pat_tidy~., ncol = 1, scales = "free", 
                      strip.position = "right", nest_line = TRUE, solo_line = TRUE) + 
    scale_x_continuous("Immunogenicity", trans = "log10",
                       breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
    scale_y_continuous("ECDF") + 
    scale_color_manual("Latitudinal\nclassification", values = col_clu) + 
    theme(legend.position = "none")
}

# Combined WAIC plot
fun_figwaiccomb <- function(x) {
  x %>%
    # Create dummy variable for the order 
    mutate(mod_name_f = tidytext::reorder_within(mod_name_tidy, -waic_diff, patcomb_tidy)) %>%
    ggplot(aes(x = waic_diff, y = mod_name_f, color = mod_name_tidy)) +
    geom_vline(aes(xintercept = 2), color = "grey") + 
    geom_point(size = 2) +
    geom_segment(aes(x = 0, xend = waic_diff)) + 
    scale_x_continuous(expression(Delta*WAIC~"from best model")) +
    tidytext::scale_y_reordered() + 
    scale_color_viridis_d("Model", option = "rocket", end = 0.8) + 
    facet_nested_wrap(patcomb_tidy~., ncol = 4, scales = "free") + 
    guides(color = guide_legend(nrow = 2)) + 
    theme(legend.position = "top",
          strip.background = element_blank(),
          axis.title.y = element_blank())
}

# Combined predictions plot
fun_figpredcomb <- function(x) {
  filter(x, type == "1draw") %>%
    ggplot(aes(x = value, group = paste0(name_draw, pat_tidy), color = pat_tidy)) + 
    geom_vline(aes(xintercept = imm_cutoff_s1, color = pat_tidy), show.legend = FALSE) + 
    geom_line(stat = "ecdf", alpha = 0.1) + 
    geom_line(data = filter(x, type == "2data"), stat = "ecdf", linetype = "dashed") +  
    facet_nested_wrap(patcomb_tidy+clu_tidy~., ncol = 2, scales = "free", nest_line = TRUE, solo_line = TRUE) + 
    scale_x_continuous("Immunogenicity", trans = "log10",
                       breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
    scale_y_continuous("ECDF") + 
    scale_color_manual("", values = col_patcomb) + 
    guides(color = guide_legend(nrow = 3, override.aes = list(alpha = 1, xintercept = 0))) + 
    theme(legend.position = "top",
          legend.title = element_blank())
}
####################################################################################################
# END
####################################################################################################