################################################################################
# INCIDENCE FUNCTION ESTIMATION ON 3 HtM GROUPS
# desc: estimate the incidence function and normalize the coefficients
################################################################################

# Load packages
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(cowplot)

################################################################################
# Load dataset
df <- read.csv(file.path(OUTPUT, "CSU_data_wRP_wHtMstat.csv"))

################################################################################
# Estimation

# Merge top10 and nHtM if necessary
if (top10Index == 1) {
  df$htm_g[df$htm_g == 4] <- 3
}

# Option to estimate joint elasticity for both HtM groups
joinHtMgroups <- 0
if (joinHtMgroups == 1) {
  df$htm_g[df$htm_g == 2] <- 1
}

# Filter age between 15 and 64
df <- df %>%
  filter(age >= 15 & age <= 64)

# Rename "empl" to "rp_empl"
df <- df %>% rename(rp_empl = empl)

# Initialize columns for regression coefficients
rates <- c("rp_empl")

# Time trend
df <- df %>%
  arrange(date_q) %>%
  mutate(time_index = as.integer(factor(date_q)))
#summary(df$time_index)

# Run the estimation
for (dep in rates) {
  df[[paste0("beta_", dep)]] <- NA
  df[[paste0("ub_beta_", dep)]] <- NA
  df[[paste0("lb_beta_", dep)]] <- NA
  
  htm_types <- unique(df$htm_g)
  
  for (type in htm_types) {
    filtered_data <- df %>%
      filter(htm_g == type)
    
    if (nrow(filtered_data) > 0) {
      #model <- lm(reformulate(c("date_q", "agg_empl"), dep), data = filtered_data)
      model <- lm(reformulate(c("agg_empl", "time_index"), dep), data = filtered_data,weights = filtered_data$wgthh)
      
      beta <- coef(model)["agg_empl"]
      se <- summary(model)$coefficients["agg_empl", "Std. Error"]
      ub <- beta + 1.96 * se
      lb <- beta - 1.96 * se
      
      df <- df %>%
        mutate(
          !!paste0("beta_", dep) := if_else(htm_g == type, beta, !!sym(paste0("beta_", dep))),
          !!paste0("ub_beta_", dep) := if_else(htm_g == type, ub, !!sym(paste0("ub_beta_", dep))),
          !!paste0("lb_beta_", dep) := if_else(htm_g == type, lb, !!sym(paste0("lb_beta_", dep)))
        )
    }
  }
}

# Collapse dataset into data on incidence functions
incidence_results <- df %>%
  group_by(htm_g) %>%
  summarise(across(starts_with("beta_"), mean, na.rm = TRUE),
            across(starts_with("ub_beta_"), mean, na.rm = TRUE),
            across(starts_with("lb_beta_"), mean, na.rm = TRUE),
            .groups = "drop")
#View(incidence_results)

# Save the dataset
write.csv(incidence_results, file.path(TABLES, paste0("CSU_incifunc_3htm_", verCalib, "calib_", estMethod, "_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), ".csv")), row.names = FALSE)

################################################################################
# Merge income shares of HtM types from external source
# when normalizing by population shares, use "_HtM_shares.csv" and divide the shares by 100
inc_shares <- read.csv(file.path(HFCSOUTPUT, paste0(HFCSDBNAME,"_HtM_inc_shares.csv")))

if (top10Index == 1) {
  inc_shares[1,4] = inc_shares[1,4] + inc_shares[1,5]
}

inc_shares_long <- inc_shares %>%
  pivot_longer(cols = c("incs_pHtM", "incs_wHtM", "incs_nHtM"),
               names_to = "htm_g",
               values_to = "inc_sh")

if (joinHtMgroups == 1) {
  new_value <- sum(inc_shares_long$inc_sh[inc_shares_long$htm_g %in% c("pHtM", "wHtM")])
  inc_shares_long$inc_sh[inc_shares_long$htm_g == "pHtM"] <- new_value
  inc_shares_long <- inc_shares_long[inc_shares_long$htm_g != "wHtM", ]
}

# Recode incidence_results' htm_g numeric values into the same text labels
incidence_results <- incidence_results %>%
  mutate(htm_g = recode(as.character(htm_g),
                        "1" = "incs_pHtM",
                        "2" = "incs_wHtM",
                        "3" = "incs_nHtM"))

# Merge the incidence shares with the incidence results by HtM group
incidence_results_norm <- incidence_results %>%
  left_join(inc_shares_long, by = c("htm_g"))

# Compute the weighted average beta and the scaling factor
incidence_results_norm <- incidence_results_norm %>%
  group_by(sa0100) %>%
  mutate(weighted_beta = sum(beta_rp_empl * inc_sh, na.rm = TRUE),
         scfac = 1 / weighted_beta,
         beta_rp_empl_norm   = beta_rp_empl * scfac,
         ub_beta_rp_empl_norm = ub_beta_rp_empl * scfac,
         lb_beta_rp_empl_norm = lb_beta_rp_empl * scfac) %>%
  ungroup() %>%
  select(-beta_rp_empl, -ub_beta_rp_empl, -lb_beta_rp_empl)
View(incidence_results_norm)

# Save the dataset
write.csv(incidence_results_norm, file.path(TABLES, paste0("CSU_incifunc_3htm_", verCalib, "calib_", estMethod, "_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_norm.csv")), row.names = FALSE)
#incidence_results_norm <- read.csv(file.path(TABLES,(paste0("CSU_incifunc_", 3, "htm_", verCalib, "calib_", estMethod, "_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_norm.csv"))))

################################################################################
# Visualise the estimates and confidence intervals
if (joinHtMgroups == 1) {
  {}
} else {
  # Rename htm_g labels
  incidence_results_plot <- incidence_results_norm %>%
    mutate(HtMstatus = factor(
      htm_g,
      levels = c("incs_pHtM", "incs_wHtM", "incs_nHtM"),
      labels = c("Poor HtM", "Wealthy HtM", "Non-HtM")
    ))

  # Plot incidence function estimates
  incidence_plot <- ggplot(incidence_results_plot, 
                           aes(x = HtMstatus, ymin = lb_beta_rp_empl_norm, ymax = ub_beta_rp_empl_norm, fill = HtMstatus)) +
    geom_rect(aes(xmin = as.numeric(HtMstatus) - 0.25, 
                  xmax = as.numeric(HtMstatus) + 0.25, 
                  ymin = lb_beta_rp_empl_norm, 
                  ymax = ub_beta_rp_empl_norm)) + 
    geom_errorbar(aes(ymin = beta_rp_empl_norm, ymax = beta_rp_empl_norm), 
                  width = 0.5, color = "black", linewidth = 1.5) +
    #geom_text(aes(y = beta_rp_empl_norm, label = sprintf("%.2f", beta_rp_empl_norm)), 
    #          vjust = -0.5, size = 6, color = "black") + 
    scale_fill_manual(values = c("Poor HtM" = "#004488", 
                                 "Wealthy HtM" = "#88CCEE", 
                                 "Non-HtM" = "#FFA500")) +
    labs(
      title = "Elasticity of Individual Employment to Aggregate",
      subtitle = "by Hand-to-Mouth Status",
      x = "",
      y = "Normalized Elasticity Estimate",
      fill = ""
    ) +
    theme_minimal(base_size = 16) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      legend.position = "bottom",
      legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0),
      plot.margin = margin(t = 10, r = 10, b = 1, l = 10),
      panel.grid.major.x = element_blank()
    )

  # Add annotation below the legend
  annotation <- ggdraw() +
    draw_label(
      if (imputationModelChoice %in% c(1, 2)) {
        paste0("Source: LFSS, 2011-2023. Country: ", unique(incidence_results_plot$sa0100), ".")
      } else {
        paste0("Source: LFSS, 2008-2023. Country: ", unique(incidence_results_plot$sa0100), ".")
      },
      fontface = "italic",
      x = 0.5, hjust = 0.5, size = 14, color = "gray40"
    )
  
  # Combine the plot and annotation
  incidence_plot <- plot_grid(incidence_plot, annotation, ncol = 1, rel_heights = c(1, 0.05))
  #incidence_plot
  
  # Save plot
  ggsave(paste0("incidence_plot_3htm_", verCalib, "calib_", estMethod, "_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_norm.png"), plot = incidence_plot, path = file.path(OUTPUT, "graphs"), bg = "white", width = 7, height = 7)
}