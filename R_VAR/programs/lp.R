################################################################################
# LOCAL PROJECTION
# desc: runs local projection as an alternative to VAR models
################################################################################

# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(ivreg)
library(ggplot2)
library(patchwork)

################################################################################
# Load dataset
df <- read_excel(file.path(OUTPUT, "output_data.xlsx"))

################################################################################
# Convert string dates like "2008Q1" to proper quarterly date format
df <- df %>%
  mutate(date = as.yearqtr(date, format = "%Yq%q")) %>%
  filter(date >= as.yearqtr("2008 Q1") & date <= as.yearqtr("2024 Q1"))

################################################################################
# Set variables
endog_vars <- c("3MPRIBOR", "mpinfl", "empl_log", "avgwage_sa_real_log", "hpind_log", 
                "psx_log", "ndcons_real_log", "gdp_real_log", "er_log")
shock_var <- "3MPRIBOR"
proxy_var <- "shock_d_FRA_1X4_12"
max_horizon <- 20
n_lags <- 4

# Create lagged controls
for (lag in 1:n_lags) {
  df <- df %>%
    mutate(!!paste0(shock_var, "_lag", lag) := dplyr::lag(get(shock_var), lag))
}

################################################################################
# Initialize output
lp_results <- list()

# Loop over horizons and variables
for (y_var in endog_vars) {
  irf <- data.frame(h = 0:max_horizon, beta = NA, se = NA)
  
  for (h in 0:max_horizon) {
    # Create lead of dependent variable
    df <- df %>%
      mutate(y_lead = dplyr::lead(get(y_var), h))
    
    # First stage: instrument shock_var with proxy_var
    first_stage <- lm(as.formula(paste0("`", shock_var, "` ~ ", proxy_var)), data = df)
    df$fitted_shock <- fitted(first_stage)
    
    # Second stage: local projection regression
    formula_lp <- as.formula(paste0(
      "y_lead ~ fitted_shock + ",
      paste0("`", shock_var, "_lag", 1:n_lags, "`", collapse = " + ")
    ))
    
    model_lp <- lm(formula_lp, data = df)
    beta_hat <- coef(summary(model_lp))["fitted_shock", "Estimate"]
    se_hat <- coef(summary(model_lp))["fitted_shock", "Std. Error"]
    
    irf$beta[h + 1] <- beta_hat
    irf$se[h + 1] <- se_hat
  }
  
  lp_results[[y_var]] <- irf
}

# Rescale IRFs to -1 pp policy shock at impact (if needed)
scaling_factor <- -1 / lp_results[["3MPRIBOR"]]$beta[1]  # horizon 0
lp_results <- lapply(lp_results, function(df) {
  df %>% mutate(across(c(beta, se), ~ .x * scaling_factor))
})

################################################################################
# Plot
theme_set(theme_minimal(base_family = "Arial", base_size = 8))

# Individual plots
irf_plots <- lapply(names(lp_results), function(varname) {
  df <- lp_results[[varname]] %>%
    mutate(
      lower90 = beta - 1.645 * se,
      upper90 = beta + 1.645 * se,
      lower68 = beta - 1.000 * se,
      upper68 = beta + 1.000 * se
    )
  
  ggplot(df, aes(x = h, y = beta)) +
    geom_ribbon(aes(ymin = lower90, ymax = upper90), fill = "#a9c9ff", alpha = 0.5) +
    geom_ribbon(aes(ymin = lower68, ymax = upper68), fill = "#4a90e2", alpha = 0.6) +
    #geom_ribbon(aes(ymin = lower90, ymax = upper90), fill = "#d2ddf4", alpha = 0.5) +
    #geom_ribbon(aes(ymin = lower68, ymax = upper68), fill = "#8caedb", alpha = 0.7) +
    geom_line(color = "black", size = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.2, color = "black") +
    labs(title = varname, x = "quarters", y = "% or p.p.") +
    theme_minimal(base_size = 6) +
    theme(
      plot.title = element_text(face = "bold", size = 8, hjust = 0.5, color = "black"),
      axis.title = element_text(size = 2, color = "black"),
      axis.text = element_text(size = 2, color = "black"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(4, 4, 4, 4)
    )
})

# Grid 3x3
irf_report <- wrap_plots(irf_plots, ncol = 3, nrow = 3) +
  plot_annotation(
    title = "Impulse responses to -100 bp monetary policy shock",
    theme = theme(
      plot.title = element_text(
        size = 10, face = "bold", family = "Arial", color = "black", hjust = 0.5,
        margin = margin(b = -5)
      )
    )
  ) &
  theme(
    axis.title = element_text(size = 8, color = "black", family = "Arial"),
    axis.text = element_text(size = 8, color = "black", family = "Arial"),
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save 
ggsave(
  filename = file.path(OUTPUT, "irf_grid_lpiv.jpg"),
  plot = irf_report,
  width = 1.1*11.69,
  height = 8.27,
  units = "in",
  dpi = 300,
  device = "jpeg"
)
