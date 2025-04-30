################################################################################
# DATA ANALYSIS FOR VAR
# desc: creates comparison plots, ACF / spectra, lag length selection etc.
################################################################################

# Load packages
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)
library(urca)

library(tidyverse)
library(xts)
library(tseries)
library(seasonal)
library(forecast)
library(vars)
library(mFilter)
library(tsbox)
library(svars)

################################################################################
# Load dataset
df <- read_excel(file.path(OUTPUT, "output_data.xlsx"))

################################################################################
# Convert "2004q1" to yearqtr format
df$date <- as.yearqtr(df$date, format = "%Yq%q")

################################################################################
# List of base variables
var_names_input <- c(
  "shock_d_FRA_1X4_12", "shock_d_FRA_3X6_12", "shock_d_FRA_6X9_12", "3MPRIBOR",
  "cpind", "hpind", "mpinfl", "gdp", "ndcons", "psx", "emplrat", "avgwage_sa",
  "cloan", "clendr", "mpinfl_ind", "empl", "wbill"
)

################################################################################
# Plotting
for (var in var_names_input) {
  
  var_real <- paste0(var, "_real")
  
  if (var_real %in% names(df)) {
    # Both original and deflated version exist
    df_long <- df %>%
      dplyr::select(date, !!var, !!var_real) %>%
      pivot_longer(cols = -date, names_to = "variable", values_to = "value")
    
    p <- ggplot(df_long, aes(x = date, y = value, color = variable)) +
      geom_line(size = 1) +
      labs(title = paste("Comparison of", var, "and", var_real),
           x = "Date", y = "Value", color = "Series") +
      theme_minimal()
    
  } else if (var %in% names(df)) {
    # Only original version exists
    p <- ggplot(df, aes(x = date, y = .data[[var]])) +
      geom_line(color = "steelblue", size = 1) +
      labs(title = paste("Time Series:", var),
           x = "Date", y = "Value") +
      theme_minimal()
  } else {
    next  # Skip if neither exists
  }
  
  print(p)
  readline(prompt = "Press [Enter] to continue to next variable...")
}


################################################################################
# List of variables after log and diff 
var_names_log <- c(
  "shock_d_FRA_1X4_12", "shock_d_FRA_3X6_12", "shock_d_FRA_6X9_12", "3MPRIBOR", "emplrat", "clendr",
  "cpind_log", "hpind_log", "psx_log", "mpinfl_ind_log", "avgwage_sa_real_log", "cloan_real_log",
  "gdp_real_log", "ndcons_real_log", "empl_log", "wbill_real_log")

################################################################################
# Stationarity

# ADF and KPSS
test_results_log <- data.frame(
  variable = character(),
  adf_stat = numeric(),
  adf_pvalue = numeric(),
  kpss_stat = numeric(),
  kpss_pvalue = numeric(),
  stringsAsFactors = FALSE
)

for (v in var_names_log) {
  if (v %in% names(df)) {
    x <- na.omit(df[[v]])  # Remove NAs
    
    # ADF test (H0: unit root, high pvalue implies unit root)
    adf <- adf.test(x, alternative = "stationary")
    
    # KPSS test (H0: stationarity)
    kpss <- kpss.test(x, null = "Level")
    
    # Store results
    test_results_log <- rbind(test_results_log, data.frame(
      variable = v,
      adf_stat = adf$statistic,
      adf_pvalue = adf$p.value,
      kpss_stat = kpss$statistic,
      kpss_pvalue = kpss$p.value
    ))
  }
}
print(test_results_log)

# ACF and spectral density
par(mfrow = c(2, 1))

for (v in var_names_log) {
  if (v %in% names(df)) {
    
    x <- na.omit(df[[v]])  # Remove NA values
    
    # Set up the plot title
    main_title <- paste("ACF and Spectral Density for", v)
    
    # Plot ACF (slow decay suggests non-stationarity)
    acf(x, main = paste("ACF of", v), col = "steelblue", lwd = 2)
    
    # Plot spectral density (trend implies non-stationarity)
    spec.pgram(x, main = paste("Spectrum of", v), col = "darkred", log = "no")
    
    # Pause to view one at a time
    readline(prompt = "Press [Enter] to continue to next variable...")
  }
}

################################################################################
# List of diff variables
var_names_diff <- c(
  "shock_d_FRA_1X4_12", "shock_d_FRA_3X6_12", "shock_d_FRA_6X9_12", "3MPRIBOR", "emplrat_diff", "clendr",
  "cpind_log_diff", "hpind_log_diff", "psx_log_diff", "mpinfl", "avgwage_sa_real_log_diff", "cloan_real_log_diff",
  "gdp_real_log_diff", "ndcons_real_log_diff", "empl_log_diff", "wbill_real_log_diff")

################################################################################
# Stationarity

# ADF and KPSS
test_results_diff <- data.frame(
  variable = character(),
  adf_stat = numeric(),
  adf_pvalue = numeric(),
  kpss_stat = numeric(),
  kpss_pvalue = numeric(),
  stringsAsFactors = FALSE
)

for (v in var_names_diff) {
  if (v %in% names(df)) {
    x <- na.omit(df[[v]])  # Remove NAs
    
    # ADF test (H0: unit root, high pvalue implies unit root)
    adf <- adf.test(x, alternative = "stationary")
    
    # KPSS test (H0: stationarity)
    kpss <- kpss.test(x, null = "Level")
    
    # Store results
    test_results_diff <- rbind(test_results_diff, data.frame(
      variable = v,
      adf_stat = adf$statistic,
      adf_pvalue = adf$p.value,
      kpss_stat = kpss$statistic,
      kpss_pvalue = kpss$p.value
    ))
  }
}
print(test_results_diff)

# ACF and spectral density
par(mfrow = c(2, 1))

for (v in var_names_diff) {
  if (v %in% names(df)) {
    
    x <- na.omit(df[[v]])  # Remove NA values
    
    # Set up the plot title
    main_title <- paste("ACF and Spectral Density for", v)
    
    # Plot ACF (slow decay suggests non-stationarity)
    acf(x, main = paste("ACF of", v), col = "steelblue", lwd = 2)
    
    # Plot spectral density (trend implies non-stationarity)
    spec.pgram(x, main = paste("Spectrum of", v), col = "darkred", log = "no")
    
    # Pause to view one at a time
    readline(prompt = "Press [Enter] to continue to next variable...")
  }
}


################################################################################
# Structural breaks

for (v in var_names_diff) {
  if (v %in% names(df)) {
    x <- na.omit(df[[v]])
    
    cat("\n==========================\n")
    cat("Zivot-Andrews Unit Root Test for:", v, "\n")
    
    # Run all three models
    models <- c("intercept", "trend", "both")
    za_results <- lapply(models, function(m) ur.za(x, model = m, lag = NULL))
    names(za_results) <- models
    
    # Print all summaries
    for (m in models) {
      cat("\n--- Model:", m, "Var:", v, "---\n")
      print(summary(za_results[[m]]))
    }
    
    # Plot all models stacked
    par(mfrow = c(3, 1))
    for (m in models) {
      plot(za_results[[m]]) #, main = paste("ZA Test:", v, "| Model:", m))
    }
    
    # Pause to next variable
    readline(prompt = "Press [Enter] to continue to next variable...")
  }
}

# Chow test (H0: no structural break)
n <- length(df$gdp_real_log)
trend <- c(1:n)
sctest(df$gdp_real_log ~ trend, type = "Chow", point = 19)

################################################################################
# ev. cont. w lag selection, ARIMA,...

#View(df)