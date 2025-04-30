################################################################################
# DATA TRANSFORMATION FOR VAR
# desc: runs the indexations, deflations and log transformations
################################################################################

# Load packages
library(readxl)
library(writexl)
library(dplyr)
library(seasonal)
library(tseries)
library(zoo)

################################################################################
# Load dataset
df <- read_excel(file.path(VARDATA, "input_data.xlsx"))

################################################################################
# Adjust sample date
#df <- df %>%
#  filter(date >= "2008Q1" & date <= "2024Q1")

################################################################################
# Rate to index: mpinfl 
df$mpinfl_ind <- NA 

# Set first 4 values to 100 (arbitrary base), no inflation data to use
df$mpinfl_ind[1:4] <- 100

# Loop from 5th quarter onward
for (i in 5:nrow(df)) {
  df$mpinfl_ind[i] <- df$mpinfl_ind[i - 4] * (1 + df$mpinfl[i] / 100)
}

################################################################################
# Reindex: gdpdefl
df$gdpdefl_reind <- df$gdpdefl / df$gdpdefl[1] * 100

################################################################################
# Deseas: avgwage
# Convert to ts object (quarterly frequency, e.g., starting in 2000 Q1)
avgwage_ts <- ts(df$avgwage, start = c(2004, 1), frequency = 4)

# Apply seasonal adjustment using X13-ARIMA-SEATS via seasonal package
avgwage_sa <- seas(avgwage_ts)

# Extract seasonally adjusted series
df$avgwage_sa <- final(avgwage_sa)

################################################################################
# Deflate: gdp, ndcons, avgwage, cloan, wbill
# List of variables to deflate
vars_deflate <- c("gdp", "ndcons", "avgwage_sa", "cloan", "wbill")

# Loop over variables and create *_defl columns
for (v in vars_deflate) {
  defl_var <- paste0(v, "_real")
  df[[defl_var]] <- df[[v]] / df$mpinfl_ind * 100
}

################################################################################
# Log-transform: cpind, hpind, mpinfl_ind, cloan, spind, ndcons, avgwage, gdp, empl, wbill
# List of variables to log-transform
vars_log <- c("cpind", "hpind", "mpinfl_ind", "cloan_real", "psx", 
                 "ndcons_real", "avgwage_sa_real", "gdp_real", "empl", "wbill_real", "er")

# Loop over variables and create *_log columns
for (v in vars_log) {
  log_var <- paste0(v, "_log")
  df[[log_var]] <- log(df[[v]])*100
}

################################################################################
# First difference: cpind, hpind, mpinfl_ind, cloan, spind, ndcons, avgwage, gdp, empl, wbill
# List of variables to log-transform
vars_diff <- c("cpind_log", "hpind_log", "cloan_real_log", "psx_log", 
              "ndcons_real_log", "avgwage_sa_real_log", "gdp_real_log", "empl_log", "wbill_real_log", "er_log")

# Loop through and create *_diff variables
for (v in vars_diff) {
  diff_var <- paste0(v, "_diff")
  df[[diff_var]] <- c(NA, diff(df[[v]]))
}

################################################################################
# Manually input inflation target
df$infltar <- c(
  3.5, 3.4, 3.4, 3.3, 3.3, 3.2, 3.1,                       # 2004 Q1–2005 Q3
  rep(3, 7),                                               # 2005 Q4-2007 Q4
  2.889, 2.778, 2.667, 2.556, 2.445, 2.334, 2.223, 2.112,  # 2008–2009
  rep(2, length(df$date) - 22)                             # 2010 Q1 onward to end of sample
)

# Subtract the target from 3MPRIBOR and mpinfl
df$`3MPRIBOR_wotar` <- df$`3MPRIBOR` - df$infltar
df$mpinfl_wotar <- df$mpinfl - df$infltar

################################################################################
# Save the dataset
write_xlsx(df, path = file.path(OUTPUT, "output_data.xlsx"))
# View(df)