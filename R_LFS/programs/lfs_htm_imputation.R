################################################################################
# LFS HtM IMPUTATION
# desc: combines EU-LFS & HFCS, imputes the probabilities of being p/w/n-HtM (or top10) to EU-LFS data
################################################################################

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(Hmisc) # for wtd.quantile
#library(broom)
library(purrr)
#library(ggplot2)

################################################################################
# Load dataset
df1 <- read.csv(file.path(OUTPUT, paste0("LFS_data_noNA_", imputationModelChoice, ".csv")))
df <- df1

################################################################################
# Generate employment rate by household
# Household member in productive age indicator (age between 15 and 64)
df <- df %>%
  mutate(
    aux = if (ageGrpMethod %in% c(1, 2)) {
      if_else(age >= 15 & age <= 64, 1, 0)
    } else if (ageGrpMethod == 3) {
      if_else(age >= 4 & age <= 13, 1, 0)
    } else {
      stop("Error: invalid ageGrpMethod selected, please choose 1, 2 or 3.")
    }
  )

# Aggregate employment across households
agg_empl <- df %>%
  group_by(date_q) %>%
  summarise(
    empl_sumforaggempl = sum(empl * wgthh, na.rm = TRUE), # Sum of weighted employment
    aux_sumforaggempl = sum(aux * wgthh, na.rm = TRUE), # Sum of weighted household members
    agg_empl = empl_sumforaggempl / aux_sumforaggempl # Aggregate employment rate
  ) %>%
  ungroup()
#View(agg_empl)

# Save aggregated employment data
write.csv(agg_empl, file.path(TABLES, "LFS_agg_empl.csv"), row.names = FALSE)

# Merge aggregated employment data with LFS data
df <- df %>%
  left_join(agg_empl, by = c("date_q")) %>%  # Merge on date_q
  filter(!is.na(agg_empl))  # Drop rows without matching aggregated data

################################################################################
# HtM imputation
# Set up columns for variables
if (imputationModelChoice == 1) {
  colnames <- c("age", "age2", "tert_edu", "married", "female", "selfempl", "perm", "hwusual",
                "job0", "job1", "job2", "job3", "job4", "job5", "job6", "job8", "job9",
                "sec1", "sec2", "sec4", "sec5", "sec6", "sec7", "sec8", "sec9", "sec10",
                "sec11", "sec12", "sec13", "sec14", "sec15", 
                "sec16", "sec17", "sec18", "sec19", "sec20", "sec21", "intercept")
} else if (imputationModelChoice == 2) {
  colnames <- c("age", "age2", "tert_edu", "married", "female", "selfempl", "perm", "hwusual",
                "job0", "job1", "job2", "job3", "job4", "job5", "job6", "job8", "job9",
                "sec1", "sec2", "sec4", "sec5", "sec6", "sec7", "sec8", "sec9", "sec10",
                "sec11", "intercept")
} else if (imputationModelChoice == 3) {
  colnames <- c("age", "age2", "tert_edu", "married", "female", "intercept")
} else {
  stop("Error: invalid imputationModelChoice selected, please choose 1, 2 or 3.")
}

df <- df %>% mutate(intercept = 1)
df <- df %>% mutate(country = "CZ")

# Imputation of HtM status
beta_htm <- read.csv(file.path(HFCSOUTPUT, paste0("beta_", HFCSDBNAME, "_prob_htm_", estMethod, "_", imputationModelChoice, ifelse(imputationWeighted, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")))
df <- df %>% left_join(beta_htm, by = "country")

# Calculate probability of HtM
df <- df %>% mutate(p_htm = 0)
for (n in colnames) {
  df <- df %>% mutate(!!sym("aux") := !!sym(n) * !!sym(paste0("b_", n)))
  df <- df %>% mutate(p_htm = if_else(is.na(aux), p_htm, p_htm + aux))
}

if (estMethod == "probit") {
  df <- df %>% mutate(p_htm = pnorm(p_htm))
} else {
  df <- df %>% mutate(p_htm = plogis(p_htm))
}

df <- df %>% select(-aux, -starts_with("b_"))

################################################################################
# Poor HtM conditional on HtM imputation
beta_phtm <- read.csv(file.path(HFCSOUTPUT, paste0("beta_", HFCSDBNAME, "_prob_phtm_", estMethod, "_", imputationModelChoice, ifelse(imputationWeighted, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")))
df <- df %>% left_join(beta_phtm, by = "country")

df <- df %>% mutate(p_phtm = 0)
for (n in colnames) {
  df <- df %>% mutate(!!sym("aux") := !!sym(n) * !!sym(paste0("b_", n)))
  df <- df %>% mutate(p_phtm = if_else(is.na(aux), p_phtm, p_phtm + aux))
}
if (estMethod == "probit") {
  df <- df %>% mutate(p_phtm = pnorm(p_phtm))
} else {
  df <- df %>% mutate(p_phtm = plogis(p_phtm))
}
df <- df %>% select(-aux, -starts_with("b_"))

################################################################################
# Imputation of probability of being Top 10% Non-HtM (conditional on being Non-HtM)
if (top10Index == 1) {
  beta_top10 <- read.csv(file.path(HFCSOUTPUT, paste0("beta_", HFCSDBNAME, "_prob_top10_", estMethod, "_", imputationModelChoice, ifelse(imputationWeighted, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")))
  df <- df %>% left_join(beta_top10, by = "country")
  
  df <- df %>% mutate(p_top10 = 0)
  for (n in colnames) {
    df <- df %>% mutate(!!sym("aux") := !!sym(n) * !!sym(paste0("b_", n)))
    df <- df %>% mutate(p_top10 = if_else(is.na(aux), p_top10, p_top10 + aux))
  }
  if (estMethod == "probit") {
    df <- df %>% mutate(p_top10 = pnorm(p_top10))
  } else {
    df <- df %>% mutate(p_top10 = plogis(p_top10))
  }
  df <- df %>% select(-aux, -starts_with("b_"))
}

# Drop rows with missing probabilities
df <- df %>%
  filter(!is.na(p_htm) & !is.na(p_phtm) & ifelse(top10Index == 1, !is.na(p_top10), TRUE))

################################################################################
# Thresholds
s_phtm <- read.csv(file.path(HFCSOUTPUT, paste0(HFCSDBNAME,"_pHtM_wHtM_shares.csv")))
s_htm <- read.csv(file.path(HFCSOUTPUT, paste0(HFCSDBNAME,"_HtM_nHtM_shares.csv")))
if (top10Index == 1) {
  s_top10 <- read.csv(file.path(HFCSOUTPUT, paste0(HFCSDBNAME,"_nHtM_top10_shares.csv")))
}

cutoff_pHtM <- s_phtm[1,"wHtM"]
cutoff_HtM <- s_htm[1,"nHtM"]
if (top10Index == 1) {
  cutoff_top10 <- s_top10[1, "nHtM"]
}

################################################################################
# Calibration of HtM Shares (Approach 1 or 2)
df <- df %>%
  group_split(date_q) %>%
  map_dfr(function(df_q) {
    if (verCalib == 1) {
      # Impute HtM status using Approach 1 (percentile-based)
      # Impute HtM status
      p_htm_cutoff <- as.numeric(wtd.quantile(x = df_q$p_htm, weights = df_q$wgthh, probs = cutoff_HtM / 100, na.rm = TRUE))
      df_q <- df_q %>%
        mutate(htm_g = if_else(p_htm <= p_htm_cutoff, 3, 99)) # 99 serves as a placeholder for future pHtM and wHtM group codes
      
      # Impute pHtM status within HtM
      p_phtm_cutoff <- as.numeric(wtd.quantile(x = df_q$p_phtm[df_q$htm_g != 3], weights = df_q$wgthh[df_q$htm_g != 3], probs = cutoff_pHtM / 100, na.rm = TRUE))
      df_q <- df_q %>%
        mutate(htm_g = if_else(htm_g != 3 & p_phtm > p_phtm_cutoff, 1, htm_g)) %>%  # Poor-HtM
        mutate(htm_g = if_else(htm_g != 3 & p_phtm <= p_phtm_cutoff, 2, htm_g))  # Wealthy-HtM
      
      # Impute Top 10 status within nHtM
      if (top10Index == 1) {
        p_top10_cutoff <- as.numeric(wtd.quantile(x = df_q$p_top10[df_q$htm_g == 3], weights = df_q$wgthh[df_q$htm_g == 3], probs = cutoff_top10 / 100, na.rm = TRUE))
        df_q <- df_q %>%
          mutate(htm_g = if_else(htm_g == 3 & p_top10 > p_top10_cutoff, 4, htm_g))  # Top10nHtM
      }
    } else {
      # Calibration using Approach 2 (cumulative distribution approach)
      # Cumulative HtM status
      df_q <- df_q %>%
        arrange(p_htm) %>%
        mutate(
          total_p_htm = sum(wgthh, na.rm = TRUE),
          cum_p_htm = cumsum(wgthh) / total_p_htm * 100,
          htm_g = if_else(cum_p_htm <= as.numeric(cutoff_HtM), 3, 99)
        )
      
      # Cumulative pHtM status
      df_q <- df_q %>% mutate(row_id = row_number()) # prep for use of temp filtering
      df_temp <- df_q %>% filter(htm_g != 3)
      df_temp <- df_temp %>%
        arrange(p_phtm) %>%
        mutate(
          total_p_phtm = sum(wgthh, na.rm = TRUE),
          cum_p_phtm = cumsum(wgthh) / total_p_phtm * 100,
          htm_g = if_else(cum_p_phtm > as.numeric(cutoff_pHtM), 1, htm_g),  # Poor-HtM
          htm_g = if_else(cum_p_phtm <= as.numeric(cutoff_pHtM), 2, htm_g)  # Wealthy-HtM
        ) %>%
        select(-total_p_phtm, -cum_p_phtm)
      df_q <- df_q %>%
        rows_update(df_temp, by = "row_id") #%>%
      #select(-row_id) 
      
      # Cumulative Top10nHtM status
      if (top10Index == 1) {
        #df_q <- df_q %>% mutate(row_id = row_number()) # prep for use of temp filtering
        df_temp <- df_q %>% filter(htm_g == 3)
        df_temp <- df_temp %>%
          arrange(p_top10) %>%
          mutate(
            total_p_top10 = sum(wgthh, na.rm = TRUE),
            cum_p_top10 = cumsum(wgthh) / total_p_top10 * 100,
            htm_g = if_else(cum_p_top10 > as.numeric(cutoff_top10), 4, htm_g)  # Top10nHtM
          ) %>%
          select(-total_p_top10, -cum_p_top10)
        df_q <- df_q %>%
          rows_update(df_temp, by = "row_id") %>%
          select(-row_id)
      }
      
      # Drop intermediate columns
      df_q <- df_q %>% select(-starts_with("total_p"), -starts_with("cum_p"))
    }
    return(df_q)
  })

################################################################################
# Display summary of shares, total and by date_q
group_shares <- df %>%
  group_by(date_q, htm_g) %>%
  summarise(weighted_count = sum(wgthh, na.rm = TRUE), .groups = "drop")

totals_per_q <- df %>%
  group_by(date_q) %>%
  summarise(
    n_obs = n(),
    n_obs_weighted = sum(wgthh, na.rm = TRUE),
    .groups = "drop"
  )

htm_g_weighted_freq <- group_shares %>%
  left_join(totals_per_q, by = "date_q") %>%
  mutate(perc = (weighted_count / n_obs_weighted) * 100)

htm_g_wide <- htm_g_weighted_freq %>%
  select(date_q, htm_g, perc, n_obs, n_obs_weighted) %>%
  pivot_wider(
    names_from = htm_g,
    names_prefix = "htm_",
    values_from = perc,
    values_fill = 0
  ) %>%
  distinct() %>%
  arrange(date_q)

total_counts <- df %>%
  group_by(htm_g) %>%
  summarise(weighted_count = sum(wgthh, na.rm = TRUE), .groups = "drop") %>%
  mutate(perc = (weighted_count / sum(weighted_count, na.rm = TRUE)) * 100)

total_row <- total_counts %>%
  select(htm_g, perc) %>%
  pivot_wider(
    names_from = htm_g,
    names_prefix = "htm_",
    values_from = perc,
    values_fill = 0
  ) %>%
  mutate(
    date_q = "total",
    n_obs = nrow(df),
    n_obs_weighted = sum(df$wgthh, na.rm = TRUE)
  ) %>%
  select(date_q, everything())


total_row$date_q <- as.character(total_row$date_q)
htm_g_wide$date_q <- as.character(htm_g_wide$date_q)

htm_g_weighted_freq <- bind_rows(total_row, htm_g_wide)
#View(htm_g_weighted_freq)
#write.csv(htm_g_weighted_freq, file.path(TABLES, "LFS_data_htm_g_weighted_freq_", imputationModelChoice, ".csv"), row.names = FALSE) 

################################################################################
# Means of variables by group
summary_by_group <- function(var) {
  df %>%
    group_by(htm_g) %>%
    summarise(
      #min = min(.data[[var]], na.rm = TRUE),
      #max = max(.data[[var]], na.rm = TRUE),
      mean = round(mean(.data[[var]], na.rm = TRUE),2),
      #median = median(.data[[var]], na.rm = TRUE)
    ) %>%
    mutate(variable = var)
}
summary_list <- lapply(colnames, summary_by_group)
summary_df <- bind_rows(summary_list)
summary_wide <- summary_df %>%
  pivot_wider(
    names_from = htm_g,
    values_from = c(mean),#(min, max, mean, median),
    names_sep = "_"
  )
#View(summary_wide)
#write.csv(summary_wide, file.path(TABLES, "LFS_data_variablessummary_", imputationModelChoice, ".csv"), row.names = FALSE) 

################################################################################
# Save the dataset
write.csv(df, file.path(OUTPUT, paste0("LFS_data_wHtMstat_", imputationModelChoice, ".csv")), row.names = FALSE) 