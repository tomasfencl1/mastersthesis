################################################################################
# CSU HtM IMPUTATION
# desc: combines CSU & HFCS, imputes the probabilities of being p/w/n-HtM (or top10) to CSU data
################################################################################

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(Hmisc)
library(broom)
library(purrr)
library(ggplot2)

################################################################################
# Load dataset
df1 <- read.csv(file.path(OUTPUT, "CSU_data_wRP2.csv"))
df <- df1

################################################################################
# Keep only reference person
df <- df %>% 
  filter(ref == 1)

################################################################################
if (imputationModelChoice == 2) {
  # Re-define sec dummies
  # Delete current
  df <- df[, !(grepl("^(sec)", names(df)) & names(df) != "sec_edu")]

  #df_sec <- df %>% filter(!is.na(zamnace))
  #sec_counts <- table(df_sec$zamnace)  
  #print(sec_counts)
  #barplot(sec_counts, main = "Distribution of Detailed NACE2 (zamnace)",
  #        col = "lightblue", las = 2, xlab = "NACE2", ylab = "Count")

  # Own NACE2 grouping (Slacalek i. transforms NACE2 to NACE, ii. groups some codes, e.g. A,B; C,D,E; O,P,Q)
  df <- df %>%
    mutate(xsecgroup = case_when(
      zamnace %in% c("A") ~ "A", # Agriculture, forestry, fishing
      zamnace %in% c("B", "D", "E") ~ "B,D,E", # Mining and quarrying and other industry
      zamnace %in% c("C") ~ "C", # Manufacturing
      zamnace %in% c("F") ~ "F", # Construction
      zamnace %in% c("G", "H", "I") ~ "G,H,I", # Wholesale and retail trade, transportation and storage, accommodation and food service activities
      zamnace %in% c("J") ~ "J", # Information and communication
      zamnace %in% c("K") ~ "K", # Financial and insurance activities
      zamnace %in% c("L")~ "L", # Real estate activities
      zamnace %in% c("M", "N")~ "M,N", # Professional, scientific, technical, administration and support service activities
      zamnace %in% c("O", "P", "Q")~ "O,P,Q", # Public administration, defence, education, human health and social work activities
      zamnace %in% c("R", "S", "T", "U") ~ "R,S,T,U", # Other services
      TRUE ~ NA_character_
    ))
  sectoral_categories <- sort(unique(df$xsecgroup[!is.na(df$xsecgroup)]))
  for (sec in sectoral_categories) {
    dummy_name <- paste0("sec", which(sectoral_categories == sec))
    df[[dummy_name]] <- ifelse(df$xsecgroup == sec, 1, ifelse(is.na(df$xsecgroup), NA, 0))
  }
  #df_secgroup <- df %>% filter(!is.na(xsecgroup))
  #secgroup_counts <- table(df_secgroup$xsecgroup, useNA = "ifany")
  #print(secgroup_counts)
  #barplot(secgroup_counts, main = "Distribution of Grouped NACE2",
  #        col = "lightblue", las = 2, xlab = "NACE2 Group", ylab = "Count")
} else if (imputationModelChoice %in% c(1, 3)) {
  {}
} else {
  stop("Error: invalid imputationModelChoice selected, please choose 1, 2 or 3.")
}

################################################################################
# Check NA proportions
var_list <- c("age", "age2", "prim_edu", "sec_edu", "tert_edu",
              "married", "female", "wgtos", "wgthh", "empl",
              "selfempl", "perm", "hwusual", 
              "job0", "job1", "job2", "job3", "job4", "job5", "job6", "job7", "job8", "job9",
              "sec1", "sec2", "sec3", "sec4", "sec5", "sec6", "sec7", "sec8", "sec9", 
              "sec10", "sec11")#, "sec12", "sec13", "sec14", "sec15")
              #,"sec16", "sec17", "sec18", "sec19", "sec20", "sec21")
df_summary <- df %>%
  group_by(date_q) %>%
  summarise(
    n = n(),  # Count of observations in each date_q
    across(all_of(var_list), ~ mean(is.na(.)) * 100, .names = "{.col}_NA_pct")
  ) %>%
  ungroup()
#View(df_summary)

################################################################################
# NA handling
if (imputationModelChoice %in% c(1, 2)) {
  # Delete observations for years where all variables are missing (ISCO starts 2011, NACE2 not included in 2014)
  df <- df %>% 
    filter(!year %in% c(2008, 2009, 2010, 2014))
}

# Characteristics not related to employment plus employment status
cols_not_employment <- c("age", "age2", "prim_edu", "sec_edu", "tert_edu", 
                         "married", "female", "wgtos", "wgthh", "empl", "date_q")

# Characteristics related to employment
if (imputationModelChoice == 1) {
  cols_employment <- c("selfempl", "perm", "hwusual", 
                       "job0", "job1", "job2", "job3", "job4", "job5", "job6", "job7", "job8", "job9",
                       "sec1", "sec2", "sec3", "sec4", "sec5", "sec6", "sec7", "sec8", "sec9", "sec10",
                       "sec11", "sec12", "sec13", "sec14", 
                       "sec15", "sec16", "sec17", "sec18", "sec19", "sec20", "sec21")
} else if (imputationModelChoice == 2) {
  cols_employment <- c("selfempl", "perm", "hwusual", 
                       "job0", "job1", "job2", "job3", "job4", "job5", "job6", "job7", "job8", "job9",
                       "sec1", "sec2", "sec3", "sec4", "sec5", "sec6", "sec7", "sec8", "sec9", "sec10",
                       "sec11")
} else if (imputationModelChoice == 3) {
  cols_employment <- NULL
} else {
  stop("Error: invalid imputationModelChoice selected, please choose 1, 2 or 3.")
}

# Drop rows with NAs in non-employment variables and in employment status
df_nafree <- df[complete.cases(df[, cols_not_employment]), ]

if (imputationModelChoice %in% c(1, 2)) {
  
  # For rows where employment is 0, replace NAs in employment-related columns with 0
  unemployed_rows <- which(!is.na(df_nafree$empl) & df_nafree$empl == 0)
  df_nafree[unemployed_rows, cols_employment] <- lapply(df_nafree[unemployed_rows, cols_employment], function(x) {
    x[is.na(x)] <- 0
    x
  })
  
  # For rows where employment is 1, drop rows that have any NA in employment-related columns
  employed_rows <- which(!is.na(df_nafree$empl) & df_nafree$empl == 1)
  
  # For rows where employment is 1...
  # ...drop rows that have any NA in employment-related columns (excluding 'perm')
  cols_emp_without_perm <- setdiff(cols_employment, "perm")
  employed_no_na <- employed_rows[complete.cases(df_nafree[employed_rows, cols_emp_without_perm])]
  
  rows_to_keep <- sort(c(unemployed_rows, employed_no_na))
  df_nafree <- df_nafree[rows_to_keep, ]
  
  employed_rows_updated <- which(df_nafree$empl == 1)
  
  # ...handle perm separately, drop rows where zampost == 1 and perm is NA, set remaining perm NA to 0
  drop_condition <- which(is.na(df_nafree$perm[employed_rows_updated]) & df_nafree$zampost[employed_rows_updated] == 1)
  
  if (length(drop_condition) > 0) {
    rows_to_drop_absolute <- employed_rows_updated[drop_condition]
    df_nafree <- df_nafree[-rows_to_drop_absolute, ]
  }
  
  employed_rows_updated <- which(df_nafree$empl == 1)
  
  fill_condition <- which(is.na(df_nafree$perm[employed_rows_updated]) & df_nafree$zampost[employed_rows_updated] != 1)
  
  if (length(fill_condition) > 0) {
    rows_to_fill_absolute <- employed_rows_updated[fill_condition]
    df_nafree$perm[rows_to_fill_absolute] <- 0
  }
  
} else if (imputationModelChoice == 3) {
  {}
} else {
  stop("Error: invalid imputationModelChoice selected, please choose 1, 2 or 3.")
}

# Re-normalize weights
# Compute the original total weights by date_q from df (before NA omission)
original_wgtos <- df %>%
  group_by(date_q) %>%
  summarise(orig_sum = sum(wgtos, na.rm = TRUE))

# Join totals into the nafree dataset and re-normalize
df_nafree <- df_nafree %>%
  left_join(original_wgtos, by = "date_q") %>%
  group_by(date_q) %>%
  mutate(current_sum = sum(wgtos, na.rm = TRUE),
         wgtos = wgtos * (orig_sum / current_sum)) %>%
  ungroup() %>%
  select(-orig_sum, -current_sum)

# Analogously for wgthh
original_wgthh <- df %>%
  group_by(date_q) %>%
  summarise(orig_sum = sum(wgthh, na.rm = TRUE))
df_nafree <- df_nafree %>%
  left_join(original_wgthh, by = "date_q") %>%
  group_by(date_q) %>%
  mutate(current_sum = sum(wgthh, na.rm = TRUE),
         wgthh = wgthh * (orig_sum / current_sum)) %>%
  ungroup() %>%
  select(-orig_sum, -current_sum)

################################################################################
# Compare means and distributions
weighted_sd <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    idx <- !is.na(x)
    x <- x[idx]
    w <- w[idx]
  }
  wm <- weighted.mean(x, w, na.rm = na.rm)
  sqrt(sum(w * (x - wm)^2, na.rm = na.rm) / sum(w, na.rm = na.rm))
}

all_vars <- unique(c(cols_not_employment, cols_employment))

comparison_results <- data.frame(
  variable       = character(),
  df_weighted_mean       = numeric(),
  df_nafree_weighted_mean = numeric(),
  mean_diff_pct      = numeric(),
  df_weighted_sd = numeric(),
  df_nafree_weighted_sd = numeric(),
  stringsAsFactors = FALSE
)

for(v in all_vars) {
  d1 <- df[[v]]
  d2 <- df_nafree[[v]]
  
  if(is.numeric(d1) & is.numeric(d2)) {
    m1 <- weighted.mean(d1, df$wgtos, na.rm = TRUE)
    m2 <- weighted.mean(d2, df_nafree$wgtos, na.rm = TRUE)
    sd1 <- weighted_sd(d1, df$wgtos, na.rm = TRUE)
    sd2 <- weighted_sd(d2, df_nafree$wgtos, na.rm = TRUE)
    
    comparison_results <- rbind(comparison_results,
                                data.frame(variable = v,
                                           df_weighted_mean = m1,
                                           df_nafree_weighted_mean = m2,
                                           mean_diff_pct = (m2 - m1)/m1*100,
                                           df_weighted_sd = sd1,
                                           df_nafree_weighted_sd = sd2,
                                           stringsAsFactors = FALSE))
  } else {
    comparison_results <- rbind(comparison_results,
                                data.frame(variable = v,
                                           df_weighted_mean = NA,
                                           df_nafree_weighted_mean = NA,
                                           mean_diff_pct = NA,
                                           df_weighted_sd = NA,
                                           df_nafree_weighted_sd = NA,
                                           stringsAsFactors = FALSE))
  }
}

comparison_results <- comparison_results %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
#print(comparison_results)

# Make dataframe without NAs the main dataframe
df <- df_nafree

################################################################################
# Generate employment rate by household
# Household member in productive age indicator (age between 15 and 64)
df <- df %>%
  mutate(aux = if_else(age >= 15 & age <= 64, 1, 0))

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
write.csv(agg_empl, file.path(TABLES, "CSU_agg_empl.csv"), row.names = FALSE)

# Merge aggregated employment data with CSU data
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
beta_htm <- read.csv(file.path(HFCSOUTPUT, paste0("beta_", HFCSDBNAME, "_prob_htm_", estMethod, "_", imputationModelChoice, ifelse(imputationWeighted, "wt", "uwt"), ".csv")))
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
beta_phtm <- read.csv(file.path(HFCSOUTPUT, paste0("beta_", HFCSDBNAME, "_prob_phtm_", estMethod, "_", imputationModelChoice, ifelse(imputationWeighted, "wt", "uwt"), ".csv")))
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
  beta_top10 <- read.csv(file.path(HFCSOUTPUT, paste0("beta_", HFCSDBNAME, "_prob_top10_", estMethod, "_", imputationModelChoice, ifelse(imputationWeighted, "wt", "uwt"), ".csv")))
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
# OLD VERSION CALIBRATING OVER WHOLE DATASET NOT WITHIN date_q
# Calibration of HtM Shares (Approach 1 or 2)
#if (verCalib == 1) {
#  # Impute HtM status using Approach 1 (percentile-based)
#  # HtM status
#  p_htm_cutoff <- as.numeric(wtd.quantile(x = df$p_htm,weights = df$wgthh,probs = cutoff_HtM / 100, na.rm = TRUE))
#  df <- df %>%
#    mutate(htm_g = if_else(p_htm <= p_htm_cutoff, 3, 99)) # 99 serves as a placeholder for future pHtM and wHtM group codes
#  
#  # Impute pHtM status within HtM
#  p_phtm_cutoff <- as.numeric(wtd.quantile(x = df$p_phtm[df$htm_g != 3],weights = df$wgthh[df$htm_g != 3],probs = cutoff_pHtM / 100,na.rm = TRUE))
#  df <- df %>%
#    mutate(htm_g = if_else(htm_g != 3 & p_phtm > p_phtm_cutoff, 1, htm_g)) %>%  # Poor-HtM
#    mutate(htm_g = if_else(htm_g != 3 & p_phtm <= p_phtm_cutoff, 2, htm_g))  # Wealthy-HtM
#  
#  # Impute Top 15% nHtM
#  if (top10Index == 1) {
#    p_top10_cutoff <- as.numeric(wtd.quantile(x = df$p_top10[df$htm_g == 3],weights = df$wgthh[df$htm_g == 3],probs = cutoff_top10 / 100,na.rm = TRUE))
#    df <- df %>%
#      mutate(htm_g = if_else(htm_g == 3 & p_top10 > p_top10_cutoff, 4, htm_g))  # Top10nHtM
#  }
#} else {
#  # Calibration using Approach 2 (cumulative distribution approach)
#  # Cumulative HtM status
#  df <- df %>%
#    arrange(p_htm) %>%
#    mutate(
#      total_p_htm = sum(wgthh, na.rm = TRUE),
#      cum_p_htm = cumsum(wgthh) / total_p_htm * 100,
#      htm_g = if_else(cum_p_htm <= as.numeric(cutoff_HtM), 3, 99)
#    )
#  
#  # Cumulative pHtM status
#  df <- df %>% mutate(row_id = row_number()) # prep for use of temp filtering
#  df_temp <- df %>% filter(htm_g != 3)
#  df_temp <- df_temp %>%
#    arrange(p_phtm) %>%
#    mutate(
#      total_p_phtm = sum(wgthh, na.rm = TRUE),
#      cum_p_phtm = cumsum(wgthh) / total_p_phtm * 100,
#      htm_g = if_else(cum_p_phtm > as.numeric(cutoff_pHtM), 1, htm_g),  # Poor-HtM
#      htm_g = if_else(cum_p_phtm <= as.numeric(cutoff_pHtM), 2, htm_g)  # Wealthy-HtM
#    ) %>%
#    select(-total_p_phtm, -cum_p_phtm)
#  df <- df %>%
#    rows_update(df_temp, by = "row_id") #%>%
#    #select(-row_id) 
#  
#  # Cumulative Top10nHtM status
#  if (top10Index == 1) {
#    #df <- df %>% mutate(row_id = row_number()) # prep for use of temp filtering
#    df_temp <- df %>% filter(htm_g == 3)
#    df_temp <- df_temp %>%
#      arrange(p_top10) %>%
#      mutate(
#        total_p_top10 = sum(wgthh, na.rm = TRUE),
#        cum_p_top10 = cumsum(wgthh) / total_p_top10 * 100,
#        htm_g = if_else(cum_p_top10 > as.numeric(cutoff_top10), 4, htm_g)  # Top10nHtM
#      ) %>%
#      select(-total_p_top10, -cum_p_top10)
#    df <- df %>%
#      rows_update(df_temp, by = "row_id") %>%
#      select(-row_id)
#  }
#  
#  # Drop intermediate columns
#  df <- df %>% select(-starts_with("total_p"), -starts_with("cum_p"))
#}

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

htm_g_weighted_freq <- bind_rows(total_row, htm_g_wide)
#View(htm_g_weighted_freq)

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

################################################################################
# Save the dataset
write_csv(df, file.path(OUTPUT, "CSU_data_wRP_wHtMstat.csv")) 
