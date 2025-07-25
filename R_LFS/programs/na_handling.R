################################################################################
# NA HANDLING
# desc: on EU-LFS data, prepares the dataset for imputation based on switches, handles NAs
################################################################################

# Load packages
library(dplyr)
library(tidyr)
library(readr)

################################################################################
# Load dataset
df1 <- read.csv(file.path(OUTPUT, "LFS_data_wRP.csv"))
df <- df1

################################################################################
# Keep only reference person
df <- df %>% 
  filter(ref == 1)

# Print total number of reference person
cat("Total number of reference person:", nrow(df), "\n")

################################################################################
if (imputationModelChoice == 2) {
  # Re-define sec dummies
  df <- df[, !(grepl("^(sec)", names(df)) & names(df) != "sec_edu")]
  
  df <- df %>%
    mutate(
      NACE2_1D_aux = if_else(is.na(NACE2_1D) | NACE2_1D == 9, NA_character_, NACE2_1D)
    )
  
  # NACE2 "high-level aggregation" grouping (Slacalek i. transforms NACE2 to NACE, ii. groups some codes, e.g. A,B; C,D,E; O,P,Q)
  df <- df %>%
    mutate(NACE2_1D_grouped = case_when(
      NACE2_1D_aux %in% c("A") ~ "A", # Agriculture, forestry, fishing
      NACE2_1D_aux %in% c("B", "D", "E") ~ "B,D,E", # Mining and quarrying and other industry
      NACE2_1D_aux %in% c("C") ~ "C", # Manufacturing
      NACE2_1D_aux %in% c("F") ~ "F", # Construction
      NACE2_1D_aux %in% c("G", "H", "I") ~ "G,H,I", # Wholesale and retail trade, transportation and storage, accommodation and food service activities
      NACE2_1D_aux %in% c("J") ~ "J", # Information and communication
      NACE2_1D_aux %in% c("K") ~ "K", # Financial and insurance activities
      NACE2_1D_aux %in% c("L")~ "L", # Real estate activities
      NACE2_1D_aux %in% c("M", "N")~ "M,N", # Professional, scientific, technical, administration and support service activities
      NACE2_1D_aux %in% c("O", "P", "Q")~ "O,P,Q", # Public administration, defence, education, human health and social work activities
      NACE2_1D_aux %in% c("R", "S", "T", "U") ~ "R,S,T,U", # Other services
      TRUE ~ NA_character_
    ))
  
  nace2_sectors_grouped <- c("A", "B,D,E", "C", "F", "G,H,I", "J", "K", "L", "M,N", "O,P,Q", "R,S,T,U")
  for (i in seq_along(nace2_sectors_grouped)) {
    sector_letter <- nace2_sectors_grouped[i]
    dummy_name <- paste0("sec", i)
    
    df[[dummy_name]] <- case_when(
      is.na(df$NACE2_1D_aux) ~ NA_real_,
      df$NACE2_1D_aux == sector_letter ~ 1,
      TRUE ~ 0
    )
  }
  
  # CHECK
  df_sec <- df %>%
    filter(NACE2_1D_grouped %in% nace2_sectors_grouped) %>%
    mutate(secgroup = NACE2_1D_grouped)
  sec_counts <- table(factor(df_sec$secgroup, levels = nace2_sectors_grouped))
  print(sec_counts)
  barplot(sec_counts, main = "Distribution of grouped NACE (NACE2_1D_grouped)",
          col = "lightblue", las = 2, xlab = "NACE2_1D_grouped", ylab = "Count")
  
  df <- df %>% select(-NACE2_1D_grouped)
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
    across(all_of(var_list), ~ mean(is.na(.)) * 100, .names = "{.col}_NAshare")
  ) %>%
  ungroup()
#View(df_summary)

################################################################################
# NA handling
if (imputationModelChoice %in% c(1, 2)) {
  # Delete observations for years where all variables are missing (NACE2 starts in 2008, ISCO08 in 2011)
  df <- df %>% filter(as.numeric(year) >= 2011)
}

# Characteristics not related to employment plus employment status
cols_not_employment <- c("age", "age2", "prim_edu", "sec_edu", "tert_edu", 
                         "married", "female", "wgtos", "wgthh", "empl", "date_q")

# Characteristics related to employment
if (imputationModelChoice == 1) {
  cols_employment <- c("selfempl", "employee", "perm", "hwusual", 
                       "job0", "job1", "job2", "job3", "job4", "job5", "job6", "job7", "job8", "job9",
                       "sec1", "sec2", "sec3", "sec4", "sec5", "sec6", "sec7", "sec8", "sec9", "sec10",
                       "sec11", "sec12", "sec13", "sec14", 
                       "sec15", "sec16", "sec17", "sec18", "sec19", "sec20", "sec21")
} else if (imputationModelChoice == 2) {
  cols_employment <- c("selfempl", "employee", "perm", "hwusual", 
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
  # ...drop rows that have any NA in employment-related columns (excluding 'perm', since selfemployed can not have permanent contract)
  cols_emp_without_perm <- setdiff(cols_employment, "perm")
  employed_no_na <- employed_rows[complete.cases(df_nafree[employed_rows, cols_emp_without_perm])]
  
  rows_to_keep <- sort(c(unemployed_rows, employed_no_na))
  df_nafree <- df_nafree[rows_to_keep, ]
  
  employed_rows_updated <- which(df_nafree$empl == 1)
  
  # ...handle perm separately, drop rows where employee == 1 and perm is NA, set remaining perm NA to 0 (only employee can be permanent)
  drop_condition <- which(is.na(df_nafree$perm[employed_rows_updated]) & df_nafree$employee[employed_rows_updated] == 1)
  
  if (length(drop_condition) > 0) {
    rows_to_drop_absolute <- employed_rows_updated[drop_condition]
    df_nafree <- df_nafree[-rows_to_drop_absolute, ]
  }
  
  employed_rows_updated <- which(df_nafree$empl == 1)
  
  fill_condition <- which(is.na(df_nafree$perm[employed_rows_updated]) & df_nafree$employee[employed_rows_updated] != 1)
  
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
# Save the dataset
write.csv(df, file.path(OUTPUT, paste0("LFS_data_noNA_", imputationModelChoice, ".csv")), row.names = FALSE) 