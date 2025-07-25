################################################################################
# APPEND DATASETS 
# desc: on EU-LFS data, merges quarterly  files and creates one source dataset
################################################################################

# Load packages
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(gt)

################################################################################
# Setup path and  define columns of interest
path <- LFSDATA

inputcols <- c("YEAR", "QUARTER", "HWUSUAL", "HATLEV1D", "HATLEVEL", "AGE", "AGE_GRP", "STAPRO", 
               "NACE2_1D", "SEX", "WKSTAT", "EMPSTAT", "TEMP", "ISCO08_1D", "COEFFQ", "HHNUM")

file_list <- list.files(path = path, pattern = "*.csv", full.names = TRUE)

################################################################################
# Load and merge datasets
df <- file_list %>%
  map_dfr(~ read_csv(.x, 
                     col_types = cols(.default = "c"), # force character type
                     show_col_types = FALSE) %>%
            select(any_of(inputcols)) %>%
            mutate(source_file = basename(.x)))

View(df)
dim(df)

################################################################################
# Analyse share of unique values
analyscols <- c("YEAR", "QUARTER", "HATLEV1D", "HATLEVEL", "AGE", "AGE_GRP", "STAPRO", 
                "NACE2_1D", "SEX", "WKSTAT", "EMPSTAT", "TEMP", "ISCO08_1D")

for (var in analyscols) {
  cat("\nAnalyzing variable:", var, "\n\n")
  
  summary_df <- df %>%
    group_by(source_file, .data[[var]]) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(source_file) %>%
    mutate(share = count / sum(count)) %>%
    select(-count) %>%
    pivot_wider(names_from = all_of(var), values_from = share, values_fill = 0) %>%
    arrange(source_file) %>%
    ungroup()  # explicitly ungroup here
  
  gt_tbl <- summary_df %>%
    gt(rowname_col = "source_file") %>%
    fmt_percent(columns = everything(), decimals = 1) %>%
    data_color(
      columns = everything(),
      colors = scales::col_numeric(
        palette = c("white", "lightblue", "blue"),
        domain = c(0, max(summary_df %>% select(-source_file), na.rm = TRUE))
      )
    )
  
  print(gt_tbl)
  
  cat("\nPress [ENTER] to continue to the next variable...\n")
  readline()
}

################################################################################
# Save the dataset
write.csv(df, file.path(OUTPUT, "LFS_data_appended.csv"), row.names = FALSE)

################################################################################
# Sample data
#df <- data.frame(
#  #YEAR = c(rep(1998, 4), rep(1999, 4), 2000, 2000),
#  YEAR = c(rep(1998,10)),
#  #QUARTER = c("Q1", "Q2", "Q3", "Q4", "Q1", "Q2", "Q3", "Q4", "Q1", "Q2"),
#  QUARTER = c(rep("Q1",10)),
#  HWUSUAL = c(45, 99, 60, 60, 60, 99, 45, 60, 60, 99),
#  HATLEV1D = c("M", "L", "H", "M", "M", "H", "L", "M", "M", "H"),
#  HATLEVEL = rep(NA, 10),
#  AGE = rep(NA, 10),
#  AGE_GRP = c("Y40-44", "Y15-19", "Y45-49", "Y20-24", "Y20-24", "Y65-69", "Y15-19", "Y40-44", "Y45-49", "Y65-69"),
#  STAPRO = c(3, 9, 0, 3, 3, 9, 9, 3, 0, 9),
#  NACE2_1D = c(NA, "9", "A", NA, "U", "9", "9", NA, "B", "9"),
#  SEX = c(2, 2, 1, 1, 1, 1, 2, 1, 1, 1),
#  WKSTAT = c(1, 4, 1, 1, 1, 4, 4, 1, 1, 4),
#  EMPSTAT = c(1, 2, 1, 1, 1, 2, 2, 1, 1, 2),
#  TEMP = c(1, 9, 9, 1, 1, 9, 9, 1, 9, 9),
#  ISCO08_1D = c(NA, "99", "00", NA, "10", "99", "99", NA, "90", "99"),
#  COEFFQ = c(0.19419, 0.20251, 0.21731, 0.18569, 0.18569, 0.18546, 0.20251, 0.19419, 0.21731, 0.18546),
#  HHNUM = c(11111, 11111, 22222, 22222, 22222, 3333, 44444, 44444, 55555, 6666),
#  #source_file = paste0("CZ", c(rep(1998, 4), rep(1999, 4), 2000, 2000), c("Q1", "Q2", "Q3", "Q4", "Q1", "Q2", "Q3", "Q4", "Q1", "Q2"), ".csv"),
#  source_file = paste0("CZ", c(rep(1998, 10)), c(rep("Q1",10)), ".csv"),
#  stringsAsFactors = FALSE
#)
#df <- df %>% mutate(across(everything(), as.character))

# View the data
#print(df)
