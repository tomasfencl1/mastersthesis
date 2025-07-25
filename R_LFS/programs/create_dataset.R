################################################################################
# CREATE DATASET
# desc: on EU-LFS data, creates variables and runs basic analysis
################################################################################

# Load packages
library(dplyr)
library(readr)
library(zoo)

################################################################################
# Load dataset
df1 <- read.csv(file.path(OUTPUT, "LFS_data_appended.csv"))
df <- df1

################################################################################
# Lowercase column names
df <- df %>%
  rename(
    year = YEAR,
    quarter = QUARTER,
    hhnum = HHNUM,
    wgtos = COEFFQ,
  )

################################################################################
# Sort and generate quarterly date
df <- df %>%
  mutate(
    quarter_num = as.numeric(sub("Q", "", quarter)),  # "Q1" → 1
    date_q = as.yearqtr(paste(year, quarter_num), format = "%Y %q")
  ) %>%
  arrange(year, quarter_num, hhnum)
  
# Cleanup
df <- df %>% select(-quarter_num, -source_file)

################################################################################
# Delete duplicit persnum / rows in given date_q
#df <- df %>%
#  group_by(date_q, persnum) %>%
#  filter(row_number() == 1) %>%  # Keep only the first occurrence of duplicate persnum within each date_q
#  ungroup()

################################################################################
# Age from age groups, either midpoint or ordered index method
age_levels <- c(
  "Y0-4", "Y5-9", "Y10-14", "Y15-19", "Y20-24", "Y25-29", "Y30-34", "Y35-39",
  "Y40-44", "Y45-49", "Y50-54", "Y55-59", "Y60-64", "Y65-69", "Y70-74", "Y75-79",
  "Y80-84", "Y85-89", "Y90-94", "Y95-99", "Y_GE100"
)
#age_levels <- unique(df$AGE_GRP)

df <- df %>%
  mutate(
    age = if (ageGrpMethod == 1) {
      AGE
    } else if (ageGrpMethod == 2) {
      case_when(
        AGE_GRP == "Y_GE100" ~ 102,
        AGE_GRP %in% age_levels ~ {
          parts <- strsplit(sub("Y", "", AGE_GRP), "-")
          sapply(parts, function(x) mean(as.numeric(x)))
          },
        TRUE ~ NA_real_
        )
    } else if (ageGrpMethod == 3) {
      match(AGE_GRP, age_levels)
    } else {
      stop("Error: invalid ageGrpMethod selected, please choose 1, 2 or 3.")
    }
  )
df <- df %>% select(-AGE)

################################################################################
# Variables age2, hwusual, female, empl, selfempl, employee, perm
df <- df %>%
  mutate(
    age2 = age^2,
    hwusual = if_else(is.na(HWUSUAL) | HWUSUAL == 99, NA_real_, HWUSUAL), #hwusual = HWUSUAL, check NA coding (99)
    #married not available in LFS #married = if_else(??? == 2, 1, 0),
    female = if_else(SEX == 2, 1, 0), # no NA
    empl = if_else(is.na(EMPSTAT) | EMPSTAT == 9, NA_real_, if_else(EMPSTAT == 1, 1, 0)), #empl = if_else(EMPSTAT == 1, 1, 0),
    selfempl = if_else(is.na(STAPRO) | STAPRO == 9, NA_real_, if_else(STAPRO == 0, 1, 0)), #selfempl = if_else(STAPRO == 0, 1, 0),
    employee = if_else(is.na(STAPRO) | STAPRO == 9, NA_real_, if_else(STAPRO == 3, 1, 0)), #selfempl = if_else(STAPRO == 3, 1, 0),
    perm = if_else(is.na(TEMP) | TEMP == 9, NA_real_, if_else(TEMP == 2, 1, 0)) #perm = if_else(TEMP == 2, 1, 0)
  )
df <- df %>% select(-WKSTAT)

################################################################################
# Education levels
df <- df %>%
  mutate(
    prim_edu = if_else(is.na(HATLEV1D) | HATLEV1D == 9, NA_real_, if_else(HATLEV1D == "L", 1, 0)), #prim_edu = if_else(HATLEV1D == "L", 1, 0),
    sec_edu = if_else(is.na(HATLEV1D) | HATLEV1D == 9, NA_real_, if_else(HATLEV1D == "M", 1, 0)), #sec_edu = if_else(HATLEV1D == "M", 1, 0),
    tert_edu = if_else(is.na(HATLEV1D) | HATLEV1D == 9, NA_real_, if_else(HATLEV1D == "H", 1, 0)) #tert_edu = if_else(HATLEV1D == "H", 1, 0)
  )

df <- df %>%
  mutate(
    HATLEV1D_num = case_when(
      is.na(HATLEV1D) | HATLEV1D == "9" ~ NA_real_,
      HATLEV1D == "L" ~ 1,
      HATLEV1D == "M" ~ 2,
      HATLEV1D == "H" ~ 3,
      TRUE ~ NA_real_  # catch any unexpected values
    )
  )

df <- df %>% select(-HATLEVEL)

################################################################################
# Job dummy from ISCO08
df <- df %>%
  mutate(
    ISCO08_1D_aux = if_else(is.na(ISCO08_1D) | ISCO08_1D == 99, NA_real_, ISCO08_1D)
  )

valid_codes <- seq(0, 90, by = 10)
for (code in valid_codes) {
  dummy_name <- paste0("job", code / 10)
  df[[dummy_name]] <- case_when(
    is.na(df$ISCO08_1D_aux) ~ NA_real_,
    df$ISCO08_1D_aux == code ~ 1,
    TRUE ~ 0
  )
}

# CHECK
df_job <- df %>%
  filter(ISCO08_1D_aux %in% valid_codes) %>%
  mutate(jobgroup = ISCO08_1D_aux)
job_counts <- table(factor(df_job$jobgroup, levels = valid_codes))
print(job_counts)
barplot(job_counts, main = "Distribution of ISCO (ISCO08_1D_aux)",
        col = "salmon", las = 2, xlab = "ISCO08_1D_aux", ylab = "Count")

df <- df %>% select(-ISCO08_1D_aux)

################################################################################
# Sector dummy from NACE2
df <- df %>%
  mutate(
    NACE2_1D_aux = if_else(is.na(NACE2_1D) | NACE2_1D == 9, NA_character_, NACE2_1D)
  )

nace2_sectors <- LETTERS[1:21]  # A to U
for (i in seq_along(nace2_sectors)) {
  sector_letter <- nace2_sectors[i]
  dummy_name <- paste0("sec", i)
  
  df[[dummy_name]] <- case_when(
    is.na(df$NACE2_1D_aux) ~ NA_real_,
    df$NACE2_1D_aux == sector_letter ~ 1,
    TRUE ~ 0
  )
}

# CHECK
df_sec <- df %>%
  filter(NACE2_1D_aux %in% nace2_sectors) %>%
  mutate(secgroup = NACE2_1D_aux)
sec_counts <- table(factor(df_sec$secgroup, levels = nace2_sectors))
print(sec_counts)
barplot(sec_counts, main = "Distribution of NACE (NACE2_1D_aux)",
        col = "lightblue", las = 2, xlab = "NACE2_1D_aux", ylab = "Count")

df <- df %>% select(-NACE2_1D_aux)

################################################################################
# Arrange final dataset
df <- df %>%
  arrange(date_q, hhnum)

# Print total number of observations
cat("Total number of observations:", nrow(df), "\n")

################################################################################
# Save the dataset
write.csv(df, file.path(OUTPUT, "LFS_data.csv"), row.names = FALSE)