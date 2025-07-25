################################################################################
# REFERENCE PERSON
# desc: on EU-LFS data, uses respectively working hours, educ. and age to determine the reference person
################################################################################

# Load packages
library(dplyr)
library(tidyr)
library(readr)

################################################################################
# Load dataset
df1 <- read.csv(file.path(OUTPUT, "LFS_data.csv"))
df <- df1

################################################################################
# Create household weight
df <- df %>%
  group_by(date_q, hhnum) %>%
  mutate(wgthh = sum(wgtos, na.rm = TRUE)) %>%
  ungroup()

# Filter age between 15 and 64
#df <- df %>%
#  filter(
#    if (ageGrpMethod %in% c(1, 2)) {
#      age >= 15 & age <= 64
#    } else if (ageGrpMethod == 3) {
#      age >= 4 & age <= 13
#    } else {
#      stop("Error: invalid ageGrpMethod selected, please choose 1, 2 or 3.")
#    }
#  )

# Assign reference person by HWUSUAL
df <- df %>%
  group_by(date_q, hhnum) %>%
  mutate(
    ref_hours = max(hwusual, na.rm = TRUE), # calculate the maximum hwusual in hhnum
    ref = if_else(hwusual == ref_hours, 1, NA_real_), # reference person is individual with the most hwusual in household
    ref_hours_check = sum(ref, na.rm = TRUE), # check whether this condition is not met by multiple people
    ref = if_else(ref_hours_check > 1 & is.na(ref), 0, ref), # case when more individuals have same max hwusual, others in hhnum can not be ref anymore
    ref = if_else(ref_hours_check > 1 & ref == 1, NA_real_, ref), # case when more individuals have same max hwusual, these individuals are kept as NA
    ref_hours_check = sum(ref, na.rm = TRUE), # recalc check after conflict resolution
    ref = if_else(is.na(ref) & ref_hours_check == 1, 0, ref) # assign zero to individuals who have exactly one reference person in hhnum
  ) %>%
  ungroup() %>%
  select(-ref_hours_check)

sum(df$ref, na.rm = TRUE)
sum(df$ref == 0, na.rm = TRUE)
sum(is.na(df$ref))

# Assign reference person by HATLEV1D_num
df <- df %>%
  group_by(date_q, hhnum) %>%
  mutate(
    ref_educ = max(HATLEV1D_num, na.rm = TRUE),
    ref = if_else(is.na(ref) & HATLEV1D_num == ref_educ, 1, ref),
    ref_educ_check = sum(ref, na.rm = TRUE),
    ref = if_else(ref_educ_check > 1 & is.na(ref), 0, ref),
    ref = if_else(ref_educ_check > 1 & ref == 1, NA_real_, ref),
    ref_educ_check = sum(ref, na.rm = TRUE),
    ref = if_else(is.na(ref) & ref_educ_check == 1, 0, ref)
  ) %>%
  ungroup() %>%
  select(-ref_educ_check)

sum(df$ref, na.rm = TRUE)
sum(df$ref == 0, na.rm = TRUE)
sum(is.na(df$ref))

# Assign reference person by AGE
df <- df %>%
  group_by(date_q, hhnum) %>%
  mutate(
    ref_age = max(age, na.rm = TRUE),
    ref = if_else(is.na(ref) & age == ref_age, 1, ref),
    ref_age_check = sum(ref, na.rm = TRUE),
    ref = if_else(ref_age_check > 1 & is.na(ref), 0, ref),
    ref = if_else(ref_age_check > 1 & ref == 1, NA_real_, ref),
    ref_age_check = sum(ref, na.rm = TRUE),
    ref = if_else(is.na(ref) & ref_age_check == 1, 0, ref)
  ) %>%
  ungroup() %>%
  select(-ref_age_check)

sum(df$ref, na.rm = TRUE)
sum(df$ref == 0, na.rm = TRUE)
sum(is.na(df$ref))

# Assign reference person randomly
df <- df %>%
  group_by(date_q, hhnum) %>%
  mutate(
    random = if (any(is.na(ref))) { # check if there are any NAs in ref
      sampled = sample(row_number()[is.na(ref)], size = sum(is.na(ref)), replace = FALSE) 
      if_else(is.na(ref), match(row_number(), sampled), NA_integer_) # assign sequence number if ref is NA
    } else {
      NA_integer_  # if no NAs, set random to NA
    }
  ) %>%
  ungroup() %>%
  mutate(
    ref = if_else(!is.na(random) & random == 1, 1, ref), # assign ref = 1 to the first random member
    ref = if_else(is.na(ref), 0, ref) # assign ref = 0 to all remaining individuals with ref set to NA
  ) %>%
  select(-random)

sum(df$ref, na.rm = TRUE)
sum(df$ref == 0, na.rm = TRUE)
sum(is.na(df$ref))

df <- df %>%select(-starts_with("ref_"))

################################################################################
# Variable married (approximation)
if (ageGrpMethod %in% c(1, 2)) {
  age_diff = 5
} else if (ageGrpMethod == 3) {
  age_diff = 1
} else {
  stop("Error: invalid ageGrpMethod selected, please choose 1, 2 or 3.")
}

df <- df %>%
  group_by(date_q, hhnum) %>%
  mutate(
    rp_age = age[ref == 1][1],
    # find if at least one non-RP person is close in age to RP
    has_close_match = any(ref != 1 & !is.na(age) & abs(age - rp_age) <= age_diff),
    married = case_when(
      ref == 1 ~ ifelse(has_close_match, 1L, 0L),
      !is.na(age) & !is.na(rp_age) & abs(age - rp_age) <= age_diff ~ 1L,
      TRUE ~ 0L
    )
  ) %>%
  ungroup() %>%
  select(-rp_age, -has_close_match)

################################################################################
# Save the dataset
write.csv(df, file.path(OUTPUT, "LFS_data_wRP.csv"), row.names = FALSE)