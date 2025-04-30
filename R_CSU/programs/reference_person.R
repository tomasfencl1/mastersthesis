################################################################################
# REFERENCE PERSON
# desc: on CSU data, uses respectively working hours, educ. and age to determine the reference person
################################################################################

# Load packages
library(dplyr)
library(tidyr)
library(readr)

################################################################################
# Load dataset
df <- read.csv(file.path(OUTPUT, "CSU_data.csv"))

################################################################################
# Create household weight
df <- df %>%
  group_by(date_q, hhnum) %>%
  mutate(wgthh = sum(wgtos, na.rm = TRUE)) %>%
  ungroup()

# Filter age between 15 and 64
#df <- df %>%
#  filter(age >= 15 & age <= 64)

# Assign reference person by HWUSUAL
df <- df %>%
  group_by(date_q, hhnum) %>%
  mutate(
    #hwusual = if_else(hwusual != 99, hwusual, NA_real_),
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

#sum(df$ref, na.rm = TRUE)
#sum(df$ref == 0, na.rm = TRUE)
#sum(is.na(df$ref))

# Assign reference person by ISCED
df <- df %>%
  group_by(date_q, hhnum) %>%
  mutate(
    #hat97lev_aux = if_else(hat97lev != 99, hat97lev, NA_real_),
    ref_educ = max(isced, na.rm = TRUE),
    ref = if_else(is.na(ref) & isced == ref_educ, 1, ref),
    ref_educ_check = sum(ref, na.rm = TRUE),
    ref = if_else(ref_educ_check > 1 & is.na(ref), 0, ref),
    ref = if_else(ref_educ_check > 1 & ref == 1, NA_real_, ref),
    ref_educ_check = sum(ref, na.rm = TRUE),
    ref = if_else(is.na(ref) & ref_educ_check == 1, 0, ref)
  ) %>%
  ungroup() %>%
  select(-ref_educ_check)

#sum(df$ref, na.rm = TRUE)
#sum(df$ref == 0, na.rm = TRUE)
#sum(is.na(df$ref))

# Assign reference person by AGE
df <- df %>%
  group_by(date_q, hhnum) %>%
  mutate(
    #age_aux = if_else(is.na(ref), age, NA_real_),
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

#sum(df$ref, na.rm = TRUE)
#sum(df$ref == 0, na.rm = TRUE)
#sum(is.na(df$ref))

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

#sum(df$ref, na.rm = TRUE)
#sum(df$ref == 0, na.rm = TRUE)
#sum(is.na(df$ref))

################################################################################
# Save the dataset
write.csv(df, file.path(OUTPUT, "CSU_data_wRP2.csv"))
