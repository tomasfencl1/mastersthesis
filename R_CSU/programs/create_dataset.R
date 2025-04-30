################################################################################
# CREATE DATASET
# desc: on CSU data, merges quarterly CSU files and creates one source dataset
################################################################################

# Load packages
library(dplyr)
library(readr)
library(zoo)

################################################################################
# Load dataset
df <- read.csv(file.path(OUTPUT, "CSU_data_appended.csv"))

################################################################################
# Sort and generate quarterly date
df <- df %>%
  mutate(
    date_q = as.yearqtr(paste(year, quarter), format = "%Y %q")
  ) %>%
  arrange(year, quarter, hhnum)

################################################################################
# Delete duplicit persnum / rows in given date_q
df <- df %>%
  group_by(date_q, persnum) %>%
  filter(row_number() == 1) %>%  # Keep only the first occurrence of duplicate persnum within each date_q
  ungroup()

################################################################################
# Age, age2, selfempl, married, female and employment status variables
df <- df %>%
  mutate(
    hwusual = obvhod,
    age = vek,
    age2 = age^2,
    selfempl = if_else(zampost %in% c(2,3), 1, 0),
    married = if_else(rodstav == 2, 1, 0),
    female = if_else(pohl == 2, 1, 0),
    empl = if_else(prac1h == 1 | (prac1h == 2 & prac0h == 1), 1, 0),
    perm = if_else(typsml == 2, 1, 0)
  )

# Education levels
df <- df %>%
  mutate(
    prim_edu = if_else(isced == 1, 1, 0),
    sec_edu = if_else(isced == 2, 1, 0),
    tert_edu = if_else(isced == 3, 1, 0)
  )

# Job dummy from ISCO
job_categories <- sort(unique(df$zamisco), na.last = TRUE)
for (job in job_categories[!is.na(job_categories)]) {
  dummy_name <- paste0("job", job)
  df[[dummy_name]] <- ifelse(df$zamisco == job, 1, ifelse(is.na(df$zamisco), NA, 0))
}
#df_job <- df %>% filter(!is.na(zamisco)) %>%
#  mutate(jobgroup = zamisco)
#job_counts <- table(df_job$jobgroup)
#print(job_counts)
#barplot(job_counts, main = "Distribution of ISCO (zamisco)",
#        col = "salmon", las = 2, xlab = "ISCO", ylab = "Count")

# Sector dummy from NACE2
#nace2_sectors <- LETTERS[1:21]
#for (sector in nace2_sectors) {
#  dummy_name <- paste0("sec", which(nace2_sectors == sector))
#  df[[dummy_name]] <- ifelse(df$zamnace == sector, 1, ifelse(is.na(df$zamnace), NA, 0))
#}
#df_sec <- df %>% filter(!is.na(zamnace))
#sec_counts <- table(df_sec$zamnace)
#print(sec_counts)
#barplot(sec_counts, main = "Distribution of Detailed NACE2 (zamnace)",
#        col = "lightblue", las = 2, xlab = "NACE2", ylab = "Count")

# Own NACE2 grouping (Slacalek i. transforms NACE2 to NACE, ii. groups some codes, e.g. A,B; C,D,E; O,P,Q)
df <- df %>%
  mutate(xsecgroup = case_when(
    zamnace %in% c("A") ~ "A", # Primary Industries, Agriculture, Forestry, Fishing
    zamnace %in% c("B") ~ "B", # Extractive Industries, Mining and Quarrying
    zamnace %in% c("C") ~ "C", # Manufacturing, Manufacturing
    zamnace %in% c("D", "E")~ "D,E", # Energy and Utilities, Electricity, Gas, Water Supply etc.
    zamnace %in% c("F") ~ "F", # Construction, Construction
    zamnace %in% c("G") ~ "G", # Trade and Distribution, Wholesale and Retail Trade
    zamnace %in% c("H", "I")~ "H,I", # Transport and Hospitality, Transportation; Accommodation and Food Service
    zamnace %in% c("J") ~ "J", # Creative and Media,Publishing, Broadcasting, etc.
    zamnace %in% c("K") ~ "K", # Information and Communication, Telecom, IT, Consulting etc.
    zamnace %in% c("L", "M")~ "L,M", # Finance and Real Estate, Financial, Insurance; Real Estate
    zamnace %in% c("N", "O")~ "N,O", # Professional and Admin Services, Professional, Scientific, Technical; Admin Support
    zamnace %in% c("P", "Q")~ "P,Q", # Public Sector and Education, Public Administration; Education
    zamnace %in% c("R") ~ "R", # Health and Social Services, Health and Social Work
    zamnace %in% c("S", "T")~ "S,T", # Leisure and Other Services, Arts, Sports, Recreation; Other Services
    zamnace %in% c("U", "V")~ "U,V", # Other/Household and Extraterritorial, Households, Extraterritorial, etc.
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

# Arrange final dataset
df <- df %>%
  arrange(date_q, hhnum)

################################################################################
# Save the dataset
write.csv(df, file.path(OUTPUT, "CSU_data.csv"))