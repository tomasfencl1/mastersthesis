################################################################################
# INCOME AND CONSUMPTION SHARES BY HtM
# desc: calculate income shares by HtM status and share of nondurables consumption for nHtM
################################################################################

# Load packages
library(dplyr)
library(tidyr)

################################################################################
# Load dataset
df <- read.csv(file.path(OUTPUT, paste0(DBNAME, "_orig.csv")))

################################################################################
# Calculate mean net income 
# Uncomment this section if you want to calculate and save mean net income
df_meaninc <- df %>%
  dplyr::group_by(sa0100, statushtm) %>%
  dplyr::summarize(Y_mean = weighted.mean(di2001, hw0010, na.rm = TRUE), .groups = "drop")

# Transform dataframe
df_meaninc <- df_meaninc %>%
  pivot_wider(
    names_from = statushtm,
    values_from = Y_mean,
  ) %>%
  rename(
    meaninc_nHtM = '3',
    meaninc_pHtM = '1',
    meaninc_wHtM = '2',
    !!!if (top10Index == 1) setNames('4', 'meaninc_top10')
  )

if (top10Index == 1) {
  df_meaninc <- df_meaninc %>%
    mutate(
      meaninc_allnHtM = (meaninc_nHtM * sum(df$hw0010[df$statushtm == 3], na.rm = TRUE) +
                           meaninc_top10 * sum(df$hw0010[df$statushtm == 4], na.rm = TRUE)) /
        (sum(df$hw0010[df$statushtm %in% c(3, 4)], na.rm = TRUE))
    )
}

# Save the dataset
#write.csv(df_meaninc, file.path(TABLES, paste0(DBNAME, "_Y_mean.csv")), row.names = FALSE)
#View(df_meaninc)

################################################################################
# Calculate weighted total income
totalinc <- sum(df$di2001 * df$hw0010, na.rm = TRUE)

# Calculate type-specific weighted incomes and income shares
df_incs <- df %>%
  dplyr::group_by(sa0100, statushtm) %>%
  dplyr::summarize(
    nHtM_inc = sum(ifelse(statushtm == 3, di2001 * hw0010, 0), na.rm = TRUE),
    HtM_inc = sum(ifelse(statushtm %in% c(1, 2), di2001 * hw0010, 0), na.rm = TRUE),
    pHtM_inc = sum(ifelse(statushtm == 1, di2001 * hw0010, 0), na.rm = TRUE),
    wHtM_inc = sum(ifelse(statushtm == 2, di2001 * hw0010, 0), na.rm = TRUE),
    top10_inc = ifelse(top10Index == 1, sum(ifelse(statushtm == 4, di2001 * hw0010, 0), na.rm = TRUE), NA_real_),
    allnHtM_inc = ifelse(top10Index == 1, sum(ifelse(statushtm %in% c(3, 4), di2001 * hw0010, 0), na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    incs_nHtM = nHtM_inc / totalinc,
    incs_HtM = HtM_inc / totalinc,
    incs_pHtM = pHtM_inc / totalinc,
    incs_wHtM = wHtM_inc / totalinc,
    incs_top10 = (top10Index == 1) * (top10_inc / totalinc),
    incs_allnHtM = (top10Index == 1) * (allnHtM_inc / totalinc),
  )

# Transform dataframe
df_incs <- df_incs %>%
  group_by(sa0100) %>%
  summarise(
    incs_nHtM = sum(incs_nHtM, na.rm = TRUE),
    incs_pHtM = sum(incs_pHtM, na.rm = TRUE),
    incs_wHtM = sum(incs_wHtM, na.rm = TRUE),
    incs_HtM = sum(incs_HtM, na.rm = TRUE),
    incs_top10 = if (top10Index == 1) sum(incs_top10, na.rm = TRUE) else NULL,
    incs_allnHtM = if (top10Index == 1) sum(incs_allnHtM, na.rm = TRUE) else NULL,
    .groups = "drop"  # Remove grouping after summarization
  )

# Save the dataset
write.csv(df_incs, file.path(TABLES, paste0(DBNAME, "_HtM_inc_shares.csv")), row.names = FALSE)
#View(df_incs)

################################################################################
# Merge the original data with income shares
df <- df %>%
  left_join(df_incs, by = "sa0100")

# Load mean incomes (if available) and merge
df <- df %>%
  left_join(df_meaninc, by = "sa0100")

# Nondurables consumption share, following appendix A.3
ctilde <- 1 - aggSavingRate_CZ
ytilde_HtM <- df_incs$incs_HtM
ytilde_allnHtM <- 1 - df_incs$incs_HtM
ctilde_allnHtM <- (ctilde - ytilde_HtM) / ytilde_allnHtM # Share of consumption for all nHtM groups
covery_allnHtM_nd_CZ <- ctilde_allnHtM * (1 - aggDurablesConsShare_CZ) # Assuming that nd share is the same for all HtM groups

# v1 (meaninc by group) Net savings mean  
YC_nHtM_CZ <- as.numeric((1 - ctilde_allnHtM) * df_meaninc$meaninc_nHtM)
if (top10Index == 1) {
  YC_top10_CZ <- as.numeric((1 - ctilde_allnHtM) * df_meaninc$meaninc_top10)
}
df <- df %>%
  mutate(
    YC = case_when(
      statushtm == 3 ~ YC_nHtM_CZ, # Assign YC_nHtM_CZ if statushtm = 3
      #statushtm == 4 ~ if (top10Index == 1) YC_top10_CZ else NA_real_, # Assign YC_top10_CZ if top10Index == 1 and statushtm = 4
      statushtm == 4 ~ ifelse(top10Index == 1, YC_top10_CZ, NA_real_), # Assign YC_top10_CZ if top10Index == 1 and statushtm = 4
      TRUE ~ NA_real_ # Assign NA for all other cases
    )
  )

# v2 (allnHtM meaninc) Net savings mean 
#YC_allnHtM_CZ <- as.numeric((1 - ctilde_allnHtM) * ifelse(top10Index == 1, df_meaninc$meaninc_allnHtM, df_meaninc$meaninc_nHtM))
#
#df <- df %>%
#  mutate(
#    YC = ifelse(statushtm > 2, YC_allnHtM_CZ, NA_real_)
#  )

# Net consumption mean
df <- df %>%
  mutate(
    C = case_when(
      statushtm == 1 ~ as.numeric((1 - aggDurablesConsShare_CZ) * df$di2001), # Y=C, adjusted for nondurable consumption only
      statushtm == 2 ~ as.numeric((1 - aggDurablesConsShare_CZ) * df$di2001), # Y=C, adjusted for nondurable consumption only
      statushtm == 3 ~ as.numeric((covery_allnHtM_nd_CZ) * df$di2001), # adjusted for consumption share and nondurable consumption
      statushtm == 4 ~ ifelse(top10Index == 1, as.numeric((covery_allnHtM_nd_CZ) * df$di2001), NA_real_), # adjusted for consumption share and nondurable consumption
      TRUE ~ NA_real_ # Assign NA for all other cases
      )
  )

#df %>%
#  group_by(statushtm) %>%
#  summarise(weighted_avg_C = weighted.mean(C, w = hw0010, na.rm = TRUE))


################################################################################
# Save the dataset
write.csv(df, file.path(OUTPUT, paste0(DBNAME, "_orig.csv")), row.names = FALSE)
