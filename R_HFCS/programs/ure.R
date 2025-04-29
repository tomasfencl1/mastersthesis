################################################################################
# NET INTEREST RATE EXPOSURE IDENTIFICATION BY HOUSEHOLD
# desc: calculates net interest rate exposure for households
################################################################################

# Load packages
library(dplyr)

################################################################################
# Load dataset
df <- read.csv(file.path(OUTPUT, paste0(DBNAME, "_orig.csv")))

################################################################################
# Adjustable-rate mortgages and consumer credit
df <- df %>%
  mutate(
    arm = rowSums(dplyr::select(., dl1110a, dl1120a), na.rm = TRUE),
  )

# Calculate sums by country for ARM and mortgages
df_armsum <- df %>%
  dplyr::group_by(sa0100) %>%
  dplyr::summarize(
    sumM = sum(dl1100 * hw0010, na.rm = TRUE),
    allARM = sum(arm * hw0010, na.rm = TRUE)
  )

# Merge to dataset
df <- df %>%
  left_join(df_armsum, by = "sa0100") %>%
  mutate(shareARM = 100 * allARM / sumM) %>%
  dplyr::select(-allARM, -sumM)

################################################################################
# Create intermediate variables for businessWealth, lifeInsurance, deposits
df <- df %>%
  mutate(
    businessWealth = -da2104,
    lifeInsurance = -da2109,
    deposits = -da2101
  )

# Calculate maturingAssets and maturingLiabilities
df <- df %>%
  mutate(
    maturingAssets_aux = 0.25 * rowSums(dplyr::select(., finAssets, businessWealth, lifeInsurance, deposits), na.rm = TRUE),
    maturingAssets = coalesce(maturingAssets_aux, 0) + coalesce(da2101, 0),
    maturingLiabilities = rowSums(dplyr::select(., arm, dl1200), na.rm = TRUE),
    debt = dl1200
  ) %>%
  dplyr::select(-businessWealth, -lifeInsurance, -deposits, -maturingAssets_aux)

# Simplified URE for every country
df <- df %>%
  mutate(
    ure = maturingAssets - maturingLiabilities,
    ureyc = YC + maturingAssets - maturingLiabilities
  )

################################################################################
# Save the dataset
write.csv(df, file.path(OUTPUT, paste0(DBNAME, "_orig.csv")), row.names = FALSE)

################################################################################
# Optional summary statistics (if needed) # MEANs SHALL BE WEIGHTED
#df_statistics <- df %>%
#  dplyr::group_by(sa0100, statushtm) %>%
#  dplyr::summarize(
#    Y_mean = mean(di2001, na.rm = TRUE),
#    #savingsrate = mean(savingsrate, na.rm = TRUE), # WHAT IS THIS?
#    incs = mean(incs, na.rm = TRUE), # NOT INCL. IN MERGED DATASET
#    #CY_nHtM = mean(CY_nHtM, na.rm = TRUE), # WHAT IS THIS?
#    #YC = mean(YC, na.rm = TRUE), 
#    ure = mean(ure, na.rm = TRUE),
#    #ureyc = mean(ureyc, na.rm = TRUE),
#    .groups = "drop"
#  )

################################################################################
# Save the summary statistics
#write.csv(df_statistics, file.path(TABLES, paste0(DBNAME, "_ure_statistics.csv")), row.names = FALSE)