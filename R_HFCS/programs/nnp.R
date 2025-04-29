################################################################################
# NOMINAL NET POSITION IDENTIFICATION BY HOUSEHOLD 
# desc: calculate net nominal position of households
################################################################################

# Load packages
library(dplyr)

################################################################################
# Load dataset
df <- read.csv(file.path(OUTPUT, paste0(DBNAME, "_orig.csv")))

################################################################################
# Calculate net wealth
df <- df %>%
  mutate(
    #netWealth = dn3001,
    nfp = ifelse(is.na(finAssets), 0, finAssets) - ifelse(is.na(totDebt), 0, totDebt),
    inp = rowSums(dplyr::select(., da2105, da2104, da2102), na.rm = TRUE), # computing the indirect nominal position as investments of household equity in the corporate sector, the sum of publicy traded shares, values of non-self employment private businesses and mutual funds
    nnp = nfp - inp, # generate the net nominal position at HH level: from the paper of Adam, Zhu (2016), and Doepke and Schneider (2006)
    #exposure = nnp / netWealth # determine exposure as NNP / netWealth, s.t. exposure <= 1
  )

# Ensure exposure does not exceed 1
#df <- df %>%
#  mutate(exposure = ifelse(exposure > 1, 1, exposure))

################################################################################
# Save the dataset
write.csv(df, file.path(OUTPUT, paste0(DBNAME, "_orig.csv")), row.names = FALSE)