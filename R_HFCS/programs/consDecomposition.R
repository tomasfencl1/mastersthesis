################################################################################
# CONSUMPTION DECOMPOSITION
# desc: evaluates effect of IR change on consumption through transm. channels
################################################################################

# Load packages
library(dplyr)

################################################################################

# Load dataset
df <- read.csv(file.path(OUTPUT, paste0(DBNAME, "_orig.csv")))

################################################################################
# Parameters
iRateShock_CZ <- priborShock_var_CZ # Transitory short-term interest rate shock
priceShock_CZ <- mpinflShock_var_CZ # Temporary price shock
yoverc_CZ <- c(1 / (1-aggDurablesConsShare_CZ), 1 / (1-aggDurablesConsShare_CZ), 1 / covery_allnHtM_nd_CZ, if (top10Index == 1) 1 / covery_allnHtM_nd_CZ)  # Consumption-to-income ratios
meanIncGrowth_CZ <- c(inc_phtm_CZ, inc_whtm_CZ, inc_nhtm_CZ, if (top10Index == 1) inc_top10_CZ) # Mean income growth rates
mpc_income <- c(mpc_income_htm, mpc_income_htm, mpc_income_nhtm, if (top10Index == 1) mpc_income_nhtm)  # MPC by HtM category
sigma <- c(0, 0, iesValue, if (top10Index == 1) iesValue)  # Elasticity of substitution

################################################################################
# House and stock price responses
df <- df %>%
  mutate(
    dqt_house_CZ = hpindShock_var_CZ,
    dqt_stock_CZ = psxShock_var_CZ
  )

# MPC for housing and stock wealth
df <- df %>%
  mutate(
    mpc_house_CZ = mpc_house_htm,
    mpc_house_CZ = ifelse(nonhtm == 1, mpc_house_nhtm, mpc_house_CZ),
    mpc_stock_CZ = mpc_stock_htm,
    mpc_stock_CZ = ifelse(nonhtm == 1, mpc_stock_nhtm, mpc_stock_CZ)
  )

# Replace missing values in asset holdings with 0
df <- df %>%
  mutate(
    house = ifelse(is.na(house), 0, house),
    da2105 = ifelse(is.na(da2105), 0, da2105),
    da1140 = ifelse(is.na(da1140), 0, da1140)
  )

################################################################################
# Calculate total weighted stock market wealth and adjusted wealth
df <- df %>%
  group_by(sa0100) %>%
  mutate(
    richIndex = ifelse(top10Index == 1, 4, 3),
    da2105_temp = ifelse(statushtm >= richIndex, da2105, 0),
    stockScalingFac = 1,
    stockScalingFac = case_when(
      statushtm == richIndex ~ stockCoverage_CZ * (sum(da2105_temp * hw0010, na.rm = TRUE) / sum(da2105 * hw0010, na.rm = TRUE)),
      TRUE ~ stockScalingFac
    ),
    da2105adj = da2105 / stockScalingFac
  ) %>%
  ungroup()

# Apply adjustment if enabled
if (blowupStockMarketIndex == 1) {
  df <- df %>% mutate(da2105 = da2105adj)
}

################################################################################
# Create stock variable and adjust if multiplying business wealth
df <- df %>%
  mutate(
    stock_temp = if (multiplyBizWealthWithStocks == 1) da2105 + da1140 else da2105,
    stock = stock_temp
  )

################################################################################
# Measure aggregate consumption response to the shock as decomposition
df <- df %>%
  mutate(
    dURE1 = NA_real_,
    dNNP1 = NA_real_,
    dy1 = NA_real_,
    dsub1 = NA_real_,
    dhouse1 = NA_real_,
    dstock1 = NA_real_
  )

# Use ure as ureyc for HtM households
df <- df %>%
  mutate(ureyc = ifelse(is.na(ureyc), ure, ureyc))

# Divide ureyc, nnp, house and stock by 1000
#df <- df %>%
#  mutate(
#    ureyc = ureyc / 1000,
#    nnp = nnp / 1000,
#    house = house / 1000,
#    stock = stock / 1000
#  )

# Iterate over hand-to-mouth status groups
unique_status <- unique(df$statushtm)

for (s in unique_status) {
  df <- df %>%
    mutate(
      # Substitution effect (dsub)
      dsub1 = ifelse(
        statushtm == s,
        -1 * sigma[s] * (1 - mpc_income[s]) * iRateShock_CZ,
        dsub1
      ),
      # Net interest rate exposure (NIRE / URE)
      dURE1 = ifelse(
        statushtm == s,
        mpc_income[s] * ureyc * 1/C * iRateShock_CZ,
        dURE1
      ),
      # Aggregate income effect (dY)
      dy1 = ifelse(
        statushtm == s,
        mpc_income[s] * meanIncGrowth_CZ[s] * yoverc_CZ[s],
        dy1
      ),
      # Net nominal position exposure (NNP)
      dNNP1 = ifelse(
        statushtm == s,
        -1 * mpc_income[s] * nnp * 1/C * priceShock_CZ,
        dNNP1
      ),
      # Housing channel effect
      dhouse1 = ifelse(
        statushtm == s,
        mpc_house_CZ * house * 1/C * dqt_house_CZ,
        dhouse1
      ),
      # Stock market channel effect
      dstock1 = ifelse(
        statushtm == s,
        mpc_stock_CZ * stock * 1/C * dqt_stock_CZ,
        dstock1
      )
    )
}

# Keep only relevant columns
df_results <- df %>%
  dplyr::select(
    id, sa0100, sa0010, im0100, starts_with("dURE"), starts_with("dNNP"),
    starts_with("dy"), starts_with("dsub"), starts_with("dhouse"), starts_with("dstock"),
    statushtm, poorhtm, wealhtm, nonhtm, di2001, hw0010, ureyc, nnp, house, stock, C,
  )

################################################################################
# Save the dataset
write.csv(df_results, file.path(TABLES, paste0(DBNAME, "_effectDecomp.csv")), row.names = FALSE)
