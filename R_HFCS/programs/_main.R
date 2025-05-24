################################################################################
# title: Household Heterogeneity and Monetary Policy Transmission in the Czech Economy
# author: Fencl Tomas
# 
# this script was created thanks to the STATA replication packages generously provided by Jiri Slacalek for his work:
# title: Household Balance Sheet Channels of Monetary Policy: A Back of the Envelope Calculation for the Euro Area
# authors: Slacalek Jiri, Tristani Oreste, Violante Giovanni L.
################################################################################

################################################################################
# MAIN 
# desc: runs the HFCS analysis, estimates imputation coefficients for CSU and prints results
################################################################################

# Reset environment and load packages
rm(list = ls())
library(openxlsx)

################################################################################
# SWITCHES
# for data setup
calculateNetIncome <- 0  # 1 to approximate net income using a simplification of the CZ tax schedule
estimateImputationCoeffs <- 0 # 1 to estimate coefficients and share targets for HtM imputation to CSU dataset

# options (HFCS flow)
top10Index <- 0  # Separate the top 10 by wealth [default: No/0]
illiquidef <- 0  # Definition of illiquid assets [default: alaSlacalek/0]
blowupStockMarketIndex <- 0  # Blow up holdings of stock market wealth of top 10 [default: No/0]
multiplyBizWealthWithStocks <- 0  # Multiply business wealth with stock prices [default: No/0] 

# options (CSU imputation)
imputationModelChoice <- 3 # Choose model variant for HtM imputation [default: 2]
  # 1 - Model with personal and employment characteristics, including job and ungrouped sec 
  # 2 - Model with personal and employment characteristics, including job and grouped sec
  # 3 - Model with personal characteristics only, without employment characteristics, job and sec
imputationWeighted <- 0 # Choose whether to use weights in imputation models [default: Yes/1]
runLASSO <- 0 # Run LASSO model for HtM imputation coefficients [default: Yes/1]
runBMA <- 0 # Run BMA model for HtM imputation coefficients [default: Yes/1]
runBMAhg <- 0 # Run BMA model with hyper-g prior for HtM imputation coefficients [default: No/0]
runProbit <- 1 # Run probit model for HtM imputation coefficients [default: Yes/1]

################################################################################
# Define paths and file locations
ROOT <- "/Users/tomasfencl/Documents/IES/_Master's Thesis"

# Path for dataset folders
DATA <- file.path(ROOT, "data")
HFCSDATA <- file.path(DATA, "hfcs/HFCS_UDB_4_0_ASCII")
NETINCDATA <- file.path(DATA, "netincome")

# HFCS dataset details
COUNTRY <- "CZ"
folder_name <- basename(HFCSDATA)
WAVE <- substr(folder_name, 10, 10)
DBNAME <- paste0("HFCS_wave",WAVE,"_", COUNTRY)

# Path for the generated output
OUTPUT <- file.path(ROOT, paste0("R_HFCS/output"))
GRAPHS <- file.path(OUTPUT, "graphs")
TABLES <- file.path(OUTPUT, "tables")

# Path for the .R files tree
PROGRAMS <- file.path(ROOT, paste0("R_HFCS/programs"))

################################################################################
#  Scaling of shocks and parameters
iesValue <- 0.5  # Elasticity of substitution

# MPCs (Marginal Propensity to Consume)
mpc_income_htm <- 0.50  # Income MPC for HtM
mpc_income_nhtm <- 0.05  # Income MPC for non-HtM

mpc_house_htm <- 0.07  # Housing MPC for HtM
mpc_house_nhtm <- 0.01  # Housing MPC for non-HtM

mpc_stock_htm <- 0.07  # Stock market MPC for HtM
mpc_stock_nhtm <- 0.01  # Stock market MPC for non-HtM

################################################################################
# Incidence functions (elasticities from CSU data)
incid_phtm_CZ <- 1
  # CZ ala Slacalek 2.65
incid_whtm_CZ <- 1
  # CZ ala Slacalek 1.82
incid_nhtm_CZ <- 1
  # CZ ala Slacalek 0.85

if (top10Index == 1) {
  incid_phtm_CZ <- 1
  # CZ ala Slacalek 2.75
  incid_whtm_CZ <- 1
  # CZ ala Slacalek 1.89
  incid_nhtm_CZ <- 1
  # CZ ala Slacalek 0.99
  incid_top10_CZ <- 1
  # CZ ala Slacalek 0.29
}

################################################################################
# Aggregate impulse responses (from VAR)
priborShock_var_nom <- -0.37 #/ 100  # IR policy rate shock size
  # CZ BVAR-sr -0.37, BP-SVAR -0.77
mpinflShock_var_CZ <- 0.74 #/ 100  # MP inflation response
  # CZ BVAR-sr 0.74, BP-SVAR -1.64
earnShock_var_CZ <- (1.58 + 3.10) #/ 100  # Employment + average wage response
  # CZ BVAR-sr (1.58 + 3.10), BP-SVAR (-0.30 + 2.16)
hpindShock_var_CZ <- 2.94 #/ 100  # House price response
  # CZ BVAR-sr 2.94, BP-SVAR 2.17
psxShock_var_CZ <- 4.17 #/ 100 # Stock price response
  # CZ BVAR-sr 4.17, BP-SVAR 3.96
consShock_var_CZ <- 2.97 #/ 100  # Consumption response
  # CZ BVAR-sr 2.97, BP-SVAR -0.74
priborShock_var_CZ <- priborShock_var_nom - mpinflShock_var_CZ # Real IR policy rate response

################################################################################
# Income by HtM (incidence times earning shock)
inc_phtm_CZ <- incid_phtm_CZ * earnShock_var_CZ
inc_whtm_CZ <- incid_whtm_CZ * earnShock_var_CZ
inc_nhtm_CZ <- incid_nhtm_CZ * earnShock_var_CZ

if (top10Index == 1) {
  inc_top10_CZ <- incid_top10_CZ * earnShock_var_CZ
}

################################################################################
# Aggregate saving rate, durables consumption share and stock market wealth coverage
aggSavingRate_CZ <- 0.1957 # Eurostat, online code: tec0013, source: nasa_10_ki
  # DE 0.167 # CZ 0.1957
aggDurablesConsShare_CZ <- 0.0802 # Eurostat, online code: nama_10_fcs, source: nama_10_fcs (9251 / 115392)
  # DE 0.112 # CZ 0.0801
stockCoverage_CZ <- 0.5662977  # share of market value in HFCS vs aggregate, calc in ureCharts.R
  # DE 0.636 # CZ 0.5662977

################################################################################
# Preliminary operations on HFCS data
source(file.path(PROGRAMS, "preliminary_operations.R"))

################################################################################
# Calculate net income based on a simplification of the CZ tax schedule
if (calculateNetIncome == 1) {
  source(file.path(PROGRAMS, paste0(COUNTRY, "calcNetIncome.R")))
}

################################################################################
# Load the main net income dataset for CZ from CSV and select columns
NINC <- read.csv(file.path(NETINCDATA, paste0(DBNAME, "_netIncome.csv"))) %>%
  dplyr::select(id, netinc) %>%
  rename(di2001 = netinc)

# Merge with the main dataset by id
df <- read.csv(file.path(OUTPUT, paste0(DBNAME, "_orig.csv")))
df <- left_join(df, NINC, by = "id")
df <- df %>%
  arrange(sa0100, sa0010, im0100)
# Save the merged data as a CSV file
write.csv(df, file.path(OUTPUT, paste0(DBNAME, "_orig.csv")), row.names = FALSE)

################################################################################
# HtM status estimation
source(file.path(PROGRAMS, "statusHtM.R"))
source(file.path(PROGRAMS, "charts/HtMdemographicsCharts.R"))

################################################################################
# Coefficient and share target estimation for imputation of HtM to CSU dataset
if (estimateImputationCoeffs == 1) {
  source(file.path(PROGRAMS, "csu/htm_prediction.R"))
  source(file.path(PROGRAMS, "csu/htmshares_HFCStargets.R"))
}

################################################################################
# Programs to calculate URE and NNP 
source(file.path(PROGRAMS, "nnp.R"))
source(file.path(PROGRAMS, "savingRateByHtM.R"))
source(file.path(PROGRAMS, "ure.R"))
source(file.path(PROGRAMS, "charts", "ureCharts.R"))

################################################################################
# Aggregate consumption response decomposition
source(file.path(PROGRAMS, "consDecomposition.R"))
source(file.path(PROGRAMS, "charts/consDecompositionCharts.R"))
