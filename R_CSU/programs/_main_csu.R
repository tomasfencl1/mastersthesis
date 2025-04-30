################################################################################
# title: Household Heterogeneity and Monetary Policy Transmission in the Czech Economy
# author: Fencl Tomas
# 
# this script was created thanks to the STATA replication packages generously provided by Jiri Slacalek for his work:
# title: Household Balance Sheet Channels of Monetary Policy: A Back of the Envelope Calculation for the Euro Area
# authors: Slacalek Jiri, Tristani Oreste, Violante Giovanni L.
################################################################################

################################################################################
# MAIN CSU
# desc: runs .R files for LFSS dataset construction, imputation and elasticity estimation
################################################################################

# Reset environment
rm(list = ls())

################################################################################
# SWITCHES
# for data set up
appendDatasets <- 0 # 1 to appned datasets from quarterly files
createVariablesRP <- 0 # 1 to create variables and define reference person in the appended dataset

# options
top10Index <- 1 # HFCS data identify top10 group [default: No/0]
imputationModelChoice <- 2 # Choose model variant for HtM imputation, 1-3 [default: 2]
imputationWeighted <- 1 # Choose whether to use coeffs estimated in weighted regressions [default: Yes/1]
estMethod <- "lasso" # Choose estimation method used, probit/lasso/bma/bmahg [default: lasso]
verCalib <- 1 # Choose way of assigning prob cutoff points [default: 1]

################################################################################
# Define paths and file locations
ROOT <- "N:/MT" # personal , work "N:/MT"

# Path for dataset folders
DATA <- file.path(ROOT, "data")
CSUDATA <- file.path(DATA, "CSU")

# Path for HFCS input
HFCSOUTPUT <- file.path(ROOT, paste0("R_HFCS/output/csu"))
HFCSDBNAME <- "HFCS_wave4_CZ"

# Path for the generated output
OUTPUT <- file.path(ROOT, paste0("R_CSU/output"))
GRAPHS <- file.path(OUTPUT, "graphs")
TABLES <- file.path(OUTPUT, "tables")

# Path for the .R files tree
PROGRAMS <- file.path(ROOT, paste0("R_CSU/programs"))

################################################################################
# Functions to create the dataset
if (appendDatasets == 1) {
  # Create variables
  source(file.path(PROGRAMS, "append_dataset.R"))
}

if (createVariablesRP == 1) {
  # Create variables
  source(file.path(PROGRAMS, "create_dataset.R"))
  
  # Assign reference person
  source(file.path(PROGRAMS, "reference_person.R"))
}

################################################################################
# Imputation to CSU
source(file.path(PROGRAMS, "csu_htm_imputation.R"))

# Estimate incidence function
if (top10Index == 1) {
  source(file.path(PROGRAMS, "incidence_function_3htm.R")) # to get elasticities of merged nHtM and top10 group
  source(file.path(PROGRAMS, "incidence_function_4htm.R"))
} else {
  source(file.path(PROGRAMS, "incidence_function_3htm.R"))
}
