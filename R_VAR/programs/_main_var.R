################################################################################
# title: Household Heterogeneity and Monetary Policy Transmission in the Czech Economy
# author: Fencl Tomas
################################################################################

################################################################################
# MAIN VAR R
# desc: runs the data preparation steps for VAR
################################################################################

# Reset environment
rm(list = ls())

################################################################################
# Define paths and file locations
ROOT <- "/Users/tomasfencl/Documents/IES/_Master's Thesis"

# Path for dataset folders
DATA <- file.path(ROOT, "data")
VARDATA <- file.path(DATA, "VAR")

# Path for the generated output
OUTPUT <- file.path(ROOT, paste0("R_VAR/output"))

# Path for the .R files tree
PROGRAMS <- file.path(ROOT, paste0("R_VAR/programs"))

################################################################################
# Program to transform the datafile
source(file.path(PROGRAMS, "datatransf.R"))

################################################################################
# Program to analyze the variables
source(file.path(PROGRAMS, "analys.R"))

################################################################################
# Program to run local projections
source(file.path(PROGRAMS, "lp.R"))