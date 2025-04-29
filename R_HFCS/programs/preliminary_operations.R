################################################################################
# PRELIMINARY OPERATIONS ON HFCS DATA
# desc: data merging, type handling, creation of quintiles, age brackets, etc.
################################################################################

# Load packages
library(dplyr)
library(statar)

################################################################################
# Merge datasets
df1 <- read.csv(file.path(HFCSDATA, "d1.csv"))
df2 <- read.csv(file.path(HFCSDATA, "d2.csv"))
df3 <- read.csv(file.path(HFCSDATA, "d3.csv"))
df4 <- read.csv(file.path(HFCSDATA, "d4.csv"))
df5 <- read.csv(file.path(HFCSDATA, "d5.csv"))
df_list <- list(df1, df2, df3, df4, df5) %>%
  lapply(function(df) {
    df %>%
      mutate(dhemph1 = as.character(dhemph1)) # Convert dhemph1 to character
  })
df <- bind_rows(df_list)
#df <- bind_rows(df1, df2, df3, df4, df5)
#write.csv(df, file.path(HFCSDATA, "D_compl.csv"), row.names = FALSE)
#df <- read.csv(file.path(HFCSDATA, "D_compl.csv"))
df <- df %>% filter(sa0100 == COUNTRY)

# Precautionary sorting
df <- df %>% arrange(sa0100, sa0010, im0100)

# Check if 'sa0100' is numeric
#if (is.numeric(df$sa0100)) {
#  # Convert 'sa0100' to a character string if it's numeric
#  df <- df %>%
#    mutate(sa0100 = as.character(sa0100)) %>%
#    arrange(sa0100, sa0010, im0100)
#} else {
#  # If 'sa0100' is already a string, ensure sorting and encoding
#  df <- df %>% arrange(sa0100)
#  # Convert 'sa0100' to a factor to simulate encoding
#  df <- df %>% mutate(sa0100 = factor(sa0100))
#}

################################################################################
# Define flows, assets, and debts
#cleanFlows <- c("di1420", "di1400", "di1300", "dl2000")
#cleanAssets <- c("da1000", "da2100", "da2102", "da2104", "da2105")
#cleanDebts <- c("dl1000", "dl1100", "dl1200")
#cleanit <- c(cleanFlows, cleanAssets, cleanDebts)
#
## Replace missing or negative values
#for (v in cleanit) {
#  df[[v]] <- ifelse((is.na(df[[v]]) | df[[v]] < 0), NA, df[[v]])
#}

# Negative or zero gross income to NA
df$di2000 <- ifelse(df$di2000 <= 0 | is.na(df$di2000), NA, df$di2000)

# Drop rows with NA gross income
df <- df %>%
  filter(!is.na(di2000))

################################################################################
# Recalculate income and net wealth quintiles for country
# Drop existing quintile variables if they exist
#df <- df %>% select(-matches("^incqtile|^nwqtile"))
#
## Prepare
#imputations <- unique(df$im0100)
#
## Loop through each implicate and calculate quintiles for all rows
#for (i in imputations) {
#  # Create columns for income and wealth quintiles
#  df <- df %>%
#    mutate(
#      !!paste0("implic_incqtile", i) := ifelse(im0100 == i, xtile(as.numeric(di2000), n = 5, wt = hw0010), NA_real_),
#      !!paste0("implic_nwqtile", i) := ifelse(im0100 == i, xtile(as.numeric(dn3001), n = 5, wt = hw0010), NA_real_)
#    )
#}
#
## Aggregate quintiles across implicates
#df <- df %>%
#  mutate(
#    incqtile = rowSums(select(., starts_with("implic_incqtile")), na.rm = TRUE),
#    nwqtile = rowSums(select(., starts_with("implic_nwqtile")), na.rm = TRUE)
#  )
#
## Drop individual implicate-specific quintiles
#df <- df %>% select(-starts_with("implic_incqtile"), -starts_with("implic_nwqtile"))
#
## Add labels for quintiles
#df <- df %>%
#  mutate(
#    incqtile = factor(incqtile, levels = 1:5, labels = c("1st quintile", "2nd quintile", "3rd quintile", "4th quintile", "5th quintile")),
#    nwqtile = factor(nwqtile, levels = 1:5, labels = c("1st quintile", "2nd quintile", "3rd quintile", "4th quintile", "5th quintile"))
#  )
#
################################################################################
# Create age brackets like in the report
if ("ageRangeRP" %in% names(df)) {
  df <- df %>% select(-ageRangeRP)  # Drop 'ageRangeRP' to re-create
}

# Create age brackets based on the age of household head (dhageh1)
df <- df %>%
  mutate(ageRangeRP = case_when(
    dhageh1 > 15 & dhageh1 < 35 ~ 1,
    dhageh1 > 34 & dhageh1 < 45 ~ 2,
    dhageh1 > 44 & dhageh1 < 55 ~ 3,
    dhageh1 > 54 & dhageh1 < 65 ~ 4,
    dhageh1 > 64 & dhageh1 < 75 ~ 5,
    dhageh1 > 74 ~ 6,
    TRUE ~ NA_real_  # Set to NA if age is missing or <16
  ))

# Label the age brackets
df$ageRangeRP <- factor(df$ageRangeRP, 
                        levels = 1:6,
                        labels = c("16-34", "35-44", "45-54", "55-64", "65-74", "75+"))

################################################################################
# Generate income and wealth components
df <- df %>%
  mutate(across(c(di1620, di1700), as.numeric)) %>%
  mutate(
    grossIncome = di2000,
    emplIncome = ifelse((di1100i == 0 & (is.na(di1100)) | di1100 < 0), NA, di1100),
    selfIncome = ifelse((di1200i == 0 & (is.na(dl1200)) | dl1200 < 0), NA, dl1200),
    pensions = ifelse((di1500i == 0 & is.na(di1500)) | di1500 < 0, NA, di1500),
    finIncome = ifelse((di1400i == 0 & is.na(di1400)) | di1400 < 0, NA, di1400),
    rentIncome = ifelse((di1300i == 0 & is.na(di1300)) | di1300 < 0, NA, di1300),
    unemplBenefits = ifelse((di1610i == 0 & is.na(di1610)) | di1610 < 0, NA, di1610)#,
    #transfers = rowSums(select(di1620, di1700), na.rm = TRUE)
  )

# Ensure transfers are set to NA if both di1620 and di1700 indicate missing values
#df <- df %>%
#  mutate(
#    transfers = ifelse((di1620 == 0 & di1700 == 0) & is.na(transfers), NA, transfers)
#  )

################################################################################
# Save the dataset
write.csv(df, file.path(OUTPUT, paste0(DBNAME, "_orig.csv")), row.names = FALSE)
#write.xlsx(df, file.path(OUTPUT, paste0(DBNAME, "_orig_1prelim.xlsx")))