################################################################################
# HAND-TO-MOUTH IDENTIFICATION BY HOUSEHOLD
# desc: creates & merges occupational and unsecured datasets, determ. HtM status
################################################################################

# Load packages
library(dplyr)
library(tidyr)
library(Hmisc)

################################################################################
# Load P1 to P5 occupational pension data and append them
p_files <- paste0("P", 1:5, ".csv")
occupational_data1 <- bind_rows(lapply(p_files, function(f) read.csv(file.path(HFCSDATA, f))))
occupational_data <- occupational_data1

# Calculate the current value of all occupational pension plans
# wave2 pf0710 related to wave4 pfa0802, i.e. current value of a plan, type occupational
occupational_data <- occupational_data %>%
  group_by(sa0100, sa0010, im0100) %>%
  mutate(occupational = if (all(is.na(pfa0802))) NA else sum(pfa0802, na.rm = TRUE)) %>%
  ungroup()

# v1 (by ref. person) Create the 'couple' variable based on 'pa0100' (either married or consensual union)
# Assign couple status based on reference person (RA0100 == 1)
#occupational_data <- occupational_data %>%
#  group_by(hid) %>%  # Group by household ID
#  mutate(couple = ifelse(any(ra0100 == 1 & pa0100 %in% c(2, 3)), 1, 0)) %>%  # Check reference person’s marital status
#  ungroup()

# v2 (any couple in hid) Create the 'couple' variable based on 'pa0100' 
#occupational_data <- occupational_data %>%
#  mutate(couple = ifelse(pa0100 %in% c(2, 3), 1, 0)) %>%
#  group_by(hid) %>%  # Group by household ID
#  mutate(couple = if (any(couple == 1)) 1 else 0) %>%  # Assign 1 if any member in the household is a couple
#  ungroup()

# v3 (no logic) Create the 'couple' variable based on 'pa0100' 
#occupational_data <- occupational_data %>%
#  mutate(couple = ifelse(pa0100 %in% c(2, 3), 1, 0))

# Rename columns
occupational_data <- occupational_data %>%
  rename(id = hid, pid = id)
# Drop duplicates based on 'id' to ensure each household appears only once
occupational_data <- occupational_data %>%
  distinct(id, .keep_all = TRUE)
# Keep only relevant columns
occupational_data <- occupational_data %>%
  dplyr::select(id, occupational)#, couple)

# Save the cleaned dataset
write.csv(occupational_data, file.path(OUTPUT, paste0(DBNAME,"_occupational.csv")), row.names = FALSE)

################################################################################
# Load H1 to H5 unsecured loan data and append them
h_files <- paste0("H", 1:5, ".csv")
unsecured_data1 <- bind_rows(lapply(h_files, function(f) read.csv(file.path(HFCSDATA, f))))
unsecured_data <- unsecured_data1

# Initialize the non-collateralized loan variables
unsecured_data <- unsecured_data %>%
  mutate(
    unsecuredhome = 0,
    unsecuredhmr = 0,
    unsecuredoth = 0,
    unsecuredren = 0,
    unsecuredveh = 0
  )

# Identify relevant hc050_a and hc080_ columns dynamically 
hc080_vars <- grep("^hc080", names(unsecured_data), value = TRUE)
nocoloans <- length(hc080_vars) # maximum number of loans household has, used for loop
hc080_columns <- paste0("hc080", 1:nocoloans)        
hc050_columns <- paste0("hc050", 1:nocoloans, "a") # only considering "a" - first choice purpose of the loan

unsecured_data[hc080_columns] <- lapply(unsecured_data[hc080_columns], function(x) ifelse(is.na(x), 0, x))
unsecured_data[hc050_columns] <- lapply(unsecured_data[hc050_columns], function(x) ifelse(is.na(x), 0, x))

# Assign loan sums to "unsecured" variables
for (i in seq_len(nocoloans)) {
  hc080_col <- hc080_columns[i]
  hc050_col <- hc050_columns[i]
  
  unsecured_data <- unsecured_data %>%
    mutate(
      unsecuredhmr = unsecuredhmr + if_else(get(hc050_col) == 1, get(hc080_col), 0), # to purchase or construct the HMR
      unsecuredoth = unsecuredoth + if_else(get(hc050_col) == 2, get(hc080_col), 0), # to purchase other real estate
      unsecuredren = unsecuredren + if_else(get(hc050_col) == 3, get(hc080_col), 0), # to refurbish or renovate the residence
      unsecuredveh = unsecuredveh + if_else(get(hc050_col) == 4, get(hc080_col), 0) # to buy a vehicle or other means of transport
    )
}

# Aggregate non-collateralized loans for real estate
unsecured_data <- unsecured_data %>%
  mutate(
    unsecuredhome = unsecuredhmr + unsecuredoth
  )

# Replace zero values with NA
unsecured_data <- unsecured_data %>%
  mutate(across(c(unsecuredhome, unsecuredhmr, unsecuredoth, unsecuredren, unsecuredveh), 
                ~ ifelse(. == 0, NA, .)))

# Select only relevant columns
unsecured_data <- unsecured_data %>%
  dplyr::select(id, starts_with("unsecured"), starts_with("hb0300"), starts_with("hb160"),
         starts_with("hb360"), starts_with("hb130"), starts_with("hb330"))

# Save the unsecured loans data as a CSV file
write.csv(unsecured_data, file.path(OUTPUT, paste0(DBNAME,"_unsecuredloans.csv")), row.names = FALSE)

################################################################################
# Load main dataset from preliminary operations and merge with occupational and unsecured loans
df <- read.csv(file.path(OUTPUT, paste0(DBNAME, "_orig.csv")))
df <- df %>%
  dplyr::select(-matches("occupational|unsecured|liquid|house|illiquid|norentIncome|htm|statushtm|biweeklyy|credit"))

# Merge with occupational data, keeping only matching rows
#occupational_data <- read.csv(file.path(OUTPUT, paste0(DBNAME,"_occupational.csv"))) %>%
#occupational_data <- occupational_data %>%
#  dplyr::select(id, occupational)#, couple)
df <- df %>%
  inner_join(occupational_data, by = "id")

# Merge with unsecured loans data, keeping only matching rows
#unsecured_data <- read.csv(file.path(OUTPUT, paste0(DBNAME,"_unsecuredloans.csv")))
df <- df %>%
  inner_join(unsecured_data, by = "id")

# Create assets, debt, and wealth components
df <- df %>%
  mutate(
    debtPayments = dl2000,
    finAssets = da2100,
    totDebt = dl1000,
    mortDebt = dl1100,
    liquidass = rowSums(dplyr::select(., da2101, da2102, da2103, da2105), na.rm = TRUE),
    liquidebt = rowSums(dplyr::select(., dl1210, dl1220), na.rm = TRUE),
    liquid = liquidass - liquidebt,
    house = rowSums(dplyr::select(., da1110, da1120), na.rm = TRUE),
    hasHouse = ifelse(da1110i == 1 | da1120i == 1, 1, 0)
  )

df <- df %>%
  mutate(
    if (illiquidef == 1) {
      illiquidass = rowSums(dplyr::select(., da2106, da2107, da2108, house, occupational, da1130, da1131, da2109, da1140), na.rm = TRUE)
      illiquidebt = rowSums(dplyr::select(., unsecuredhome, mortDebt), na.rm = TRUE)
    } else {
      illiquidass = rowSums(dplyr::select(., house, occupational, da2109, da1140), na.rm = TRUE)
      illiquidebt = rowSums(dplyr::select(., unsecuredhome, mortDebt), na.rm = TRUE)
    },
    illiquid = illiquidass - illiquidebt
  )

################################################################################
# HtM defined based on the gross income (old version)
#df <- df %>%
#  mutate(
#    norentIncome = rowSums(dplyr::select(., di1100, di1200, di1610, di1620, di1700, di1510, di1520), na.rm = TRUE),
#    biweeklyy = norentIncome / 24,
#    credit = norentIncome / 12,
#    htm = ifelse(liquid <= biweeklyy & liquid >= 0 | liquid <= (biweeklyy - credit) & liquid < 0, 1, 0),
#    poorhtm = ifelse(htm == 1 & illiquid <= 0, 1, 0),
#    tempHtM = ifelse(poorhtm == 1 & hasHouse == 1, 1, 0),
#    poorhtm = ifelse(tempHtM == 1, 0, poorhtm),
#    wealhtm = ifelse(htm == 1 & illiquid > 0 | tempHtM == 1, 1, 0),
#    nonhtm = ifelse(htm == 0, 1, 0),
#    statushtm = case_when(
#      poorhtm == 1 ~ 1,
#      wealhtm == 1 ~ 2,
#      htm == 0 ~ 3
#    )
#  )

# HtM defined based on the net income
df <- df %>%
  mutate(
    netIncome = di2001,
    biweeklyy = netIncome / 24,
    credit = netIncome / 12,
    htm = ifelse(liquid <= biweeklyy & liquid >= 0 | liquid <= (biweeklyy - credit) & liquid < 0, 1, 0),
    poorhtm = ifelse(htm == 1 & illiquid <= 0, 1, 0),
    tempHtM = ifelse(poorhtm == 1 & hasHouse == 1, 1, 0),
    poorhtm = ifelse(tempHtM == 1, 0, poorhtm),
    wealhtm = ifelse(htm == 1 & illiquid > 0 | tempHtM == 1, 1, 0),
    nonhtm = ifelse(htm == 0, 1, 0),
    statushtm = case_when(
      poorhtm == 1 ~ 1,
      wealhtm == 1 ~ 2,
      htm == 0 ~ 3
    )
  )

# Add top 10 status
if (top10Index == 1) {
  # Initialize the `top10` column to 0
  df <- df %>% mutate(top10 = 0)
  
  # Calculate the 90th percentile of net wealth for `non-HtM` within each `im0100`
  df <- df %>% 
    group_by(im0100) %>% 
    mutate(
      wealth_threshold_top10 = wtd.quantile(dn3001[statushtm == 3], weights = hw0010[statushtm == 3], probs = 0.85, na.rm = TRUE)
    ) %>% 
    ungroup()
  
  # Assign `top10 = 1` for households with `dn3001` greater than the 90th percentile
  df <- df %>% 
    mutate(
      top10 = ifelse(im0100 %in% unique(im0100) & statushtm == 3 & dn3001 > wealth_threshold_top10, 1, top10),
      statushtm = ifelse(top10 == 1, 4, statushtm),
      top10htm = ifelse(top10 == 1, 1, 0),
      nonhtm = ifelse(top10 == 1, 0, nonhtm)
    )
  # Remove unnecessary threshold
  df <- df %>% dplyr::select(-wealth_threshold_top10)
}

# Adjust statushtm based on the netIncome4HtMindex
#if (netIncome4HtMindex == 1) {
#  df <- df %>%
#    mutate(
#      statushtm = statushtm_net,
#      poorhtm = poorhtm_net,
#      wealhtm = wealhtm_net,
#      nonhtm = nonhtm_net
#    )
#}

################################################################################
# Save the dataset
write.csv(df, file.path(OUTPUT, paste0(DBNAME, "_orig.csv")), row.names = FALSE)
#write.xlsx(df, file.path(OUTPUT, paste0(DBNAME, "_orig_3status.xlsx")))

# Clean up temporary files
file.remove(file.path(OUTPUT, paste0(DBNAME,"_occupational.csv")), file.path(OUTPUT, paste0(DBNAME,"_unsecuredloans.csv")))