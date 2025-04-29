################################################################################
# NET INCOME APPROXIMATION 
# desc: based on the Slacalek et. al. (2020) replication of DIW working paper 2017
################################################################################

# Merge datasets
file_list <- paste0(HFCSDATA, "/D", 1:5, ".csv")
df <- bind_rows(lapply(file_list, function(file) {
  read.csv(file) %>%
    select(id, sa0100, sa0010, im0100, di2000, di1100, di1200, di1300, 
           di1400, di1500, di1600, di1700, di1800, dh0001, hw0010)
}))

#write_xlsx(df, file.path(DATA, "netincome", paste0(COUNTRY, "netIncome.xlsx")))
################################################################################
# Replace negative values in di2000 with NA
df <- df %>%
  mutate(
    di2000 = as.numeric(di2000),          # Convert to numeric; non-numeric values become NA
    di2000 = ifelse(di2000 < 0, NA, di2000) # Replace negative values with NA
  )
# Create tax and auxiliary variables
df <- df %>%
  mutate(#taxa = NA,
         #taxb = NA,
         #taxHelpa0 = 0,
         #taxHelpb0 = 0,
         aux = di1200 * 2 / 3)

# Compute taxable income versions
df <- df %>%
  mutate(taxablea = rowSums(cbind(di1100, aux), na.rm = TRUE),
         taxableb = ifelse(is.na(di1100), 0, di1100))

# Replace taxablea and taxableb with zero if they are negative
df <- df %>%
  mutate(
    taxablea = ifelse(taxablea < 0, 0, taxablea),
    taxableb = ifelse(taxableb < 0, 0, taxableb)
  )

# Filter country
df <- df %>%
  filter(sa0100 == COUNTRY)

# Calculate total transfers
df <- df %>%
  mutate(across(c(di1300, di1400, di1500, di1600, di1700, di1800), as.numeric)) %>%
  mutate(
    transfers = rowSums(select(., di1300, di1400, di1500, di1600, di1700, di1800), na.rm = TRUE),
    transfers = ifelse(is.na(transfers), 0, transfers)
  )

#df <- df %>%
#  mutate(transfers = rowSums(select(., di1300, di1400, di1500, di1600, di1700, di1800), na.rm = TRUE)) %>%
#  mutate(transfers = ifelse(is.na(transfers), 0, transfers))

# Clean taxes
#df <- df %>%
#  mutate(
#    taxa = ifelse(is.na(taxa) & !is.na(taxablea) | taxa < 0, 0, taxa),
#    taxb = ifelse(is.na(taxb) & !is.na(taxableb) | taxb < 0, 0, taxb)
#  )

# Calculate net income
#df <- df %>%
#  mutate(
#    di2001a = taxablea - taxa + transfers,
#    di2001b = taxableb - taxb + transfers
#  )

################################################################################
# Personal allowances ??????????

# The personal allowances are commented out as in the original code
# Subtract personal allowances if available; replace missing values with 0

################################################################################
# Approximation of German taxrates based on DIW working paper 2017
# use tax brackets as in OECD data
# below 8130: no taxes
# between 8130 and 13468: linear increase from 0.14 to 0.23
# between 13469 and 52881: linear increase from 0.23 to 0.42
# above 52881 marginal tax rate of 0.42

# Create marginal rates for the first tax bracket
MR1 <- tibble(
  di2000r = 8130:(8130 + 5338),
  MR = 0.14 + seq(0, 0.09, length.out = 5339)
)

# Save MR1 to a CSV (adjust to desired file path)
write.csv(MR1, file.path(OUTPUT, "MR1.csv"))

# Create marginal rates for the second tax bracket
MR2 <- tibble(
  di2000r = 13469:(13469 + 39412),
  MR = 0.23 + seq(0, 0.19, length.out = 39413)
)

# Save MR2 to a CSV
write.csv(MR2, file.path(OUTPUT, "MR2.csv"))

# Comprehensive marginal tax rate dataset
max_income <- max(as.numeric(df$di2000), na.rm = TRUE)
all_income <- tibble(di2000r = 0:max_income)

# Merge MR1 and MR2 with the comprehensive dataset
all_income <- all_income %>%
  left_join(MR1, by = "di2000r") %>%
  left_join(MR2, by = "di2000r") %>%
  mutate(
    MR = case_when(
      !is.na(MR.x) ~ MR.x,
      !is.na(MR.y) ~ MR.y,
      di2000r < 8130 ~ 0,
      di2000r > 52881 ~ 0.42
    )
  ) %>%
  select(di2000r, MR)

# Round di2000r to integers
all_income <- all_income %>%
  mutate(di2000r = round(di2000r))

# Calculate cumulative tax based on marginal rates
all_income <- all_income %>%
  mutate(tax = cumsum(MR))

# Finalize the tax dataset as two versions (for with and without self-employment income)
tax_data_a <- all_income %>%
  rename(taxablear = di2000r, MRa = MR, taxa = tax)

tax_data_b <- all_income %>%
  rename(taxablebr = di2000r, MRb = MR, taxb = tax)

# Save the tax datasets
#write.csv(tax_data_a, file.path(OUTPUT, "tax/totala.csv"), row.names = FALSE)
#write.csv(tax_data_b, file.path(OUTPUT, "tax/totalb.csv"), row.names = FALSE)

################################################################################
# Merge German Marginal Tax Rates with Data
# Round taxable income for matching with tax brackets
df <- df %>%
  mutate(
    taxablear = round(taxablea),
    taxablebr = round(taxableb)
  )

# Load the tax datasets
#tax_data_a <- read.csv(file.path(OUTPUT, "tax/totala.csv"))
#tax_data_b <- read.csv(file.path(OUTPUT, "tax/totalb.csv"))

# Merge marginal tax rates for taxablear and taxablebr
df <- df %>%
  left_join(tax_data_a, by = "taxablear") %>%
  left_join(tax_data_b, by = "taxablebr")

################################################################################
# Calculate Net Income
df <- df %>%
  mutate(di2001a = taxablea - taxa + transfers,
         di2001b = taxableb - taxb + transfers)  # Net income without self-employed income

################################################################################
# Save Output
df <- df %>%
  select(id, sa0100, sa0010, im0100, hw0010, di2001a, di2001b, taxa, taxb, transfers, taxablea, taxableb)

# Save the output as a CSV file
write.csv(df, file.path(DATA, "netincome", paste0(DBNAME, "netIncome.csv")), row.names = FALSE)
#write_xlsx(df, file.path(DATA, "netincome", paste0(COUNTRY, "netIncome.xlsx")))
