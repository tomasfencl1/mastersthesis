################################################################################
# NET INCOME APPROXIMATION w SELF-EMPLOYED TAXATION
# desc: based on the CZ laws 586/1992 Sb., 589/1992 Sb., 592/1992 Sb.
################################################################################

# Load packages
library(dplyr)

################################################################################
# Merge personal level datasets
file_list_pers <- paste0(HFCSDATA, "/P", 1:5, ".csv")
df_pers1 <- bind_rows(lapply(file_list_pers, function(file) {
  read.csv(file) %>%
    dplyr::select(hid, id, sa0100, sa0010, ra0010, im0100, pg0110, pg0210, pg0310, pg0410, pg0510)
}))
df_pers <- df_pers1

################################################################################
# Filter country
df_pers <- df_pers %>%
  filter(sa0100 == COUNTRY)

# Calc number of individuals in sample
df_pers <- df_pers %>%
  mutate(unique_pid = paste0(sa0010, ra0010))

num_distinct_pid <- df_pers %>%
  summarise(unique_ids = n_distinct(unique_pid)) %>%
  pull(unique_ids)

# Rename columns
df_pers <- df_pers %>%
  rename(id = hid, pid = id)

# Calculate taxable income (applying approximate deductions on self-employment income)
df_pers <- df_pers %>%
  mutate(
    taxable_empl = ifelse(is.na(pg0110) | pg0110 < 0, 0, pg0110),  # Employee taxable income
    taxable_self = ifelse(is.na(pg0210) | pg0210 < 0, 0, pg0210 * 2/3)  # Self-employment taxable income
  )

################################################################################
# Replicate tax schedule, according to the law 586/1992 Sb.

# Income tax threshold and rates
# as defined in https://www.zakonyprolidi.cz/cs/1992-586/zneni-20210101
avg_wage_m_CZK_2021 <- 35441 # product of 1.1 and 1.2 from source https://www.zakonyprolidi.cz/cs/2020-381
er_CZKEUR_2021 <- 25.645 # source https://www.cnb.cz/cs/financni-trhy/devizovy-trh/kurzy-devizoveho-trhu/kurzy-devizoveho-trhu/prumerne_mena.html?mena=EUR
avg_wage_m_EUR_2021 <- avg_wage_m_CZK_2021 / er_CZKEUR_2021 # Approximate monthly average wage in EUR
threshold1 <- 48 * avg_wage_m_EUR_2021
rate1 <- 0.15
rate2 <- 0.23

# Social and health insurance contributions, according to the laws 589/1992 Sb., 592/1992 Sb. 
# social defined in https://www.zakonyprolidi.cz/cs/1992-589/zneni-20210101
# health defined in https://www.zakonyprolidi.cz/cs/1992-592/zneni-20210101
social_empl <- 0.065
social_self <- 0.292
health_empl <- 0.045
health_self <- 0.135

# Flat-rate tax amount for self-employed, according to law 586/1992 Sb.
# as defined in https://www.zakonyprolidi.cz/cs/1992-586/zneni-20210101
flattax_m_CZK <- 5469 # source income tax 100 CZK defined in https://www.zakonyprolidi.cz/cs/1992-586/zneni-20210101
                          # social 2976 CZK (25 % of avg wage is the base, times the rate) defined in https://www.zakonyprolidi.cz/cs/1992-589/zneni-20210101
                          # health 2393 CZK (50 % of avg wage is the base, times the rate) defined in https://www.zakonyprolidi.cz/cs/1992-592/zneni-20210101
flattax_m_EUR <- flattax_m_CZK / er_CZKEUR_2021 # Approximate monthly flat tax in EUR
flattax_y_EUR <- flattax_m_EUR * 12

# Approximate the tax values
df_pers <- df_pers %>%
  mutate(
    # Employee tax calculation
    tax_empl = ifelse(taxable_empl <= threshold1,
                      taxable_empl * rate1,
                      threshold1 * rate1 + (taxable_empl - threshold1) * rate2),
    # Apply social & health insurance contributions
    tax_empl = tax_empl + (taxable_empl * social_empl) + (taxable_empl * health_empl),
    
    # Standard self-employment tax calculation (progressive)
    tax_self_STANDARD = ifelse(taxable_self <= threshold1,
                               taxable_self * rate1,
                               threshold1 * rate1 + (taxable_self - threshold1) * rate2),
    # Apply social & health insurance contributions
    tax_self_STANDARD = tax_self_STANDARD + (taxable_self * social_self) + (taxable_self * health_self),
    
    # Flat-rate tax for self-employment
    tax_self_FLAT = flattax_y_EUR
  )

################################################################################
# Tax credit (i.e. sleva na poplatníka)
# as defined in https://www.zakonyprolidi.cz/cs/1992-586/zneni-20210101
taxcredit_a_CZK_2021 <- 27840
taxcredit_a_EUR_2021 <- taxcredit_a_CZK_2021 / er_CZKEUR_2021

# Apply tax credit
df_pers <- df_pers %>%
  mutate(
    # Apply tax credit to employee tax first
    tax_empl_adj = pmax(tax_empl - taxcredit_a_EUR_2021, 0),
    
    # Remaining tax credit amount after applying to employee tax
    residual_credit = pmax(taxcredit_a_EUR_2021 - tax_empl, 0),
    
    # Apply remaining tax credit to standard self-employment tax
    tax_self_STANDARD_adj = pmax(tax_self_STANDARD - residual_credit, 0)
  )

################################################################################
# Flat-rate tax scheme eligibility, according to the law 586/1992 Sb., and rational choice
# as defined in https://www.zakonyprolidi.cz/cs/1992-586/zneni-20210101 
flattax_cap_CZK <- 1000000  # Maximum allowed revenue for flat-rate taxation
flattax_cap_EUR <- flattax_cap_CZK / er_CZKEUR_2021

# Check eligibility and individual's choice under rationality
df_pers <- df_pers %>%
  mutate(
    # Check eligibility for flat-rate tax
    flattax_eligible = (taxable_empl == 0) & ((taxable_self * 3/2) <= flattax_cap_EUR),
    
    # Choose tax scheme that results in lower tax burden
    tax_self_adj = ifelse(flattax_eligible & tax_self_STANDARD_adj > tax_self_FLAT,
                      tax_self_FLAT, tax_self_STANDARD_adj)
  )

################################################################################
# Total tax burden at the individual level
df_pers <- df_pers %>%
  mutate(
    tax = tax_empl_adj + tax_self_adj
  )

#View(df_pers)

################################################################################
# Aggregate tax to household level and compute net income
df_pers <- df_pers %>%
  group_by(id, sa0100, sa0010, im0100) %>%
  summarise(
    pg0110 = sum(pg0110, na.rm = TRUE),
    pg0210 = sum(pg0210, na.rm = TRUE),
    taxable_empl = sum(taxable_empl, na.rm = TRUE),
    taxable_self = sum(taxable_self, na.rm = TRUE),
    tax_empl = sum(tax_empl, na.rm = TRUE),
    tax_self_STANDARD = sum(tax_self_STANDARD, na.rm = TRUE),
    tax_self_FLAT = sum(tax_self_FLAT, na.rm = TRUE),
    tax_empl_adj = sum(tax_empl_adj, na.rm = TRUE),
    tax_self_adj = sum(tax_self_adj, na.rm = TRUE),
    tax = sum(tax, na.rm = TRUE)
  ) %>%
  ungroup()

# Drop duplicates based on 'id' to ensure each household appears only once
df_pers <- df_pers %>%
  distinct(id, .keep_all = TRUE)

# Keep only relevant columns
df_pers <- df_pers %>%
  dplyr::select(id, pg0110, pg0210, taxable_empl, taxable_self, tax_empl, tax_self_STANDARD,
         tax_self_FLAT, tax_empl_adj, tax_self_adj, tax)

################################################################################
# Merge household level datasets
file_list_hh <- paste0(HFCSDATA, "/D", 1:5, ".csv")
df_hh1 <- bind_rows(lapply(file_list_hh, function(file) {
  read.csv(file) %>%
    dplyr::select(id, sa0100, sa0010, im0100, di2000, di1100, di1200, di1300, 
           di1400, di1500, di1600, di1700, di1800, dh0001, hw0010)
}))
df_hh <- df_hh1

# Filter country
df_hh <- df_hh %>%
  filter(sa0100 == COUNTRY)

# Merge the aggregated personal level dataset into household level dataset
df <- df_hh %>%
  inner_join(df_pers, by = "id")

################################################################################
# Calculate total transfers
df <- df %>%
  mutate(transfers = rowSums(dplyr::select(., di1300, di1400, di1500, di1600, di1700, di1800), na.rm = TRUE)) %>%
  mutate(transfers = ifelse(is.na(transfers), 0, transfers))

# Approximate net income
df <- df %>%
  mutate(netinc = ifelse(is.na(di1100), 0, di1100) + ifelse(is.na(di1200), 0, di1200) - tax + transfers)

################################################################################
# Keep only necessary variables
df <- df %>%
  dplyr::select(id, sa0100, sa0010, im0100, hw0010, netinc, taxable_empl, taxable_self,
         tax_empl, tax_self_STANDARD, tax_self_FLAT, tax_empl_adj, tax_self_adj, 
         tax, transfers)

# Save the output
write.csv(df, file.path(NETINCDATA, paste0(DBNAME, "_netIncome.csv")), row.names = FALSE)

################################################################################
# Summary statistics
num_distinct_hid <- df %>%
  summarise(unique_ids = n_distinct(sa0010)) %>%
  pull(unique_ids)

# Mean annual CZK per capita net income
#weighted.mean(df$netinc, df$hw0010, na.rm = TRUE) * er_CZKEUR_2021 / (num_distinct_pid / num_distinct_hid)