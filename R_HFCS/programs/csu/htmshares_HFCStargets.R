################################################################################
# HtM SHARES HFCS TARGETS
# desc: calculates p/w/n-HtM (and top10) shares for targets in CSU imputation
################################################################################

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(openxlsx)

################################################################################
# Load dataset
df_htm <- read.csv(file.path(OUTPUT, paste0(DBNAME, "_orig.csv")))

################################################################################
# Breakdown of all HtM types
s_all <- df_htm %>%
  mutate(aux = 1) %>%  # Equivalent to "gen aux = 1"
  group_by(sa0100, statushtm, im0100) %>%
  summarise(weighted_count = sum(hw0010 * aux, na.rm = TRUE), .groups = "drop") %>%  # Collapse sum by sa0100, statushtm, im0100
  group_by(sa0100, im0100) %>%
  mutate(total = sum(weighted_count, na.rm = TRUE)) %>%  # Total for each (sa0100, im0100)
  ungroup() %>%
  mutate(share = (weighted_count / total) * 100) %>%  # Calculate share as percentage
  group_by(sa0100, statushtm) %>%
  summarise(share = mean(share, na.rm = TRUE), .groups = "drop") %>%  # Collapse across implicates
  pivot_wider(names_from = statushtm, values_from = share, values_fill = 0) %>%  # Wide format
  rename(
    pHtM = `1`,
    wHtM = `2`,
    nHtM = `3`,
    !!!if (top10Index == 1) list(top10 = sym("4")) else NULL  # Include top10 only if needed
  )

# Export breakdown to Excel
#wb <- createWorkbook()
#addWorksheet(wb, "breakdown")
#writeData(wb, "breakdown", s_all, startRow = 1)
#saveWorkbook(wb, file.path(TABLES, paste0(DBNAME,"_imputation_targets.xlsx")), overwrite = TRUE)

################################################################################
# Breakdown of HtM vs Non-HtM
s_htm <- df_htm %>%
  mutate(htm = if_else(statushtm %in% c(1, 2), 1, 0), aux = 1) %>%
  group_by(sa0100, htm, im0100) %>%
  summarise(weighted_count = sum(hw0010 * aux, na.rm = TRUE), .groups = "drop") %>%
  group_by(sa0100, im0100) %>%
  mutate(total = sum(weighted_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(share = (weighted_count / total) * 100) %>%
  group_by(sa0100, htm) %>%
  summarise(share = mean(share, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = htm, values_from = share, values_fill = 0) %>%
  rename(nHtM = `0`, HtM = `1`)

################################################################################
# Breakdown of Poor HtM vs Wealthy HtM
s_phtm <- df_htm %>%
  filter(statushtm %in% c(1, 2)) %>%
  mutate(phtm = if_else(statushtm == 1, 1, 0), aux = 1) %>%
  group_by(sa0100, phtm, im0100) %>%
  summarise(weighted_count = sum(hw0010 * aux, na.rm = TRUE), .groups = "drop") %>%
  group_by(sa0100, im0100) %>%
  mutate(total = sum(weighted_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(share = (weighted_count / total) * 100) %>%
  group_by(sa0100, phtm) %>%
  summarise(share = mean(share, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = phtm, values_from = share, values_fill = 0) %>%
  rename(wHtM = `0`, pHtM = `1`)

################################################################################
# Breakdown of Non-HtM vs Top 10% Non-HtM
if (top10Index == 1) {
  s_t10nhtm <- df_htm %>%
    filter(statushtm %in% c(3, 4)) %>%
    mutate(t15nhtm = if_else(statushtm == 4, 1, 0), aux = 1) %>%
    group_by(sa0100, t15nhtm, im0100) %>%
    summarise(weighted_count = sum(hw0010 * aux, na.rm = TRUE), .groups = "drop") %>%
    group_by(sa0100, im0100) %>%
    mutate(total = sum(weighted_count, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(share = (weighted_count / total) * 100) %>%
    group_by(sa0100, t15nhtm) %>%
    summarise(share = mean(share, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = t15nhtm, values_from = share, values_fill = 0) %>%
    rename(nHtM = `0`, top10 = `1`)
}

################################################################################
# Income Shares by HtM Status
s_inc_all <- df_htm %>%
  mutate(aux = 1) %>%  # Equivalent to "gen aux = 1"
  group_by(sa0100, statushtm, im0100) %>%
  summarise(income_sum = sum(di2001 * hw0010 * aux, na.rm = TRUE), .groups = "drop") %>%
  group_by(sa0100, im0100) %>%
  mutate(total_income = sum(income_sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(inc_sh = income_sum / total_income) %>%
  group_by(sa0100, statushtm) %>%
  summarise(inc_sh = mean(inc_sh, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = statushtm, values_from = inc_sh, values_fill = 0) %>%
  rename(
    incs_pHtM = `1`,
    incs_wHtM = `2`,
    incs_nHtM = `3`,
    !!!if (top10Index == 1) list(incs_top10 = sym("4")) else NULL
  )

################################################################################
# Save to CSV
write.csv(s_all, file.path(OUTPUT, paste0("csu/",DBNAME,"_HtM_shares.csv")), row.names = FALSE)
write.csv(s_htm, file.path(OUTPUT, paste0("csu/",DBNAME,"_HtM_nHtM_shares.csv")), row.names = FALSE)
write.csv(s_phtm, file.path(OUTPUT, paste0("csu/",DBNAME,"_pHtM_wHtM_shares.csv")), row.names = FALSE)
if (top10Index == 1) {
  write.csv(s_t10nhtm, file.path(OUTPUT, paste0("csu/",DBNAME,"_nHtM_top10_shares.csv")), row.names = FALSE)
}
write.csv(s_inc_all, file.path(OUTPUT, paste0("csu/",DBNAME,"_HtM_inc_shares.csv")), row.names = FALSE)
