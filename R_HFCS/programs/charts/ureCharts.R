################################################################################
# URE CHARTS
# desc: creates charts and decomp. of NIRE, NNP, housing and stock market wealth
################################################################################

# Load packages
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)
library(dplyr)

################################################################################
# Load dataset
df1 <- read.csv(file.path(OUTPUT, paste0(DBNAME, "_orig.csv")))

################################################################################
# Handle missing data
df <- df1 %>%
  filter(statushtm >= 0, !is.na(statushtm)) %>%
  mutate(
    house = ifelse(is.na(house), 0, house),
    da2105 = ifelse(is.na(da2105), 0, da2105),
    da1140 = ifelse(is.na(da1140), 0, da1140)
  )

################################################################################
# Set richIndex based on top10Index
richIndex <- ifelse(top10Index == 1, 4, 3)

# Adjust stock market wealth for top10
df <- df %>%
  mutate(
    da2105_temp = ifelse(statushtm >= richIndex, da2105, 0),
    stockScalingFac = 1  # Default scaling factor
  )

################################################################################
# Stock market coverage adjustment
# Country level weighted sum of HMR value, outstanding mortgage debt and stock market wealth
df_hh <- df %>%
  group_by(sa0010) %>%
  summarise(across(c(house, da1400i, dl1100, dl1100i, da2105, da2105i, hw0010), ~ weighted.mean(.x, hw0010, na.rm = TRUE)))

# Average CZK/EUR exchange rate for 2021
er_CZKEUR_2021 <- 25.645 # source https://www.cnb.cz/cs/financni-trhy/devizovy-trh/kurzy-devizoveho-trhu/kurzy-devizoveho-trhu/prumerne_mena.html?mena=EUR

#colSums(df_hh[, c("house", "dl1100", "da2105")] * df_hh$hw0010 * er_CZKEUR_2021 / 1000000000, na.rm = TRUE)
stockmarketvalue_2021_bilion_CZK_hfcs <- colSums(df_hh[, c("house", "dl1100", "da2105")] * df_hh$hw0010 * er_CZKEUR_2021 / 1000000000, na.rm = TRUE)[3]
#colSums(df_hh[, c("da2105i")] * df_hh$hw0010, na.rm = TRUE) / sum(df_hh$hw0010) # % of hh with stock market wealth
#colSums(df_hh[, c("da1400i")] * df_hh$hw0010, na.rm = TRUE) / sum(df_hh$hw0010) # % of hh with real estate wealth

# Stock market value aggregate
stockmarketvalue_toGDP_2021 <- 0.0172249140573786 # source https://data.worldbank.org/indicator/CM.MKT.TRAD.GD.ZS?locations=CZ
GDP_2021_CZK <- 6306061000000 # source https://www.kurzy.cz/svet/ceska-republika/hdp/
stockmarketvalue_2021_bilion_CZK_agg <- stockmarketvalue_toGDP_2021 * GDP_2021_CZK / 1000000000 

# Stock market coverage ratio
#stockmarketvalue_2021_bilion_CZK_hfcs / stockmarketvalue_2021_bilion_CZK_agg

# Calculate adjusted stock market wealth
df <- df %>%
  mutate(
    stockScalingFac = case_when(
      statushtm == richIndex ~ stockCoverage_CZ * (sum(da2105_temp * hw0010, na.rm = TRUE) / sum(da2105 * hw0010, na.rm = TRUE)),
      TRUE ~ stockScalingFac
    ),
    da2105adj = da2105 / stockScalingFac
  )

# Apply adjustment if enabled
if (blowupStockMarketIndex == 1) {
  df <- df %>% mutate(da2105 = da2105adj)
}

################################################################################
# Collapse dataset (mean weighted values)
df_summary1 <- df %>%
  group_by(sa0100, statushtm, im0100) %>%
  summarise(
    ure = weighted.mean(ure, hw0010, na.rm = TRUE),
    ureyc = weighted.mean(ureyc, hw0010, na.rm = TRUE),
    grossIncome = weighted.mean(as.numeric(grossIncome), hw0010, na.rm = TRUE),
    house = weighted.mean(house, hw0010, na.rm = TRUE),
    da2105 = weighted.mean(da2105, hw0010, na.rm = TRUE),
    da1140 = weighted.mean(da1140, hw0010, na.rm = TRUE),
    nnp = weighted.mean(nnp, hw0010, na.rm = TRUE),
    .groups = "drop"
  )

# Collapse to sa0100, statushtm, averaging across im0100
df_summary2 <- df_summary1 %>%
  group_by(sa0100, statushtm) %>%
  summarise(
    ure = mean(ure, na.rm = TRUE),
    ureyc = mean(ureyc, na.rm = TRUE),
    grossIncome = mean(grossIncome, na.rm = TRUE),
    house = mean(house, na.rm = TRUE),
    da2105 = mean(da2105, na.rm = TRUE),
    da1140 = mean(da1140, na.rm = TRUE),
    nnp = mean(nnp, na.rm = TRUE),
    .groups = "drop"
  )

# Divide values by 1000 and round to 0 decimal places
df_summary3 <- df_summary2 %>%
  mutate(
    across(c(ure, ureyc, grossIncome, house, da2105, da1140, nnp), ~ .x / 1000)#round(.x / 1000, 1))
  )

# Use ure as ureyc for HtM households
df_summary3 <- df_summary3 %>%
  mutate(ureyc = ifelse(is.na(ureyc), ure, ureyc))

# Replace NA's with 0
df_summary3[is.na(df_summary3)] <- 0

# Add HtM labels for plotting
df_summary3 <- df_summary3 %>%
  mutate(
    HtMstatus = factor(
      statushtm,
      levels = c(1, 2, 3, 4),
      labels = c("Poor HtM", "Wealthy HtM", "Non-HtM", "Top 10")
    )
  )

# See summary dataframe
#View(df_summary3)

################################################################################
# Plot Net Interest Rate Exposure
NIRE_plot <- ggplot(df_summary3, aes(x = COUNTRY, y = ureyc, fill = HtMstatus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.9) +
  geom_text(
    aes(
      label = sprintf("%.1f", ureyc),
      vjust = ifelse(ureyc < 0, 1.5, -0.5)  # Below for negative values, above for positive
    ),
    position = position_dodge(width = 0.95),
    size = 6,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Poor HtM" = "#004488",
      "Wealthy HtM" = "#88CCEE",
      "Non-HtM" = "#FFA500",
      if (top10Index == 1) list("Top 10" = "#D55E00")
    )
  ) +
  labs(
    title = "Net interest rate exposures",
    subtitle = "by hand-to-mouth status, mean, EUR thousands",
    x = "",
    y = "Mean EUR thousands",
    fill = ""
  ) +
  coord_cartesian(ylim = c(min(df_summary3$ureyc) - (max(df_summary3$ureyc)-min(df_summary3$ureyc))*0.04, max(df_summary3$ureyc) + (max(df_summary3$ureyc)-min(df_summary3$ureyc))*0.04)) +  # Extend Y-axis limits
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0),
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10),
    panel.grid.major.x = element_blank() 
  )

# Add annotation
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ". Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 14, color = "gray40"
  )
NIRE_plot <- plot_grid(NIRE_plot, annotation, ncol = 1, rel_heights = c(1, 0.05)) # Maintain relative heights
#NIRE_plot

################################################################################
# Plot Net Nominal Positions
NNP_plot <- ggplot(df_summary3, aes(x = COUNTRY, y = nnp, fill = HtMstatus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.9) +
  geom_text(
    aes(
      label = sprintf("%.1f", nnp),
      vjust = ifelse(nnp < 0, 1.5, -0.5)  # Below for negative values, above for positive
    ),
    position = position_dodge(width = 0.95),
    size = 6,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Poor HtM" = "#004488",
      "Wealthy HtM" = "#88CCEE",
      "Non-HtM" = "#FFA500",
      if (top10Index == 1) list("Top 10" = "#D55E00")
    )
  ) +
  labs(
    title = "Net nominal positions",
    subtitle = "by hand-to-mouth status, mean, EUR thousands",
    x = "",
    y = "Mean EUR thousands",
    fill = ""
  ) +
  coord_cartesian(ylim = c(min(df_summary3$nnp) - (max(df_summary3$nnp)-min(df_summary3$nnp))*0.04, max(df_summary3$nnp) + (max(df_summary3$nnp)-min(df_summary3$nnp))*0.04)) +  # Extend Y-axis limits
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0),
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10),
    panel.grid.major.x = element_blank() 
  )

# Add annotation
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ". Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 14, color = "gray40"
  )
NNP_plot <- plot_grid(NNP_plot, annotation, ncol = 1, rel_heights = c(1, 0.05)) # Maintain relative heights
#NNP_plot

################################################################################
# Plot Housing Wealth
HousingWealth_plot <- ggplot(df_summary3, aes(x = COUNTRY, y = house, fill = HtMstatus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.9) +
  geom_text(
    aes(
      label = sprintf("%.1f", house),
      vjust = ifelse(house < 0, 1.5, -0.5)  # Below for negative values, above for positive
    ),
    position = position_dodge(width = 0.95),
    size = 6,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Poor HtM" = "#004488",
      "Wealthy HtM" = "#88CCEE",
      "Non-HtM" = "#FFA500",
      if (top10Index == 1) list("Top 10" = "#D55E00")
    )
  ) +
  labs(
    title = "Housing wealth",
    subtitle = "by hand-to-mouth status, mean, EUR thousands",
    x = "",
    y = "Mean EUR thousands",
    fill = ""
  ) +
  coord_cartesian(ylim = c(min(df_summary3$house) - (max(df_summary3$house)-min(df_summary3$house))*0.02, max(df_summary3$house) + (max(df_summary3$house)-min(df_summary3$house))*0.04)) +  # Extend Y-axis limits
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0),
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10),
    panel.grid.major.x = element_blank() 
  )

# Add annotation
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ". Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 14, color = "gray40"
  )
HousingWealth_plot <- plot_grid(HousingWealth_plot, annotation, ncol = 1, rel_heights = c(1, 0.05)) # Maintain relative heights
#HousingWealth_plot

################################################################################
# Plot Stock Market Wealth
StockMarketWealth_plot <- ggplot(df_summary3, aes(x = COUNTRY, y = da2105, fill = HtMstatus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.9) +
  geom_text(
    aes(
      label = sprintf("%.1f", da2105),
      vjust = ifelse(da2105 < 0, 1.5, -0.5)  # Below for negative values, above for positive
    ),
    position = position_dodge(width = 0.95),
    size = 6,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Poor HtM" = "#004488",
      "Wealthy HtM" = "#88CCEE",
      "Non-HtM" = "#FFA500",
      if (top10Index == 1) list("Top 10" = "#D55E00")
    )
  ) +
  labs(
    title = "Stock market wealth",
    subtitle = "by hand-to-mouth status, mean, EUR thousands",
    x = "",
    y = "Mean EUR thousands",
    fill = ""
  ) +
  coord_cartesian(ylim = c(min(df_summary3$da2105) - (max(df_summary3$da2105)-min(df_summary3$da2105))*0.02, max(df_summary3$da2105) + (max(df_summary3$da2105)-min(df_summary3$da2105))*0.04)) +  # Extend Y-axis limits
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0),
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10),
    panel.grid.major.x = element_blank() 
  )

# Add annotation
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ". Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 14, color = "gray40"
  )
StockMarketWealth_plot <- plot_grid(StockMarketWealth_plot, annotation, ncol = 1, rel_heights = c(1, 0.05)) # Maintain relative heights
#StockMarketWealth_plot

################################################################################
# Save plots
ggsave(paste0(DBNAME, "_NIRE_plot.png"), plot = NIRE_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)
ggsave(paste0(DBNAME, "_NNP_plot.png"), plot = NNP_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)
ggsave(paste0(DBNAME, "_HousingWealth_plot.png"), plot = HousingWealth_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)
ggsave(paste0(DBNAME, "_StockMarketWealth_plot.png"), plot = StockMarketWealth_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)

################################################################################
# Prepare variables for decomposition
df <- df1 %>%
  mutate(savings = YC) %>%
  mutate(across(c(da2100, da2101, da2102, da2103, da2104, da2105, da2106, 
                  da2107, da2108, da2109, dl1000, dl1200, dl1210, dl1220,
                  dl1231, dl1232, dl1110, dl1110a, dl1120, dl1120a), ~ replace_na(.x, 0)))
  #mutate(across(c(da2100, da2101, da2102, da2104, da2105, da2109,  
  #                dl1000, dl1200, dl1110a, dl1120a), ~ replace_na(.x, 0)))

# Compute weighted means at the individual level
df_summary1 <- df %>%
  group_by(sa0100, statushtm, im0100) %>%
  summarise(
    ure = weighted.mean(ure, hw0010, na.rm = TRUE),
    ureyc = weighted.mean(ureyc, hw0010, na.rm = TRUE),
    savings = weighted.mean(savings, hw0010, na.rm = TRUE),
    nnp = weighted.mean(nnp, hw0010, na.rm = TRUE),
    da2100 = weighted.mean(da2100, hw0010, na.rm = TRUE),
    da2101 = weighted.mean(da2101, hw0010, na.rm = TRUE),
    da2102 = weighted.mean(da2102, hw0010, na.rm = TRUE),
    da2103 = weighted.mean(da2103, hw0010, na.rm = TRUE),
    da2104 = weighted.mean(da2104, hw0010, na.rm = TRUE),
    da2105 = weighted.mean(da2105, hw0010, na.rm = TRUE),
    da2106 = weighted.mean(da2106, hw0010, na.rm = TRUE),
    da2107 = weighted.mean(da2107, hw0010, na.rm = TRUE),
    da2108 = weighted.mean(da2108, hw0010, na.rm = TRUE),
    da2109 = weighted.mean(da2109, hw0010, na.rm = TRUE),
    dl1000 = weighted.mean(dl1000, hw0010, na.rm = TRUE),
    dl1200 = weighted.mean(dl1200, hw0010, na.rm = TRUE),
    dl1210 = weighted.mean(dl1210, hw0010, na.rm = TRUE),
    dl1220 = weighted.mean(dl1220, hw0010, na.rm = TRUE),
    dl1231 = weighted.mean(dl1231, hw0010, na.rm = TRUE),
    dl1232 = weighted.mean(dl1232, hw0010, na.rm = TRUE),
    dl1110 = weighted.mean(dl1110, hw0010, na.rm = TRUE),
    dl1110a = weighted.mean(dl1110a, hw0010, na.rm = TRUE),
    dl1120 = weighted.mean(dl1120, hw0010, na.rm = TRUE),
    dl1120a = weighted.mean(dl1120a, hw0010, na.rm = TRUE),
    .groups = "drop"
  )

# Collapse to sa0100, statushtm, averaging across im0100
df_summary2 <- df_summary1 %>%
  group_by(sa0100, statushtm) %>%
  summarise(
    ure = mean(ure, na.rm = TRUE),
    ureyc = mean(ureyc, na.rm = TRUE),
    savings = mean(savings, na.rm = TRUE),
    nnp = mean(nnp, na.rm = TRUE),
    da2100 = mean(da2100, na.rm = TRUE),
    da2101 = mean(da2101, na.rm = TRUE),
    da2102 = mean(da2102, na.rm = TRUE),
    da2103 = mean(da2103, na.rm = TRUE),
    da2104 = mean(da2104, na.rm = TRUE),
    da2105 = mean(da2105, na.rm = TRUE),
    da2106 = mean(da2106, na.rm = TRUE),
    da2107 = mean(da2107, na.rm = TRUE),
    da2108 = mean(da2108, na.rm = TRUE),
    da2109 = mean(da2109, na.rm = TRUE),
    dl1000 = mean(dl1000, na.rm = TRUE),
    dl1200 = mean(dl1200, na.rm = TRUE),
    dl1210 = mean(dl1210, na.rm = TRUE),
    dl1220 = mean(dl1220, na.rm = TRUE),
    dl1231 = mean(dl1231, na.rm = TRUE),
    dl1232 = mean(dl1232, na.rm = TRUE),
    dl1110 = mean(dl1110, na.rm = TRUE),
    dl1110a = mean(dl1110a, na.rm = TRUE),
    dl1120 = mean(dl1120, na.rm = TRUE),
    dl1120a = mean(dl1120a, na.rm = TRUE),
    .groups = "drop"
  )

# Divide values by 1000
df_summary3 <- df_summary2 %>%
  mutate(
    across(c(ure, ureyc, savings, nnp, da2100, da2101, da2102, da2103, da2104,
             da2105, da2106, da2107, da2108, da2109, dl1000, dl1200, dl1210,
             dl1220, dl1231, dl1232, dl1110, dl1110a, dl1120, dl1120a), ~ .x / 1000)
  )

# Use ure as ureyc for HtM households
df_summary3 <- df_summary3 %>%
  mutate(ureyc = ifelse(is.na(ureyc), ure, ureyc))

# Add HtM labels for plotting
df_summary3 <- df_summary3 %>%
  mutate(
    HtMstatus = factor(
      statushtm,
      levels = c(1, 2, 3, 4),
      labels = c("Poor HtM", "Wealthy HtM", "Non-HtM", "Top 10")
    )
  )

# Convert HtMstatus to numeric for lines positioning
df_summary3 <- df_summary3 %>%
  mutate(HtMstatus_numeric = as.numeric(HtMstatus))

# See summary dataframe
#View(df_summary3)

# Define color palette with grouped shades
colors <- c(
  "da2100" = "#1F78B4",   # Strong Blue
  "da2101" = "#A6CEE3",   # Light Blue
  "da2102" = "#6AABD2",   # Medium Light Blue
  "da2103" = "#5599CC",   # Medium Blue
  "da2104" = "#3E85C6",   # Medium Dark Blue
  "da2105" = "#2B6BB0",   # Dark Blue
  "da2106" = "#1C559D",   # Deeper Blue
  "da2107" = "#144A8A",   # Even Darker Blue ("#98FB98" lime green)
  "da2108" = "#0E3F77",   # Darkest Blue
  "da2109" = "#083366",   # Near Black Blue
  "dl1200" = "#E31A1C",   # Bright Red
  "dl1210" = "#FC9272",   # Light Red
  "dl1220" = "#FB6A4A",   # Medium Light Red
  "dl1231" = "#DE2D26",   # Medium Red
  "dl1232" = "#A50F15",   # Dark Red
  "dl1110" = "#6A3D9A",   # Deep Purple
  "dl1110a" = "#6A3D9A",
  "dl1120" = "#8E6BAF",   # Soft Purple
  "dl1120a" = "#8E6BAF",
  "dl1000" = "#D55E00", #"#B15928",    # Orange
  "savings" = "#FFA500" #"#33A02C"    # Yellow
)

################################################################################
# Decompose Net Interest Rate Exposure
# Transform data to long format for ggplot
df_long <- df_summary3 %>%
  mutate(
    #da2100 = da2100 * 0.25,
    #da2101 = da2101 * 0.75,
    #da2104 = -da2104 * 0.25,
    #da2109 = -da2109 * 0.25,
    da2101 = da2101,
    da2102 = da2102 * 0.25,
    da2103 = da2103 * 0.25,
    da2105 = da2105 * 0.25,
    da2106 = da2106 * 0.25,
    da2107 = da2107 * 0.25,
    da2108 = da2108 * 0.25,
    #dl1200 = -dl1200,
    dl1210 = -dl1210,
    dl1220 = -dl1220,
    dl1231 = -dl1231,
    dl1232 = -dl1232,
    dl1110a = -dl1110a,
    dl1120a = -dl1120a) %>%
  #dplyr::select(HtMstatus, ureyc, savings, da2100, da2101, da2104, da2109, 
  #       dl1200, dl1110a, dl1120a) %>%
  dplyr::select(HtMstatus, ureyc, savings, da2101, da2102, da2103, da2105, da2106, 
         da2107, da2108, dl1210, dl1220, dl1231, dl1232, dl1110a, dl1120a) %>%
  pivot_longer(cols = -c(HtMstatus, ureyc), names_to = "Component", values_to = "Value")

# Create stacked bar decomposition
NIRE_decomp <- ggplot(df_long, aes(x = HtMstatus, y = Value, fill = Component)) +
  geom_bar(stat = "identity") +
  geom_segment(data = df_summary3, 
               mapping = aes(x = HtMstatus_numeric - 0.45, xend = HtMstatus_numeric + 0.45, 
                             y = ureyc, yend = ureyc), 
               inherit.aes = FALSE, color = "black", linetype = "solid", size = 1.2) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Net interest rate exposures decomposition",
    subtitle = "by hand-to-mouth status, mean, EUR thousands",
    x = "",
    y = "Mean EUR thousands",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0),
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10),
    panel.grid.major.x = element_blank() 
  )

# Add annotation
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ". Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 14, color = "gray40"
  )

NIRE_decomp <- plot_grid(NIRE_decomp, annotation, ncol = 1, rel_heights = c(1, 0.05))
#NIRE_decomp

################################################################################
# Decompose Net Nominal Positions
# Transform data to long format for ggplot
df_long <- df_summary3 %>%
  mutate(
    da2101 = da2101,
    da2103 = da2103,
    da2106 = da2106,
    da2107 = da2107,
    da2108 = da2108,
    da2109 = da2109,
    dl1110 = -dl1110,
    dl1120 = -dl1120,
    dl1210 = -dl1210,
    dl1220 = -dl1220,
    dl1231 = -dl1231,
    dl1232 = -dl1232) %>%
    #da2102 = -da2102,
    #da2104 = -da2104,
    #da2105 = -da2105, 
    #dl1000 = -dl1000) %>%
  dplyr::select(HtMstatus, nnp, da2101, da2103, da2106, da2107, da2108, da2109, dl1110,
         dl1120, dl1210, dl1220, dl1231, dl1232) %>%
  #dplyr::select(HtMstatus, nnp, da2100, da2102, da2104, da2105, dl1000) %>%
  pivot_longer(cols = -c(HtMstatus, nnp), names_to = "Component", values_to = "Value")

# Create stacked bar decomposition
NNP_decomp <- ggplot(df_long, aes(x = HtMstatus, y = Value, fill = Component)) +
  geom_bar(stat = "identity") +
  geom_segment(data = df_summary3, 
               mapping = aes(x = HtMstatus_numeric - 0.45, xend = HtMstatus_numeric + 0.45, 
                             y = nnp, yend = nnp), 
               inherit.aes = FALSE, color = "black", linetype = "solid", size = 1.2) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Net nominal positions decomposition",
    subtitle = "by hand-to-mouth status, mean, EUR thousands",
    x = "",
    y = "Mean EUR thousands",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0),
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10),
    panel.grid.major.x = element_blank() 
  )

# Add annotation
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ". Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 14, color = "gray40"
  )

NNP_decomp <- plot_grid(NNP_decomp, annotation, ncol = 1, rel_heights = c(1, 0.05))
#NNP_decomp

################################################################################
# Save plots
ggsave(paste0(DBNAME, "_NIRE_decomp.png"), plot = NIRE_decomp, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)
ggsave(paste0(DBNAME, "_NNP_decomp.png"), plot = NNP_decomp, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)