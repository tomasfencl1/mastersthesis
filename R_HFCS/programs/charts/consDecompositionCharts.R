################################################################################
# CONSUMPTION AGGREGATE RESPONSE CHARTS
# desc: creates df of results by taking weighted means of effects, plots results
################################################################################

# Load packages
library(ggplot2)
library(cowplot)
library(dplyr)

################################################################################

# Load dataset
df <- read.csv(file.path(TABLES, paste0(DBNAME, "_effectDecomp.csv")))
df_incs <- read.csv(file.path(TABLES, paste0(DBNAME, "_HtM_inc_shares.csv")))

################################################################################
# v1 direct consumption shares
cShare <- df %>%
  group_by(statushtm) %>%
  summarise(group_C = sum(C * hw0010, na.rm = TRUE)) %>%
  mutate(share = group_C / sum(group_C)) %>%
  arrange(statushtm) %>%
  pull(share) %>%
  matrix(nrow = 1)

colnames(cShare) <- sort(unique(df$statushtm))

# v2 consumption shares from income shares
#df_incs <- df_incs %>%
#  rename(
#    "1" = incs_pHtM,
#    "2" = incs_wHtM,
#    "3" = incs_nHtM,
#  )
#
## Handle Top 10% group if `top10Index == 1`
#if (top10Index == 1) {
#  df_incs <- df_incs %>% rename("4" = incs_top10)
#}
#
## Remove any remaining columns starting with "incs"
#df_incs <- df_incs %>% dplyr::select(-starts_with("incs"))
#
## Ordering
#ordered_columns <- c("1", "2", "3")
#if (top10Index == 1) {
#  ordered_columns <- c(ordered_columns, "4")
#}
#df_incs <- df_incs %>%
#  dplyr::select(sa0100, all_of(ordered_columns)) 
#
## Convert to matrix (without the `sa0100` column)
#cShare <- df_incs %>%
#  dplyr::select(-sa0100) %>%
#  as.matrix()

################################################################################
# Step 1: Collapse by statushtm and im0100 (mean weighted values)
df_summary1 <- df %>%
  group_by(statushtm, im0100) %>%
  summarise(
    across(starts_with(c("dURE", "dNNP", "dy", "dsub", "dhouse", "dstock", "di2001")), 
           ~ weighted.mean(.x, hw0010, na.rm = TRUE)),
    .groups = "drop"
  )

# Step 2: Collapse by statushtm (mean of already weighted means)
df_summary2 <- df_summary1 %>%
  group_by(statushtm) %>%
  summarise(
    across(starts_with(c("dURE", "dNNP", "dy", "dsub", "dhouse", "dstock", "di2001")), 
           ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# Save temporary file 
#write.csv(df_summary2, file.path(OUTPUT, "temp1.csv"), row.names = FALSE)

################################################################################
# Convert statushtm to character for matching with cShare column names
df_summary3 <- df_summary2 %>%
  mutate(statushtm = as.character(statushtm))

# Step 3: Loop through the columns to apply the consumption share and sum
for (v in c("dURE1", "dNNP1", "dy1", "dsub1", "dhouse1", "dstock1", "di2001")) {
  for (s in unique(df_summary3$statushtm)) {
    c_share <- cShare[1, s]
    df_summary3[[v]] <- ifelse(df_summary3$statushtm == s, df_summary3[[v]] * c_share, df_summary3[[v]])
  }
  
  # Calculate the total sum and overwrite all values in the column with the total
  total_var <- sum(df_summary3[[v]], na.rm = TRUE)
  df_summary3[[v]] <- total_var
}

# Keep only relevant columns and keep the first row (rows are identical)
df_summary3 <- df_summary3 %>%
  dplyr::select(dURE1, dNNP1, dy1, dsub1, dhouse1, dstock1, di2001) %>%
  slice(1)  # Keep only the first row

# Save the temporary dataframe to "temp.csv"
#write.csv(df_summary3, file.path(OUTPUT, "temp.csv"), row.names = FALSE)

################################################################################
# Append results by HtM status and total
df_results <- bind_rows(df_summary2, df_summary3)
#View(df_results)

# Add total category for the appended row (statushtm == 5)
df_results <- df_results %>%
  mutate(statushtm = ifelse(is.na(statushtm), ifelse(top10Index == 1, "5", "4"), statushtm))

# Add total effect column
df_results <- df_results %>%
  mutate(total = rowSums(dplyr::select(., dURE1, dNNP1, dy1, dsub1, dhouse1, dstock1), na.rm = TRUE))

# Add HtM labels for plotting
if (top10Index == 1) {
  df_results <- df_results %>%
    mutate(
      HtMstatus = factor(
        statushtm,
        levels = c(1, 2, 3, 4, 5),
        labels = c("Poor HtM", "Wealthy HtM", "Non-HtM", "Top 10", "All")
      )
    )
} else {
  df_results <- df_results %>%
    mutate(
      HtMstatus = factor(
        statushtm,
        levels = c(1, 2, 3, 4),
        labels = c("Poor HtM", "Wealthy HtM", "Non-HtM", "All")
      )
    )
}

# Prepare comparison df
df_comp <- df_results %>%
  filter(HtMstatus == "All") %>%
  select(dsub1, total) %>%
  mutate(var = consShock_var_CZ)

# Save the results dataframe
#write.csv(df_results, file.path(OUTPUT, "temp1.csv"), row.names = FALSE)

################################################################################
# Plot consumption response decomposition
# Reshape the dataframe to long format
df_results <- df_results %>%
  pivot_longer(
    cols = c("dsub1", "dURE1", "dy1", "dNNP1", "dhouse1", "dstock1"),
    names_to = "effect_type",
    values_to = "value"
  )

# Compute total bar height (sum of effects) per group for overlay
df_totalbar <- df_results %>%
  group_by(HtMstatus) %>%
  summarise(total_effect = sum(value, na.rm = TRUE)) %>%
  mutate(HtMstatus_numeric = as.numeric(HtMstatus))

# Create order of the effects
effect_order <- c("dsub1", "dURE1", "dy1", "dNNP1", "dhouse1", "dstock1")

# Create labels for the effects
effect_labels <- c(
  "dsub1" = "IES",
  "dURE1" = "NIE",
  "dy1" = "income",
  "dNNP1" = "Fisher",
  "dhouse1" = "housing",
  "dstock1" = "stocks"
)

# Color scheme
effect_colors <- c(
  "dsub1" = "#004488",  # IES
  "dURE1" = "#88CCEE",  # NIE
  "dy1" = "#FFA500",  # income
  "dNNP1" = "#D55E00",  # Fisher
  "dhouse1" = "#006400",  # housing
  "dstock1" = "#8BC34A"  # stocks
)

# Consumption response plot
consResponse_plot <- ggplot(df_results, aes(x = HtMstatus, y = value, fill = factor(effect_type, levels = rev(effect_order)))) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_segment(
    data = df_totalbar,
    aes(x = HtMstatus_numeric - 0.35, xend = HtMstatus_numeric + 0.35,
        y = total_effect, yend = total_effect),
    inherit.aes = FALSE,
    color = "black", size = 1.2) + # this adds the total bar
  scale_fill_manual(
    values = effect_colors,
    labels = effect_labels,
    breaks = effect_order  # Control legend order
  ) +
  labs(
    title = "Effects of -100 bp monetary policy shock on consumption",
    subtitle = "by hand-to-mouth status, percent",
    x = "",
    y = "Change in consumption, percent",
    fill = "Type of Effect"
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Add annotation for source
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ", authors' calculations. Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 12, color = "gray40"
  )
consResponse_plot <- plot_grid(consResponse_plot, annotation, ncol = 1, rel_heights = c(1, 0.05))
#consResponse_plot

################################################################################
# Save plots
ggsave(paste0(DBNAME, "_consResponse_plot.png"), plot = consResponse_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)

################################################################################
# RANK, HANK and VAR comparison
df_totalbar <- tibble(
  category = "HANK",
  total_effect = df_comp$total
)

df_comp <- tibble(
  category = c("HANK", "HANK", "VAR"),
  component = c("IES", "HANK ampl.", "VAR"),
  value = c(df_comp$dsub1,
            df_comp$total - df_comp$dsub1,
            df_comp$var)
)

colors_comp <- c(
  "IES" = "#004488",
  "HANK ampl." = "#88CCEE",
  "VAR" = "#FFA500"
)

# Comparison plot
HANKvsVAR_plot <- ggplot(df_comp, aes(x = category, y = value, fill = component)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_segment(
    data = df_totalbar,
    aes(x = 0.7, xend = 1.3, y = total_effect, yend = total_effect),
    inherit.aes = FALSE,
    color = "black", size = 1.2
  ) +
  scale_fill_manual(values = colors_comp) +
  labs(
    title = "Effects of -100 bp monetary policy shock on consumption",
    subtitle = "by modelling choice",
    x = "",
    y = "Change in consumption, percent",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0),
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10),
    panel.grid.major.x = element_blank()
  )

# Optional annotation (adjust variables if needed)
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ", authors' calculations. Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 14, color = "gray40"
  )
HANKvsVAR_plot <- plot_grid(HANKvsVAR_plot, annotation, ncol = 1, rel_heights = c(1, 0.05))
#HANKvsVAR_plot

################################################################################
# Save plots
ggsave(paste0(DBNAME, "_HANKvsVAR_plot.png"), plot = HANKvsVAR_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)

# Clean up temporary files
#file.remove(file.path(OUTPUT, "temp.csv"), file.path(OUTPUT, "temp1.csv"))