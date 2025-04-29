################################################################################
# HtM DEMOGRAPHICS CHARTS
# desc: creates descriptive charts of the HtM shares, demographics, etc.
################################################################################

# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(statar)

################################################################################
# Load dataset
df <- read.csv(file.path(OUTPUT, paste0(DBNAME, "_orig.csv")))

df_filtered <- df %>%
  filter(statushtm >= 0, !is.na(statushtm))

################################################################################
# Share of P-HtM and W-HtM, country total
# Calculate the mean of poorhtm, wealhtm, nonhtm, eventually top10
HtMshare <- df_filtered %>% 
  summarise(
    poorhtm = weighted.mean(poorhtm, hw0010, na.rm = TRUE) * 100,
    wealhtm = weighted.mean(wealhtm, hw0010, na.rm = TRUE) * 100,
    nonhtm = weighted.mean(nonhtm, hw0010, na.rm = TRUE) * 100,
    top10 = if (top10Index == 1) weighted.mean(top10htm, hw0010, na.rm = TRUE) * 100  else NA_real_
  ) %>%
  # Remove the top10 column if `top10Index` is 0
  dplyr::select(where(~ !all(is.na(.))))

# Prepare the data for plotting (only poor and wealthy HtM)
HtMshare <- HtMshare %>%
  dplyr::select(poorhtm, wealhtm) %>%
  pivot_longer(cols = everything(), names_to = "HtMstatus", values_to = "percentage") %>%
  mutate(
    HtMstatus = factor(
      HtMstatus,
      levels = c("wealhtm","poorhtm"), # Order matters here
      labels = c("Wealthy HtM","Poor HtM")
    )
  )

# Main plot HtMshare_plot
HtMshare_plot <- ggplot(HtMshare, aes(x = COUNTRY, y = percentage, fill = HtMstatus)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  geom_text(
    aes(label = sprintf("%.1f", percentage), 
        y = cumsum(percentage) - 0.5), # Correct label placement at the top
    size = 6,
    color = "white"
  ) +
  scale_fill_manual(
    values = c(
      "Poor HtM" = "#004488",
      "Wealthy HtM" = "#88CCEE"
    ),
    breaks = c("Poor HtM", "Wealthy HtM") # Reverse legend order
  ) +
  labs(
    title = "Share of hand-to-mouth households",
    subtitle = "percentages over total population",
    x = "",
    y = "Percentage",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA), # Ensure white background
    plot.background = element_rect(fill = "white", color = NA),  # Ensure white background
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0), # Adjust spacing between legend and plot
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10), # Maintain reduced bottom margin
    panel.grid.major.x = element_blank() 
  )

# Add annotation
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ". Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 14, color = "gray40"
  )
HtMshare_plot <- plot_grid(HtMshare_plot, annotation, ncol = 1, rel_heights = c(1, 0.05)) # Maintain relative heights
#HtMshare_plot

################################################################################
# Share of P-HtM and W-HtM, by age group
# Calculate the mean of poorhtm, wealhtm, and nonhtm by age group
HtMshare_age <- df_filtered %>%
  group_by(ageRangeRP) %>%
  summarise(
    poorhtm = weighted.mean(poorhtm, hw0010, na.rm = TRUE)* 100,
    wealhtm = weighted.mean(wealhtm, hw0010, na.rm = TRUE)* 100,
    nonhtm = weighted.mean(nonhtm, hw0010, na.rm = TRUE)* 100
  )

# Prepare the data for plotting by age group (only poor and wealthy HtM)
HtMshare_age <- HtMshare_age %>%
  dplyr::select(ageRangeRP, poorhtm, wealhtm) %>%
  pivot_longer(
    cols = c(poorhtm, wealhtm),
    names_to = "HtMstatus",
    values_to = "percentage"
  ) %>%
  mutate(
    HtMstatus = factor(
      HtMstatus,
      levels = c("poorhtm","wealhtm"), # Order matters here
      labels = c("Poor HtM","Wealthy HtM")
    )
  )

# Main plot HtMshare_age_plot
HtMshare_age_plot <- ggplot(HtMshare_age, aes(x = ageRangeRP, y = percentage, fill = HtMstatus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.9) +
  geom_text(
    aes(label = sprintf("%.1f", percentage)),
    position = position_dodge(width = 0.95),
    vjust = 1.5, # Move labels inside the columns
    size = 6,
    color = "white"
  ) +
  scale_fill_manual(
    values = c(
      "Poor HtM" = "#004488",
      "Wealthy HtM" = "#88CCEE"
    )
  ) +
  labs(
    title = "Share of hand-to-mouth households",
    subtitle = "percentages over total population, by age of the reference person",
    x = "",
    y = "Percentage",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA),  # Set plot background to white
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.box.margin = margin(t = -20, r = 0, b = 0, l = 0), # Adjust spacing between legend and plot
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10), # Maintain reduced bottom margin
    panel.grid.major.x = element_blank() 
  )

# Add annotation
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ". Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 14, color = "gray40"
  )
HtMshare_age_plot <- plot_grid(HtMshare_age_plot, annotation, ncol = 1, rel_heights = c(1, 0.05)) # Maintain relative heights
#HtMshare_age_plot

################################################################################
# Prepare weighted_median function
weighted_median <- function(x, w) {
  if (all(is.na(x))) return(NA_real_)  # Return NA if all values are NA
  sorted <- order(x, na.last = NA)     # Sort x, ignoring NAs
  x <- x[sorted]
  w <- w[sorted]
  w <- w / sum(w, na.rm = TRUE)        # Normalize weights
  cum_w <- cumsum(w)                   # Cumulative weights
  x[which(cum_w >= 0.5)[1]]            # Return first value meeting cumulative weight >= 0.5
}

################################################################################
# Net Liquid Wealth, by HtM
# Summarize liquid wealth (weighted median) by HtM status
NetLiquidWealth <- df_filtered %>%
  group_by(sa0100, statushtm) %>%
  summarise(liquid = weighted_median(liquid, hw0010) /1000) %>% 
  mutate(
    HtMstatus = factor(
      statushtm,
      levels = c(1, 2, 3, 4),
      labels = c("Poor HtM", "Wealthy HtM", "Non-HtM", "Top 10")
    )
  )

# Plot NetLiquidWealth_plot
NetLiquidWealth_plot <- ggplot(NetLiquidWealth, aes(x = COUNTRY, y = liquid, fill = HtMstatus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.9) +
  geom_text(
    aes(label = sprintf("%.1f", liquid)),
    position = position_dodge(width = 0.95),
    vjust = -0.5, # Place labels above the bars
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
    title = "Net liquid wealth",
    subtitle = "by hand-to-mouth status, median, EUR thousands",
    x = "",
    y = "Median EUR thousands",
    fill = ""
  ) +
  coord_cartesian(ylim = c(min(NetLiquidWealth$liquid) - (max(NetLiquidWealth$liquid)-min(NetLiquidWealth$liquid))*0.02, max(NetLiquidWealth$liquid) + (max(NetLiquidWealth$liquid)-min(NetLiquidWealth$liquid))*0.04)) +  # Extend Y-axis limits
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
NetLiquidWealth_plot <- plot_grid(NetLiquidWealth_plot, annotation, ncol = 1, rel_heights = c(1, 0.05))
#NetLiquidWealth_plot

################################################################################
# Net Illiquid Wealth, by HtM
# Summarize illiquid wealth (weighted median) by HtM status
NetIlliquidWealth <- df_filtered %>%
  group_by(sa0100, statushtm) %>%
  summarise(illiquid = weighted_median(illiquid, hw0010) /1000) %>% 
  mutate(
    HtMstatus = factor(
      statushtm,
      levels = c(1, 2, 3, 4),
      labels = c("Poor HtM", "Wealthy HtM", "Non-HtM", "Top 10")
    )
  )

# Plot NetIlliquidWealth_plot
NetIlliquidWealth_plot <- ggplot(NetIlliquidWealth, aes(x = COUNTRY, y = illiquid, fill = HtMstatus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.9) +
  geom_text(
    aes(label = sprintf("%.1f", illiquid)),
    position = position_dodge(width = 0.95),
    vjust = -0.5, # Place labels above the bars
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
    title = "Net illiquid wealth",
    subtitle = "by hand-to-mouth status, median, EUR thousands",
    x = "",
    y = "Median EUR thousands",
    fill = ""
  ) +
  coord_cartesian(ylim = c(min(NetIlliquidWealth$illiquid) - (max(NetIlliquidWealth$illiquid)-min(NetIlliquidWealth$illiquid))*0.02, max(NetIlliquidWealth$illiquid) + (max(NetIlliquidWealth$illiquid)-min(NetIlliquidWealth$illiquid))*0.04)) +  # Extend Y-axis limits
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
NetIlliquidWealth_plot <- plot_grid(NetIlliquidWealth_plot, annotation, ncol = 1, rel_heights = c(1, 0.05))
#NetIlliquidWealth_plot

################################################################################
# Net Wealth, by HtM
# Summarize net wealth (weighted median) by HtM status
NetWealth <- df_filtered %>%
  group_by(sa0100, statushtm) %>%
  summarise(wealth = weighted_median(dn3001, hw0010) /1000) %>% 
  mutate(
    HtMstatus = factor(
      statushtm,
      levels = c(1, 2, 3, 4), # Adjust levels for Poor, Wealthy, and Non-HtM
      labels = c("Poor HtM", "Wealthy HtM", "Non-HtM", "Top 10")
    )
  )

# Plot NetWealth_plot
NetWealth_plot <- ggplot(NetWealth, aes(x = COUNTRY, y = wealth, fill = HtMstatus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.9) +
  geom_text(
    aes(label = sprintf("%.1f", wealth)),
    position = position_dodge(width = 0.95),
    vjust = -0.5, # Place labels above the bars
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
    title = "Net wealth",
    subtitle = "by hand-to-mouth status, median, EUR thousands",
    x = "",
    y = "Median EUR thousands",
    fill = ""
  ) +
  coord_cartesian(ylim = c(min(NetWealth$wealth) - (max(NetWealth$wealth)-min(NetWealth$wealth))*0.02, max(NetWealth$wealth) + (max(NetWealth$wealth)-min(NetWealth$wealth))*0.04)) +  # Extend Y-axis limits
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
NetWealth_plot <- plot_grid(NetWealth_plot, annotation, ncol = 1, rel_heights = c(1, 0.05))
#NetWealth_plot

################################################################################
# Save plots
ggsave(paste0(DBNAME, "_HtMshare_plot.png"), plot = HtMshare_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)
ggsave(paste0(DBNAME, "_HtMshare_age_plot.png"), plot = HtMshare_age_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)
ggsave(paste0(DBNAME, "_NetLiquidWealth_plot.png"), plot = NetLiquidWealth_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)
ggsave(paste0(DBNAME, "_NetIlliquidWealth_plot.png"), plot = NetIlliquidWealth_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)
ggsave(paste0(DBNAME, "_NetWealth_plot.png"), plot = NetWealth_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)

################################################################################
# Prepare variables for decomposition
df <- df_filtered %>%
  mutate(across(c(liquid, illiquid, da2101, da2102, da2103, da2105, da2109, 
                  dl1210, dl1220, da1110, da1120, da1140, dl1110, dl1120, 
                  occupational, unsecuredhome), ~ replace_na(.x, 0)))

# Compute weighted means at the individual level
df_summary1 <- df %>%
  group_by(sa0100, statushtm, im0100) %>%
  summarise(
    liquid = weighted.mean(liquid, hw0010, na.rm = TRUE),
    illiquid = weighted.mean(illiquid, hw0010, na.rm = TRUE),
    da2101 = weighted.mean(da2101, hw0010, na.rm = TRUE),
    da2102 = weighted.mean(da2102, hw0010, na.rm = TRUE),
    da2103 = weighted.mean(da2103, hw0010, na.rm = TRUE),
    da2105 = weighted.mean(da2105, hw0010, na.rm = TRUE),
    da2109 = weighted.mean(da2109, hw0010, na.rm = TRUE),
    dl1210 = weighted.mean(dl1210, hw0010, na.rm = TRUE),
    dl1220 = weighted.mean(dl1220, hw0010, na.rm = TRUE),
    da1110 = weighted.mean(da1110, hw0010, na.rm = TRUE),
    da1120 = weighted.mean(da1120, hw0010, na.rm = TRUE),
    da1140 = weighted.mean(da1140, hw0010, na.rm = TRUE),
    dl1110 = weighted.mean(dl1110, hw0010, na.rm = TRUE),
    dl1120 = weighted.mean(dl1120, hw0010, na.rm = TRUE),
    occupational = weighted.mean(occupational, hw0010, na.rm = TRUE),
    unsecuredhome = weighted.mean(unsecuredhome, hw0010, na.rm = TRUE),
    .groups = "drop"
  )

# Collapse to sa0100, statushtm, averaging across im0100
df_summary2 <- df_summary1 %>%
  group_by(sa0100, statushtm) %>%
  summarise(
    liquid = mean(liquid, na.rm = TRUE),
    illiquid = mean(illiquid, na.rm = TRUE),
    da2101 = mean(da2101, na.rm = TRUE),
    da2102 = mean(da2102, na.rm = TRUE),
    da2103 = mean(da2103, na.rm = TRUE),
    da2105 = mean(da2105, na.rm = TRUE),
    da2109 = mean(da2109, na.rm = TRUE),
    dl1210 = mean(dl1210, na.rm = TRUE),
    dl1220 = mean(dl1220, na.rm = TRUE),
    da1110 = mean(da1110, na.rm = TRUE),
    da1120 = mean(da1120, na.rm = TRUE),
    da1140 = mean(da1140, na.rm = TRUE),
    dl1110 = mean(dl1110, na.rm = TRUE),
    dl1120 = mean(dl1120, na.rm = TRUE),
    occupational = mean(occupational, na.rm = TRUE),
    unsecuredhome = mean(unsecuredhome, na.rm = TRUE),
    .groups = "drop"
  )

# Divide values by 1000
df_summary3 <- df_summary2 %>%
  mutate(
    across(c(liquid, illiquid, da2101, da2102, da2103, da2105, da2109, dl1210, 
             dl1220, da1110, da1120, da1140, dl1110, dl1120, occupational, 
             unsecuredhome), ~ .x / 1000)
  )

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
  "da2101" = "#A6CEE3",   # Light Blue
  "da2102" = "#6AABD2",   # Medium Light Blue
  "da2103" = "#5599CC",   # Medium Blue
  "da2105" = "#2B6BB0",   # Dark Blue
  "da2109" = "#083366",   # Near Black Blue
  "dl1210" = "#FC9272",   # Light Red
  "dl1220" = "#FB6A4A",   # Medium Light Red
  "da1110" = "#006400",   # Dark Green
  "da1120" = "#228B22",   # Forest Green
  "da1140" = "#98FB98",   # Pale Green
  "dl1110" = "#6A3D9A",   # Deep Purple
  "dl1120" = "#8E6BAF",   # Soft Purple
  "occupational" = "#D55E00", #"#B15928",    # Orange
  "unsecuredhome" = "#FFA500" #"#33A02C"    # Yellow
)

################################################################################
# Decompose Net Liquid Wealth
# Transform data to long format for ggplot
df_long <- df_summary3 %>%
  mutate(
    da2101 = da2101,
    da2102 = da2102,
    da2103 = da2103,
    da2105 = da2105,
    dl1210 = -dl1210,
    dl1220 = -dl1220) %>%
  dplyr::select(HtMstatus, liquid, da2101, da2102, da2103, da2105, dl1210, dl1220) %>%
  pivot_longer(cols = -c(HtMstatus, liquid), names_to = "Component", values_to = "Value")

# Create stacked bar decomposition
NetLiquidWealth_decomp <- ggplot(df_long, aes(x = HtMstatus, y = Value, fill = Component)) +
  geom_bar(stat = "identity") +
  geom_segment(data = df_summary3, 
               mapping = aes(x = HtMstatus_numeric - 0.45, xend = HtMstatus_numeric + 0.45, 
                             y = liquid, yend = liquid), 
               inherit.aes = FALSE, color = "black", linetype = "solid", size = 1.2) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Net liquid wealth decomposition",
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

NetLiquidWealth_decomp <- plot_grid(NetLiquidWealth_decomp, annotation, ncol = 1, rel_heights = c(1, 0.05))
#NetLiquidWealth_decomp

################################################################################
# Decompose Net Illiquid Wealth
# Transform data to long format for ggplot
df_long <- df_summary3 %>%
  mutate(
    da1110 = da1110,
    da1120 = da1120,
    da1140 = da1140,
    occupational = occupational,
    da2109 = da2109,
    dl1110 = -dl1110,
    dl1120 = -dl1120,
    unsecuredhome = -unsecuredhome) %>%
  dplyr::select(HtMstatus, illiquid, da1110, da1120, da1140, occupational, da2109, dl1110, dl1120, unsecuredhome) %>%
  pivot_longer(cols = -c(HtMstatus, illiquid), names_to = "Component", values_to = "Value")

# Create stacked bar decomposition
NetIlliquidWealth_decomp <- ggplot(df_long, aes(x = HtMstatus, y = Value, fill = Component)) +
  geom_bar(stat = "identity") +
  geom_segment(data = df_summary3, 
               mapping = aes(x = HtMstatus_numeric - 0.45, xend = HtMstatus_numeric + 0.45, 
                             y = illiquid, yend = illiquid), 
               inherit.aes = FALSE, color = "black", linetype = "solid", size = 1.2) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Net illiquid wealth decomposition",
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

NetIlliquidWealth_decomp <- plot_grid(NetIlliquidWealth_decomp, annotation, ncol = 1, rel_heights = c(1, 0.05))
#NetIlliquidWealth_decomp

################################################################################
# Save plots
ggsave(paste0(DBNAME, "_NetLiquidWealth_decomp.png"), plot = NetLiquidWealth_decomp, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)
ggsave(paste0(DBNAME, "_NetIlliquidWealth_decomp.png"), plot = NetIlliquidWealth_decomp, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)

################################################################################
# Prepare income quintiles
HtMshare_incqtile <- df_filtered %>%
  mutate(di2001 = if_else(if_all(c(di1100, di1200, di1300, di1400, di1500, di1600, di1700, di1800), is.na), NA_real_, di2001))

# Prepare
imputations <- unique(HtMshare_incqtile$im0100)

# Loop through each implicate and calculate quintiles for all rows
for (i in imputations) {
  # Create columns for income and wealth quintiles
  HtMshare_incqtile <- HtMshare_incqtile %>%
    mutate(
      !!paste0("implic_grossincqtile", i) := ifelse(im0100 == i, xtile(as.numeric(di2000), n = 5, wt = hw0010), NA_real_),
      !!paste0("implic_netincqtile", i) := ifelse(im0100 == i, xtile(as.numeric(di2001), n = 5, wt = hw0010), NA_real_),
      !!paste0("implic_dn3001qtile", i) := ifelse(im0100 == i, xtile(as.numeric(dn3001), n = 5, wt = hw0010), NA_real_)
    )
}

# Aggregate quintiles across implicates
HtMshare_incqtile <- HtMshare_incqtile %>%
  mutate(
    grossincqtile = rowSums(dplyr::select(., starts_with("implic_grossincqtile")), na.rm = TRUE),
    netincqtile = rowSums(dplyr::select(., starts_with("implic_netincqtile")), na.rm = TRUE),
    dn3001qtile = rowSums(dplyr::select(., starts_with("implic_dn3001qtile")), na.rm = TRUE)
  )

# Drop individual implicate-specific quintiles
HtMshare_incqtile <- HtMshare_incqtile %>% dplyr::select(-starts_with("implic_grossincqtile"), -starts_with("implic_netincqtile"), -starts_with("implic_dn3001qtile"))

################################################################################
# Share of P-HtM and W-HtM, by gross income quintile
# Calculate the mean of poorhtm, wealhtm, and nonhtm by income quintile
HtMshare_grossincqtile <- HtMshare_incqtile %>%
  group_by(grossincqtile) %>%
  summarise(
    poorhtm = weighted.mean(poorhtm, hw0010, na.rm = TRUE)* 100,
    wealhtm = weighted.mean(wealhtm, hw0010, na.rm = TRUE)* 100,
    nonhtm = weighted.mean(nonhtm, hw0010, na.rm = TRUE)* 100
  )

# Prepare the data for plotting by income quintile (only poor and wealthy HtM)
HtMshare_grossincqtile <- HtMshare_grossincqtile %>%
  dplyr::select(grossincqtile, poorhtm, wealhtm) %>%
  pivot_longer(
    cols = c(poorhtm, wealhtm),
    names_to = "HtMstatus",
    values_to = "percentage"
  ) %>%
  mutate(
    HtMstatus = factor(
      HtMstatus,
      levels = c("poorhtm","wealhtm"), # Order matters here
      labels = c("Poor HtM","Wealthy HtM")
    )
  )

# HtMshare_grossincqtile_plot
HtMshare_grossincqtile_plot <- ggplot(HtMshare_grossincqtile, aes(x = grossincqtile, y = percentage, fill = HtMstatus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.9) +
  geom_text(
    aes(label = sprintf("%.1f", percentage)),
    position = position_dodge(width = 0.95),
    vjust = 1.05, # Move labels inside the columns
    size = 6,
    color = "white"
  ) +
  scale_fill_manual(
    values = c(
      "Poor HtM" = "#004488",
      "Wealthy HtM" = "#88CCEE"
    )
  ) +
  labs(
    title = "Share of hand-to-mouth households",
    subtitle = "percentages over total population, by gross income quintile",
    x = "",
    y = "Percentage",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA),  # Set plot background to white
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.box.margin = margin(t = -20, r = 0, b = 0, l = 0), # Adjust spacing between legend and plot
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10), # Maintain reduced bottom margin
    panel.grid.major.x = element_blank() 
  )

# Add annotation
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ". Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 14, color = "gray40"
  )
HtMshare_grossincqtile_plot <- plot_grid(HtMshare_grossincqtile_plot, annotation, ncol = 1, rel_heights = c(1, 0.05)) # Maintain relative heights
#HtMshare_grossincqtile_plot

################################################################################
# Share of P-HtM and W-HtM, by net income quintile
# Calculate the mean of poorhtm, wealhtm, and nonhtm by income quintile
HtMshare_netincqtile <- HtMshare_incqtile %>%
  group_by(netincqtile) %>%
  summarise(
    poorhtm = weighted.mean(poorhtm, hw0010, na.rm = TRUE)* 100,
    wealhtm = weighted.mean(wealhtm, hw0010, na.rm = TRUE)* 100,
    nonhtm = weighted.mean(nonhtm, hw0010, na.rm = TRUE)* 100
  )

# Prepare the data for plotting by income quintile (only poor and wealthy HtM)
HtMshare_netincqtile <- HtMshare_netincqtile %>%
  dplyr::select(netincqtile, poorhtm, wealhtm) %>%
  pivot_longer(
    cols = c(poorhtm, wealhtm),
    names_to = "HtMstatus",
    values_to = "percentage"
  ) %>%
  mutate(
    HtMstatus = factor(
      HtMstatus,
      levels = c("poorhtm","wealhtm"), # Order matters here
      labels = c("Poor HtM","Wealthy HtM")
    )
  )

# HtMshare_netincqtile_plot
HtMshare_netincqtile_plot <- ggplot(HtMshare_netincqtile, aes(x = netincqtile, y = percentage, fill = HtMstatus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.9) +
  geom_text(
    aes(label = sprintf("%.1f", percentage)),
    position = position_dodge(width = 0.95),
    vjust = 1.05, # Move labels inside the columns
    size = 6,
    color = "white"
  ) +
  scale_fill_manual(
    values = c(
      "Poor HtM" = "#004488",
      "Wealthy HtM" = "#88CCEE"
    )
  ) +
  labs(
    title = "Share of hand-to-mouth households",
    subtitle = "percentages over total population, by net income quintile",
    x = "",
    y = "Percentage",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA),  # Set plot background to white
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.box.margin = margin(t = -20, r = 0, b = 0, l = 0), # Adjust spacing between legend and plot
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10), # Maintain reduced bottom margin
    panel.grid.major.x = element_blank() 
  )

# Add annotation
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ". Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 14, color = "gray40"
  )
HtMshare_netincqtile_plot <- plot_grid(HtMshare_netincqtile_plot, annotation, ncol = 1, rel_heights = c(1, 0.05)) # Maintain relative heights
#HtMshare_netincqtile_plot

################################################################################
# Share of P-HtM and W-HtM, by dn3001 quintile
# Calculate the mean of poorhtm, wealhtm, and nonhtm by dn3001 quintile
HtMshare_dn3001qtile <- HtMshare_incqtile %>%
  group_by(dn3001qtile) %>%
  summarise(
    poorhtm = weighted.mean(poorhtm, hw0010, na.rm = TRUE)* 100,
    wealhtm = weighted.mean(wealhtm, hw0010, na.rm = TRUE)* 100,
    nonhtm = weighted.mean(nonhtm, hw0010, na.rm = TRUE)* 100
  )

# Prepare the data for plotting by income quintile (only poor and wealthy HtM)
HtMshare_dn3001qtile <- HtMshare_dn3001qtile %>%
  dplyr::select(dn3001qtile, poorhtm, wealhtm) %>%
  pivot_longer(
    cols = c(poorhtm, wealhtm),
    names_to = "HtMstatus",
    values_to = "percentage"
  ) %>%
  mutate(
    HtMstatus = factor(
      HtMstatus,
      levels = c("poorhtm","wealhtm"), # Order matters here
      labels = c("Poor HtM","Wealthy HtM")
    )
  )

# HtMshare_dn3001qtile_plot
HtMshare_dn3001qtile_plot <- ggplot(HtMshare_dn3001qtile, aes(x = dn3001qtile, y = percentage, fill = HtMstatus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.9) +
  geom_text(
    aes(label = sprintf("%.1f", percentage)),
    position = position_dodge(width = 0.95),
    vjust = 1.05, # Move labels inside the columns
    size = 6,
    color = "white"
  ) +
  scale_fill_manual(
    values = c(
      "Poor HtM" = "#004488",
      "Wealthy HtM" = "#88CCEE"
    )
  ) +
  labs(
    title = "Share of hand-to-mouth households",
    subtitle = "percentages over total population, by dn3001 quintile",
    x = "",
    y = "Percentage",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA),  # Set plot background to white
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.box.margin = margin(t = -20, r = 0, b = 0, l = 0), # Adjust spacing between legend and plot
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10), # Maintain reduced bottom margin
    panel.grid.major.x = element_blank() 
  )

# Add annotation
annotation <- ggdraw() +
  draw_label(
    paste0("Source: HFCS wave ", WAVE, ". Country: ", COUNTRY, "."),
    fontface = "italic",
    x = 0.5, hjust = 0.5, size = 14, color = "gray40"
  )
HtMshare_dn3001qtile_plot <- plot_grid(HtMshare_dn3001qtile_plot, annotation, ncol = 1, rel_heights = c(1, 0.05)) # Maintain relative heights
#HtMshare_dn3001qtile_plot
#View(HtMshare_dn3001qtile)

################################################################################
# Distributions
df_long <- df_filtered %>%
  dplyr::select(di2000, di2001) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Plot the density distributions
ggplot(df_long, aes(x = Value, color = Variable)) +
  geom_density(size = 1.2) +
  labs(
    title = "Distribution of di2000 and di2001",
    x = "Value",
    y = "Density",
    color = "Variable"
  ) +
  scale_color_manual(values = c("di2000" = "#004488", "di2001" = "#D55E00")) + # Custom colors
  theme_minimal(base_size = 14)

################################################################################
# Save plots
ggsave(paste0(DBNAME, "_HtMshare_grossincqtile_plot.png"), plot = HtMshare_grossincqtile_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)
ggsave(paste0(DBNAME, "_HtMshare_netincqtile_plot.png"), plot = HtMshare_netincqtile_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)
ggsave(paste0(DBNAME, "_HtMshare_dn3001qtile_plot.png"), plot = HtMshare_dn3001qtile_plot, path = file.path(GRAPHS), bg = "white", width = 7, height = 7)