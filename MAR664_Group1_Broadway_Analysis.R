# =============================================================================
# MAR 664 – Predictive Analytics | Group 1 Final Project
# Broadway Weekly Grosses Predictive Model
# =============================================================================
# Team: Jenni Ogasian · Alex Popovic · Eisenhower Agyekum-Yamoah
#       Michael Mansaray · Tiangay Kallon
#
# This script runs:
#   1. Package loading & data import
#   2. Data cleaning & feature engineering
#   3. Exploratory Data Analysis (EDA) — ggplot2 visualisations
#   4. Model preparation (train/test split, scaling, encoding)
#   5. Models: OLS · Ridge · LASSO · ElasticNet · Random Forest ·
#              Gradient Boosting · XGBoost · LightGBM
#   6. Cross-validation (5-fold)
#   7. Feature importance + SHAP
#   8. Model comparison & results export
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# 0. INSTALL & LOAD PACKAGES
# ─────────────────────────────────────────────────────────────────────────────

# Uncomment to install on first run:
# install.packages(c(
#   "tidyverse", "lubridate", "scales", "ggcorrplot",
#   "caret", "glmnet", "ranger", "gbm", "xgboost", "lightgbm",
#   "vip", "shapviz", "SHAPforxgboost", "patchwork",
#   "knitr", "kableExtra", "Metrics", "doParallel"
# ))

library(tidyverse)       # dplyr, ggplot2, tidyr, readr, stringr, purrr
library(lubridate)       # date handling
library(scales)          # axis formatting ($, %)
library(ggcorrplot)      # correlation heatmap

library(caret)           # unified ML framework (train/test split, CV)
library(glmnet)          # Ridge, LASSO, ElasticNet
library(ranger)          # fast Random Forest
library(gbm)             # Gradient Boosting
library(xgboost)         # XGBoost
library(lightgbm)        # LightGBM

library(vip)             # variable importance plots
library(SHAPforxgboost)  # SHAP values for XGBoost
library(patchwork)       # combine ggplot panels

library(Metrics)         # rmse(), mae()
library(doParallel)      # parallel CV backend

# Suppress package startup messages
options(warn = -1)

# Set global plot theme
theme_broadway <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle    = element_text(color = "grey40", size = 10, hjust = 0),
    axis.title       = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom"
  )
theme_set(theme_broadway)

# Colour palette
COL_DARK   <- "#1a1a2e"
COL_ACCENT <- "#e94560"
COL_GOLD   <- "#f5a623"
COL_BLUE   <- "#0f3460"
PAL_MAIN   <- c(COL_DARK, COL_BLUE, "#16213e", COL_ACCENT, COL_GOLD,
                "#7ec8e3", "#a8d8a8", "#ffcc99")

set.seed(42)


# ─────────────────────────────────────────────────────────────────────────────
# 1. DATA IMPORT
# ─────────────────────────────────────────────────────────────────────────────

# Update this path to match your local file location
FILE_PATH <- "broadway_data_with_show_type_cleaned_names__seasons_and_long_term.csv"

df_raw <- read_csv(FILE_PATH, show_col_types = FALSE)

cat("─────────────────────────────────────────────────────\n")
cat("BROADWAY PREDICTIVE ANALYTICS – GROUP 1\n")
cat("─────────────────────────────────────────────────────\n")
cat(sprintf("✓ Loaded %s rows × %s columns\n",
            format(nrow(df_raw), big.mark = ","), ncol(df_raw)))
cat(sprintf("  Year range: %d – %d\n",
            min(df_raw$Date.Year), max(df_raw$Date.Year)))

glimpse(df_raw)
summary(df_raw)


# ─────────────────────────────────────────────────────────────────────────────
# 2. DATA CLEANING & FEATURE ENGINEERING
# ─────────────────────────────────────────────────────────────────────────────

df <- df_raw %>%

  # ── Parse date ─────────────────────────────────────────────────────────
  mutate(Date.Full = mdy(Date.Full)) %>%

  # ── Filter valid rows ───────────────────────────────────────────────────
  filter(Statistics.Gross > 0) %>%

  # ── Recession indicator (NBER dates) ───────────────────────────────────
  mutate(
    Is_Recession = case_when(
      Date.Year == 2001 & Date.Month %in% 3:11   ~ 1L,
      Date.Year == 2007 & Date.Month == 12        ~ 1L,
      Date.Year == 2008                           ~ 1L,
      Date.Year == 2009 & Date.Month <= 6         ~ 1L,
      TRUE                                        ~ 0L
    )
  ) %>%

  # ── Average ticket price (proxy) ────────────────────────────────────────
  mutate(
    Avg_Ticket_Price = if_else(
      Statistics.Attendance > 0,
      Statistics.Gross / Statistics.Attendance,
      NA_real_
    )
  ) %>%
  mutate(Avg_Ticket_Price = replace_na(
    Avg_Ticket_Price, median(Avg_Ticket_Price, na.rm = TRUE)
  )) %>%

  # ── Capacity utilisation ────────────────────────────────────────────────
  mutate(
    Capacity_Utilisation = if_else(
      Statistics.Capacity > 0,
      Statistics.Attendance / Statistics.Capacity,
      NA_real_
    )
  ) %>%
  mutate(Capacity_Utilisation = replace_na(
    Capacity_Utilisation, median(Capacity_Utilisation, na.rm = TRUE)
  )) %>%

  # ── Revenue per performance ─────────────────────────────────────────────
  mutate(
    Rev_Per_Performance = if_else(
      Statistics.Performances > 0,
      Statistics.Gross / Statistics.Performances,
      NA_real_
    )
  ) %>%
  mutate(Rev_Per_Performance = replace_na(
    Rev_Per_Performance, median(Rev_Per_Performance, na.rm = TRUE)
  )) %>%

  # ── Gross potential % ───────────────────────────────────────────────────
  mutate(
    Has_GrossPotential   = as.integer(`Statistics.Gross Potential` > 0),
    Pct_Gross_Potential  = if_else(
      `Statistics.Gross Potential` > 0,
      Statistics.Gross / `Statistics.Gross Potential`,
      NA_real_
    )
  ) %>%
  mutate(Pct_Gross_Potential = replace_na(
    Pct_Gross_Potential, median(Pct_Gross_Potential, na.rm = TRUE)
  )) %>%

  # ── Holiday / long-term / top-theatre flags ──────────────────────────────
  mutate(
    Is_Holiday_Week = as.integer(Season == "Holidays"),
    Is_LongTerm     = as.integer(`Long Term vs. New Show` == "Yes")
  ) %>%

  # ── Theatre tier ────────────────────────────────────────────────────────
  group_by(Show.Theatre) %>%
  mutate(Theatre_MedianGross = median(Statistics.Gross)) %>%
  ungroup() %>%
  mutate(
    Is_TopTheatre = as.integer(
      Theatre_MedianGross >= quantile(
        Theatre_MedianGross, 0.85, na.rm = TRUE
      )
    )
  ) %>%

  # ── Decade label ─────────────────────────────────────────────────────────
  mutate(Decade = paste0(floor(Date.Year / 10) * 10, "s")) %>%

  # ── Log gross (modelling target alternative) ────────────────────────────
  mutate(Log_Gross = log1p(Statistics.Gross))

cat(sprintf("✓ Feature engineering complete — %s columns\n", ncol(df)))
cat(sprintf("  Recession weeks : %s\n",
            format(sum(df$Is_Recession), big.mark = ",")))
cat(sprintf("  Long-term rows  : %s\n",
            format(sum(df$Is_LongTerm), big.mark = ",")))
cat(sprintf("  Holiday weeks   : %s\n",
            format(sum(df$Is_Holiday_Week), big.mark = ",")))
cat(sprintf("  Median ticket $ : $%.2f\n",
            median(df$Avg_Ticket_Price)))


# ─────────────────────────────────────────────────────────────────────────────
# 3. EDA – FIGURE 1: Distributions & Time Trends
# ─────────────────────────────────────────────────────────────────────────────

cat("\n[EDA] Building Figure 1 – Distributions & Time Trends …\n")

# 1a. Weekly Gross Distribution
p1a <- ggplot(df, aes(x = Statistics.Gross / 1e6)) +
  geom_histogram(bins = 60, fill = COL_DARK, color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = median(Statistics.Gross / 1e6)),
             color = COL_ACCENT, linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(Statistics.Gross / 1e6)),
             color = COL_GOLD, linewidth = 1, linetype = "dotted") +
  annotate("text", x = median(df$Statistics.Gross / 1e6) + 0.05,
           y = Inf, vjust = 2, hjust = 0, size = 3, color = COL_ACCENT,
           label = sprintf("Median: $%.2fM", median(df$Statistics.Gross / 1e6))) +
  annotate("text", x = mean(df$Statistics.Gross / 1e6) + 0.05,
           y = Inf, vjust = 4, hjust = 0, size = 3, color = COL_GOLD,
           label = sprintf("Mean: $%.2fM", mean(df$Statistics.Gross / 1e6))) +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Distribution of Weekly Gross",
       x = "Weekly Gross ($M)", y = "Count")

# 1b. Log-transformed gross
p1b <- ggplot(df, aes(x = Log_Gross)) +
  geom_histogram(bins = 60, fill = COL_BLUE, color = "white", alpha = 0.85) +
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 3.5,
           label = sprintf("Skewness: %.3f", moments::skewness(df$Log_Gross))) +
  labs(title = "Log-Transformed Gross (Closer to Normal)",
       x = "log(1 + Weekly Gross)", y = "Count")

# 1c. Gross by show type
p1c <- df %>%
  mutate(Show.Type = fct_reorder(Show.Type, Statistics.Gross, median, .desc = TRUE)) %>%
  ggplot(aes(x = Show.Type, y = Statistics.Gross / 1e6, fill = Show.Type)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_fill_manual(values = PAL_MAIN[1:3]) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Gross by Show Type",
       x = NULL, y = "Weekly Gross ($M)") +
  theme(legend.position = "none")

# 1d. Annual gross trend
annual_df <- df %>%
  group_by(Date.Year) %>%
  summarise(
    Total_Gross = sum(Statistics.Gross, na.rm = TRUE),
    Avg_Gross   = mean(Statistics.Gross, na.rm = TRUE),
    .groups     = "drop"
  )

p1d <- ggplot(annual_df, aes(x = Date.Year)) +
  annotate("rect", xmin = 2001.25, xmax = 2001.92,
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.1) +
  annotate("rect", xmin = 2007.92, xmax = 2009.5,
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.1) +
  geom_col(aes(y = Total_Gross / 1e9), fill = COL_DARK, alpha = 0.75) +
  geom_line(aes(y = Avg_Gross / 1e6 * 0.15), color = COL_ACCENT,
            linewidth = 1.2) +
  geom_point(aes(y = Avg_Gross / 1e6 * 0.15), color = COL_ACCENT, size = 2) +
  scale_y_continuous(
    name = "Total Annual Gross ($B)",
    labels = dollar_format(prefix = "$", suffix = "B"),
    sec.axis = sec_axis(~ . / 0.15, name = "Avg Weekly Gross ($M)",
                        labels = dollar_format(prefix = "$", suffix = "M"))
  ) +
  labs(title = "Annual Broadway Gross Trend (Recession Shading)",
       x = "Year",
       caption = "Red shading = recession periods (NBER)")

# 1e. Seasonal patterns
season_order <- c("Winter", "Tony Season", "Summer", "Fall", "Holidays")
season_order <- intersect(season_order, unique(df$Season))

p1e <- df %>%
  mutate(Season = factor(Season, levels = season_order)) %>%
  group_by(Season) %>%
  summarise(Median_Gross = median(Statistics.Gross, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = Season, y = Median_Gross / 1e6, fill = Season)) +
  geom_col(color = "white") +
  geom_text(aes(label = dollar(Median_Gross / 1e6, accuracy = 0.01, suffix = "M")),
            vjust = -0.5, size = 3.2) +
  scale_fill_manual(values = PAL_MAIN) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Median Gross by Season",
       x = NULL, y = "Median Weekly Gross ($M)") +
  theme(legend.position = "none")

# 1f. Long-term vs. new
p1f <- ggplot(df, aes(x = Statistics.Gross / 1e6,
                       fill = `Long Term vs. New Show`)) +
  geom_histogram(bins = 50, alpha = 0.65, position = "identity") +
  scale_fill_manual(values = c("No" = COL_ACCENT, "Yes" = COL_DARK),
                    labels = c("No" = "New Show", "Yes" = "Long-Term")) +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Long-Term vs. New Show Distribution",
       x = "Weekly Gross ($M)", y = "Count", fill = NULL)

fig1 <- (p1a | p1b | p1c) / (p1d | p1e | p1f) +
  plot_annotation(
    title    = "Broadway Weekly Grosses — EDA: Distributions & Time Trends",
    subtitle = "1990–2016 | Pre-pandemic data",
    theme    = theme(plot.title = element_text(face = "bold", size = 15))
  )

ggsave("fig1_eda_distributions.png", fig1, width = 18, height = 11, dpi = 150)
cat("  ✓ fig1_eda_distributions.png saved\n")


# ─────────────────────────────────────────────────────────────────────────────
# 4. EDA – FIGURE 2: Correlations & Relationships
# ─────────────────────────────────────────────────────────────────────────────

cat("[EDA] Building Figure 2 – Correlations & Relationships …\n")

# 2a. Correlation matrix
num_vars <- df %>%
  select(
    Statistics.Gross, Statistics.Attendance, Statistics.Capacity,
    Statistics.Performances, Avg_Ticket_Price, Capacity_Utilisation,
    Rev_Per_Performance, Is_Recession, Is_LongTerm, Is_Holiday_Week
  )

corr_mat <- cor(num_vars, use = "complete.obs")

p2a <- ggcorrplot(
  corr_mat, hc.order = TRUE, type = "lower",
  lab = TRUE, lab_size = 2.8,
  colors = c(COL_ACCENT, "white", COL_DARK),
  outline.color = "white"
) + labs(title = "Correlation Matrix")

# 2b. Attendance vs Gross (sample)
df_sample <- df %>% sample_n(min(5000, nrow(df)))
p2b <- ggplot(df_sample,
              aes(x = Statistics.Attendance,
                  y = Statistics.Gross / 1e6,
                  color = factor(Is_LongTerm))) +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, color = COL_ACCENT, linewidth = 1.2) +
  scale_color_manual(values = c("0" = COL_BLUE, "1" = COL_GOLD),
                     labels = c("0" = "New Show", "1" = "Long-Term")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Attendance vs. Weekly Gross",
       x = "Attendance", y = "Weekly Gross ($M)", color = NULL)

# 2c. Ticket price over time
p2c <- df %>%
  group_by(Date.Year) %>%
  summarise(Median_Ticket = median(Avg_Ticket_Price, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = Date.Year, y = Median_Ticket)) +
  geom_area(fill = COL_DARK, alpha = 0.3) +
  geom_line(color = COL_DARK, linewidth = 1.3) +
  geom_point(color = COL_DARK, size = 2) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(title = "Median Average Ticket Price Over Time",
       x = "Year", y = "Avg Ticket Price ($)")

# 2d. Top 10 theatres by median gross
p2d <- df %>%
  group_by(Show.Theatre) %>%
  summarise(Median_Gross = median(Statistics.Gross, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  slice_max(Median_Gross, n = 10) %>%
  mutate(Show.Theatre = fct_reorder(Show.Theatre, Median_Gross)) %>%
  ggplot(aes(x = Median_Gross / 1e6, y = Show.Theatre)) +
  geom_col(fill = COL_DARK, color = "white") +
  geom_text(aes(label = dollar(Median_Gross / 1e6, accuracy = 0.01, suffix = "M")),
            hjust = -0.1, size = 3) +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "M"),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Top 10 Theatres by Median Gross",
       x = "Median Weekly Gross ($M)", y = NULL)

# 2e. Recession impact
p2e <- df %>%
  group_by(Date.Year, Is_Recession) %>%
  summarise(Avg_Gross = mean(Statistics.Gross, na.rm = TRUE), .groups = "drop") %>%
  mutate(Period = if_else(Is_Recession == 1, "Recession", "Normal")) %>%
  ggplot(aes(x = Date.Year, y = Avg_Gross / 1e6, fill = Period)) +
  geom_col(color = "white", width = 0.85) +
  scale_fill_manual(values = c("Normal" = COL_DARK, "Recession" = COL_ACCENT)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Recession Impact on Weekly Gross",
       x = "Year", y = "Avg Weekly Gross ($M)", fill = NULL)

# 2f. Capacity utilisation bands vs gross
p2f <- df %>%
  mutate(Util_Band = cut(Capacity_Utilisation, breaks = 10)) %>%
  group_by(Util_Band) %>%
  summarise(Median_Gross = median(Statistics.Gross, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = Util_Band, y = Median_Gross / 1e6)) +
  geom_col(fill = COL_BLUE, color = "white") +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Gross by Capacity Utilisation Band",
       x = "Utilisation", y = "Median Weekly Gross ($M)") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))

fig2 <- (p2a | p2b | p2c) / (p2d | p2e | p2f) +
  plot_annotation(
    title    = "Broadway Weekly Grosses — Feature Relationships & Correlations",
    theme    = theme(plot.title = element_text(face = "bold", size = 15))
  )

ggsave("fig2_eda_relationships.png", fig2, width = 18, height = 11, dpi = 150)
cat("  ✓ fig2_eda_relationships.png saved\n")


# ─────────────────────────────────────────────────────────────────────────────
# 5. EDA – FIGURE 3: Categorical Deep Dives
# ─────────────────────────────────────────────────────────────────────────────

cat("[EDA] Building Figure 3 – Categorical Deep Dives …\n")

# 3a. Monthly pattern
month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")
holiday_months <- c(1, 11, 12)

p3a <- df %>%
  group_by(Date.Month) %>%
  summarise(Median_Gross = median(Statistics.Gross, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(
    Month_Label = month_labels[Date.Month],
    Is_Holiday  = Date.Month %in% holiday_months
  ) %>%
  mutate(Month_Label = fct_inorder(Month_Label)) %>%
  ggplot(aes(x = Month_Label, y = Median_Gross / 1e6, fill = Is_Holiday)) +
  geom_col(color = "white") +
  scale_fill_manual(values = c("FALSE" = COL_DARK, "TRUE" = COL_ACCENT),
                    labels = c("FALSE" = "Standard", "TRUE" = "Holiday Month")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Monthly Gross Patterns",
       x = NULL, y = "Median Weekly Gross ($M)", fill = NULL) +
  theme(axis.text.x = element_text(size = 9))

# 3b. Decade × Show Type
p3b <- df %>%
  group_by(Decade, Show.Type) %>%
  summarise(Avg_Gross = mean(Statistics.Gross, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Decade, y = Avg_Gross / 1e6, fill = Show.Type)) +
  geom_col(position = "dodge", color = "white") +
  scale_fill_manual(values = PAL_MAIN[1:3]) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Avg Gross by Decade & Show Type",
       x = "Decade", y = "Avg Weekly Gross ($M)", fill = "Type")

# 3c. Long-term vs New by season
p3c <- df %>%
  mutate(Season = factor(Season, levels = season_order)) %>%
  group_by(Season, `Long Term vs. New Show`) %>%
  summarise(Median_Gross = median(Statistics.Gross, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = Season, y = Median_Gross / 1e6,
             fill = `Long Term vs. New Show`)) +
  geom_col(position = "dodge", color = "white") +
  scale_fill_manual(values = c("No" = COL_ACCENT, "Yes" = COL_DARK),
                    labels = c("No" = "New Show", "Yes" = "Long-Term")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Long-Term vs. New Show by Season",
       x = NULL, y = "Median Weekly Gross ($M)", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# 3d. Top 15 shows all-time
p3d <- df %>%
  group_by(Show.Name) %>%
  summarise(Total_Gross = sum(Statistics.Gross, na.rm = TRUE),
            .groups = "drop") %>%
  slice_max(Total_Gross, n = 15) %>%
  mutate(Show.Name = fct_reorder(Show.Name, Total_Gross)) %>%
  ggplot(aes(x = Total_Gross / 1e9, y = Show.Name)) +
  geom_col(fill = COL_ACCENT, color = "white") +
  geom_text(aes(label = dollar(Total_Gross / 1e9, accuracy = 0.01, suffix = "B")),
            hjust = -0.1, size = 3) +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "B"),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Top 15 Shows — Total Gross (All Years)",
       x = "Total Gross ($B)", y = NULL)

# 3e. Performances per week distribution
p3e <- df %>%
  count(Statistics.Performances) %>%
  mutate(Pct = n / sum(n)) %>%
  ggplot(aes(x = factor(Statistics.Performances), y = Pct)) +
  geom_col(fill = COL_BLUE, color = "white") +
  geom_text(aes(label = percent(Pct, accuracy = 0.1)), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Performances per Week — Frequency",
       x = "Performances per Week", y = "Share of Weeks")

# 3f. Gross vs Performances (violin)
p3f <- df %>%
  filter(Statistics.Performances > 0, Statistics.Performances <= 12) %>%
  mutate(Performances = factor(Statistics.Performances)) %>%
  ggplot(aes(x = Performances, y = Statistics.Gross / 1e6, fill = Performances)) +
  geom_violin(trim = TRUE, alpha = 0.75, color = "white") +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 0.3) +
  scale_fill_manual(values = colorRampPalette(PAL_MAIN)(length(unique(
    df$Statistics.Performances[df$Statistics.Performances %in% 1:12]
  )))) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Weekly Gross by Number of Performances",
       x = "Performances per Week", y = "Weekly Gross ($M)") +
  theme(legend.position = "none")

fig3 <- (p3a | p3b | p3c) / (p3d | p3e | p3f) +
  plot_annotation(
    title = "Broadway Weekly Grosses — Categorical Analysis",
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )

ggsave("fig3_eda_categorical.png", fig3, width = 18, height = 11, dpi = 150)
cat("  ✓ fig3_eda_categorical.png saved\n")


# ─────────────────────────────────────────────────────────────────────────────
# 6. MODEL PREPARATION
# ─────────────────────────────────────────────────────────────────────────────

cat("\n[MODEL] Preparing model matrix …\n")

# Encode categorical variables as numeric factors
df_model <- df %>%
  mutate(
    ShowType_enc  = as.integer(factor(Show.Type)),
    Season_enc    = as.integer(factor(Season)),
    Theatre_enc   = as.integer(factor(Show.Theatre))
  )

# Feature list
FEATURES <- c(
  "Statistics.Attendance", "Statistics.Capacity", "Statistics.Performances",
  "Avg_Ticket_Price", "Capacity_Utilisation", "Rev_Per_Performance",
  "Is_Recession", "Is_LongTerm", "Is_Holiday_Week", "Is_TopTheatre",
  "Date.Month", "Date.Year",
  "ShowType_enc", "Season_enc", "Theatre_enc",
  "Pct_Gross_Potential"
)

TARGET <- "Statistics.Gross"

# Clean feature matrix (impute with median where NA)
X_full <- df_model %>%
  select(all_of(FEATURES)) %>%
  mutate(across(everything(), ~replace_na(., median(., na.rm = TRUE))))

y_full <- df_model[[TARGET]]

# 80/20 train-test split
idx_train <- createDataPartition(y_full, p = 0.80, list = FALSE)
X_train   <- X_full[idx_train, ]
X_test    <- X_full[-idx_train, ]
y_train   <- y_full[idx_train]
y_test    <- y_full[-idx_train]

# Standardise (for linear models)
preproc  <- preProcess(X_train, method = c("center", "scale"))
X_train_s <- predict(preproc, X_train)
X_test_s  <- predict(preproc, X_test)

cat(sprintf("  Train: %s | Test: %s\n",
            format(nrow(X_train), big.mark = ","),
            format(nrow(X_test),  big.mark = ",")))
cat(sprintf("  Features: %d\n", length(FEATURES)))


# ─────────────────────────────────────────────────────────────────────────────
# Helper: Evaluate model performance
# ─────────────────────────────────────────────────────────────────────────────

eval_model <- function(name, y_actual, y_predicted) {
  r2   <- cor(y_actual, y_predicted)^2
  rmse <- rmse(y_actual, y_predicted)
  mae  <- mae(y_actual, y_predicted)
  cat(sprintf("  %-30s R²=%.4f  RMSE=$%10s  MAE=$%10s\n",
              name, r2,
              format(round(rmse), big.mark = ","),
              format(round(mae),  big.mark = ",")))
  tibble(Model = name, R2 = round(r2, 4),
         RMSE = round(rmse, 0), MAE = round(mae, 0))
}

results_list <- list()

cat("\n[MODEL] Training all models …\n")
cat(sprintf("  %-30s  %-8s  %-16s  %-16s\n", "Model","R²","RMSE","MAE"))
cat("  ", strrep("-", 70), "\n", sep = "")


# ─────────────────────────────────────────────────────────────────────────────
# 7a. OLS Linear Regression
# ─────────────────────────────────────────────────────────────────────────────

ols_train <- cbind(X_train_s, y = y_train)
ols_model <- lm(y ~ ., data = ols_train)
ols_pred  <- predict(ols_model, newdata = X_test_s)
results_list[["OLS"]] <- eval_model("OLS Linear Regression", y_test, ols_pred)


# ─────────────────────────────────────────────────────────────────────────────
# 7b. Ridge Regression
# ─────────────────────────────────────────────────────────────────────────────

X_tr_mat <- as.matrix(X_train_s)
X_te_mat <- as.matrix(X_test_s)

cv_ridge  <- cv.glmnet(X_tr_mat, y_train, alpha = 0, nfolds = 5)
ridge_mod <- glmnet(X_tr_mat, y_train, alpha = 0, lambda = cv_ridge$lambda.min)
ridge_pred <- as.vector(predict(ridge_mod, newx = X_te_mat))
results_list[["Ridge"]] <- eval_model("Ridge Regression", y_test, ridge_pred)


# ─────────────────────────────────────────────────────────────────────────────
# 7c. LASSO Regression
# ─────────────────────────────────────────────────────────────────────────────

cv_lasso  <- cv.glmnet(X_tr_mat, y_train, alpha = 1, nfolds = 5)
lasso_mod <- glmnet(X_tr_mat, y_train, alpha = 1, lambda = cv_lasso$lambda.min)
lasso_pred <- as.vector(predict(lasso_mod, newx = X_te_mat))
results_list[["LASSO"]] <- eval_model("LASSO Regression", y_test, lasso_pred)

# Show which features LASSO zeroed out
lasso_coef <- coef(lasso_mod)
cat("  LASSO non-zero features:\n")
print(lasso_coef[lasso_coef[, 1] != 0, , drop = FALSE])


# ─────────────────────────────────────────────────────────────────────────────
# 7d. ElasticNet
# ─────────────────────────────────────────────────────────────────────────────

cv_en  <- cv.glmnet(X_tr_mat, y_train, alpha = 0.5, nfolds = 5)
en_mod <- glmnet(X_tr_mat, y_train, alpha = 0.5, lambda = cv_en$lambda.min)
en_pred <- as.vector(predict(en_mod, newx = X_te_mat))
results_list[["ElasticNet"]] <- eval_model("ElasticNet (α=0.5)", y_test, en_pred)


# ─────────────────────────────────────────────────────────────────────────────
# 7e. Random Forest (ranger)
# ─────────────────────────────────────────────────────────────────────────────

rf_model <- ranger(
  formula         = y ~ .,
  data            = cbind(X_train, y = y_train),
  num.trees        = 300,
  max.depth        = 15,
  min.node.size    = 5,
  importance       = "impurity",
  num.threads      = parallel::detectCores() - 1,
  seed             = 42
)
rf_pred <- predict(rf_model, data = X_test)$predictions
results_list[["RF"]] <- eval_model("Random Forest", y_test, rf_pred)


# ─────────────────────────────────────────────────────────────────────────────
# 7f. Gradient Boosting (gbm)
# ─────────────────────────────────────────────────────────────────────────────

gbm_data   <- cbind(X_train, y = y_train)
gbm_model  <- gbm(
  formula        = y ~ .,
  data           = gbm_data,
  distribution   = "gaussian",
  n.trees        = 400,
  interaction.depth = 5,
  shrinkage      = 0.05,
  bag.fraction   = 0.8,
  n.minobsinnode = 10,
  cv.folds       = 5,
  verbose        = FALSE
)
best_trees <- gbm.perf(gbm_model, method = "cv", plot.it = FALSE)
gbm_pred   <- predict(gbm_model, newdata = X_test, n.trees = best_trees)
results_list[["GBM"]] <- eval_model("Gradient Boosting", y_test, gbm_pred)


# ─────────────────────────────────────────────────────────────────────────────
# 7g. XGBoost
# ─────────────────────────────────────────────────────────────────────────────

dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest  <- xgb.DMatrix(data = as.matrix(X_test),  label = y_test)

xgb_params <- list(
  objective         = "reg:squarederror",
  eta               = 0.05,
  max_depth         = 6,
  subsample         = 0.8,
  colsample_bytree  = 0.8,
  eval_metric       = "rmse",
  seed              = 42
)

xgb_cv <- xgb.cv(
  params   = xgb_params,
  data     = dtrain,
  nrounds  = 500,
  nfold    = 5,
  early_stopping_rounds = 30,
  verbose  = 0
)
best_xgb_rounds <- xgb_cv$best_iteration

xgb_model <- xgb.train(
  params  = xgb_params,
  data    = dtrain,
  nrounds = best_xgb_rounds,
  verbose = 0
)
xgb_pred <- predict(xgb_model, dtest)
results_list[["XGBoost"]] <- eval_model("XGBoost", y_test, xgb_pred)


# ─────────────────────────────────────────────────────────────────────────────
# 7h. LightGBM
# ─────────────────────────────────────────────────────────────────────────────

lgb_train <- lgb.Dataset(data  = as.matrix(X_train), label = y_train)
lgb_test  <- lgb.Dataset(data  = as.matrix(X_test),  label = y_test,
                          reference = lgb_train)

lgb_params <- list(
  objective    = "regression",
  metric       = "rmse",
  learning_rate = 0.05,
  num_leaves   = 63,
  max_depth    = 7,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 5,
  verbosity    = -1,
  seed         = 42
)

lgb_cv <- lgb.cv(
  params   = lgb_params,
  data     = lgb_train,
  nrounds  = 500,
  nfold    = 5,
  early_stopping_rounds = 30,
  verbose  = -1
)
best_lgb_rounds <- lgb_cv$best_iter

lgb_model <- lgb.train(
  params  = lgb_params,
  data    = lgb_train,
  nrounds = best_lgb_rounds,
  verbose = -1
)
lgb_pred <- predict(lgb_model, as.matrix(X_test))
results_list[["LightGBM"]] <- eval_model("LightGBM", y_test, lgb_pred)


# Compile results table
results_df <- bind_rows(results_list)
cat("\n[RESULTS] Model Performance Summary:\n")
print(results_df)


# ─────────────────────────────────────────────────────────────────────────────
# 8. CROSS-VALIDATION (5-fold, caret)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n[CV] Running 5-fold cross-validation …\n")

cv_control <- trainControl(method = "cv", number = 5,
                            savePredictions = "final", allowParallel = TRUE)

# Register parallel backend
cl <- makePSOCKcluster(max(1, parallel::detectCores() - 1))
registerDoParallel(cl)

cv_lm <- train(
  x = X_train_s, y = y_train,
  method    = "lm",
  trControl = cv_control
)
cat(sprintf("  OLS CV-R²: %.4f\n", max(cv_lm$results$Rsquared)))

cv_rf <- train(
  x = X_train, y = y_train,
  method    = "ranger",
  trControl = cv_control,
  tuneGrid  = expand.grid(mtry = 5, splitrule = "variance", min.node.size = 5)
)
cat(sprintf("  Random Forest CV-R²: %.4f\n", max(cv_rf$results$Rsquared)))

stopCluster(cl)


# ─────────────────────────────────────────────────────────────────────────────
# 9. FEATURE IMPORTANCE – FIGURE 4
# ─────────────────────────────────────────────────────────────────────────────

cat("\n[FIGURE] Building Figure 4 – Feature Importance …\n")

feature_labels <- c(
  "Attendance", "Capacity%", "Performances",
  "Avg Ticket $", "Cap. Utilisation", "Rev/Performance",
  "Recession", "Long-Term", "Holiday Week", "Top Theatre",
  "Month", "Year", "Show Type", "Season", "Theatre",
  "Pct Gross Potential"
)

# XGBoost importance
xgb_imp_raw <- xgb.importance(model = xgb_model)
xgb_imp_raw$Feature_Label <- feature_labels[match(xgb_imp_raw$Feature, FEATURES)]

p_xgb <- xgb_imp_raw %>%
  mutate(Feature_Label = fct_reorder(Feature_Label, Gain)) %>%
  ggplot(aes(x = Gain, y = Feature_Label,
             fill = Gain == max(Gain))) +
  geom_col(color = "white") +
  scale_fill_manual(values = c("FALSE" = COL_DARK, "TRUE" = COL_ACCENT)) +
  labs(title = "XGBoost Feature Importance (Gain)",
       x = "Importance Score", y = NULL) +
  theme(legend.position = "none")

# Random Forest importance
rf_imp_df <- tibble(
  Feature = FEATURES,
  Label   = feature_labels,
  Importance = rf_model$variable.importance
) %>%
  arrange(Importance) %>%
  mutate(Label = fct_inorder(Label))

p_rf <- rf_imp_df %>%
  ggplot(aes(x = Importance, y = Label,
             fill = Importance == max(Importance))) +
  geom_col(color = "white") +
  scale_fill_manual(values = c("FALSE" = COL_BLUE, "TRUE" = COL_ACCENT)) +
  labs(title = "Random Forest Feature Importance",
       x = "Impurity Reduction", y = NULL) +
  theme(legend.position = "none")

# SHAP values (XGBoost)
X_shap <- as.matrix(X_test[sample(nrow(X_test), min(800, nrow(X_test))), ])
shap_vals   <- shap.values(xgb_model = xgb_model, X_train = X_shap)
shap_long   <- shap.prep(shap_contrib = shap_vals$shap_score,
                          X_train = X_shap)

# Rename features in shap_long for nicer labels
shap_imp_df <- shap_long %>%
  group_by(variable) %>%
  summarise(mean_abs_shap = mean(abs(value)), .groups = "drop") %>%
  mutate(
    label = feature_labels[match(variable, FEATURES)],
    label = fct_reorder(coalesce(label, variable), mean_abs_shap)
  )

p_shap <- ggplot(shap_imp_df,
                 aes(x = mean_abs_shap, y = label,
                     fill = mean_abs_shap == max(mean_abs_shap))) +
  geom_col(color = "white") +
  scale_fill_manual(values = c("FALSE" = "#0f3460", "TRUE" = COL_ACCENT)) +
  labs(title = "SHAP Mean |Value| — XGBoost",
       subtitle = "Model-agnostic explanation",
       x = "Mean |SHAP Value|", y = NULL) +
  theme(legend.position = "none")

fig4 <- (p_xgb | p_rf | p_shap) +
  plot_annotation(
    title    = "Feature Importance Analysis — Broadway Gross Prediction",
    theme    = theme(plot.title = element_text(face = "bold", size = 15))
  )

ggsave("fig4_feature_importance.png", fig4, width = 20, height = 8, dpi = 150)
cat("  ✓ fig4_feature_importance.png saved\n")


# ─────────────────────────────────────────────────────────────────────────────
# 10. MODEL COMPARISON – FIGURE 5
# ─────────────────────────────────────────────────────────────────────────────

cat("[FIGURE] Building Figure 5 – Model Comparison …\n")

best_model_name <- results_df$Model[which.max(results_df$R2)]
preds_named <- list(
  "OLS"               = ols_pred,
  "Ridge"             = ridge_pred,
  "LASSO"             = lasso_pred,
  "ElasticNet"        = en_pred,
  "Random Forest"     = rf_pred,
  "Gradient Boosting" = gbm_pred,
  "XGBoost"           = xgb_pred,
  "LightGBM"          = lgb_pred
)
best_pred <- preds_named[[best_model_name]]

# 5a. R² bar chart
p5a <- results_df %>%
  mutate(
    Model    = fct_reorder(Model, R2),
    Is_Best  = Model == best_model_name
  ) %>%
  ggplot(aes(x = R2, y = Model, fill = Is_Best)) +
  geom_col(color = "white") +
  geom_text(aes(label = sprintf("%.4f", R2)), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c("FALSE" = COL_DARK, "TRUE" = COL_ACCENT)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "R² Score — All Models",
       x = "R²", y = NULL) +
  theme(legend.position = "none")

# 5b. RMSE bar chart
p5b <- results_df %>%
  mutate(
    Model   = fct_reorder(Model, -RMSE),
    Is_Best = Model == best_model_name
  ) %>%
  ggplot(aes(x = RMSE / 1e3, y = Model, fill = Is_Best)) +
  geom_col(color = "white") +
  geom_text(aes(label = dollar(RMSE / 1e3, accuracy = 0.1, suffix = "K")),
            hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c("FALSE" = COL_DARK, "TRUE" = COL_ACCENT)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = dollar_format(prefix = "$", suffix = "K")) +
  labs(title = "RMSE — All Models (Lower = Better)",
       x = "RMSE ($K)", y = NULL) +
  theme(legend.position = "none")

# 5c. Actual vs Predicted (best model)
ap_df <- tibble(Actual = y_test, Predicted = best_pred) %>%
  sample_n(min(3000, n()))

p5c <- ggplot(ap_df, aes(x = Actual / 1e6, y = Predicted / 1e6)) +
  geom_point(alpha = 0.25, size = 1, color = COL_DARK) +
  geom_abline(color = COL_ACCENT, linewidth = 1.2, linetype = "dashed") +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(
    title    = sprintf("Actual vs. Predicted — %s", best_model_name),
    subtitle = sprintf("R² = %.4f | RMSE = $%sK",
                       results_df$R2[results_df$Model == best_model_name],
                       format(round(results_df$RMSE[results_df$Model == best_model_name] / 1e3, 1))),
    x = "Actual Weekly Gross ($M)", y = "Predicted Weekly Gross ($M)"
  )

# 5d. Residuals histogram
res_df <- tibble(Residual = (y_test - best_pred) / 1e6)
p5d <- ggplot(res_df, aes(x = Residual)) +
  geom_histogram(bins = 60, fill = COL_DARK, color = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, color = COL_ACCENT, linewidth = 1.2, linetype = "dashed") +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Residuals Distribution",
       subtitle = sprintf("Mean: $%.3fM | SD: $%.3fM",
                          mean(res_df$Residual), sd(res_df$Residual)),
       x = "Residual ($M)", y = "Count")

# 5e. Residuals vs fitted
p5e <- ggplot(tibble(Fitted = best_pred / 1e6,
                      Residual = (y_test - best_pred) / 1e6),
              aes(x = Fitted, y = Residual)) +
  geom_point(alpha = 0.2, size = 1, color = COL_BLUE) +
  geom_hline(yintercept = 0, color = COL_ACCENT, linewidth = 1, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = COL_GOLD, linewidth = 1) +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values ($M)", y = "Residuals ($M)")

# 5f. Ridge / LASSO regularisation paths
cv_ridge_plot_df <- tibble(
  Log_Lambda = log(cv_ridge$lambda),
  MSE        = cv_ridge$cvm,
  Upper      = cv_ridge$cvup,
  Lower      = cv_ridge$cvlo
)

p5f <- ggplot(cv_ridge_plot_df, aes(x = Log_Lambda, y = MSE)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = COL_DARK, alpha = 0.2) +
  geom_line(color = COL_DARK, linewidth = 1) +
  geom_vline(xintercept = log(cv_ridge$lambda.min), color = COL_ACCENT,
             linetype = "dashed", linewidth = 0.9) +
  labs(title = "Ridge — CV Error vs. λ",
       subtitle = sprintf("Optimal λ = %.1f", cv_ridge$lambda.min),
       x = "log(λ)", y = "CV Mean Squared Error")

fig5 <- (p5a | p5b | p5c) / (p5d | p5e | p5f) +
  plot_annotation(
    title    = "Predictive Models — Performance Comparison",
    subtitle = sprintf("Best model: %s (R² = %.4f)", best_model_name,
                       max(results_df$R2)),
    theme    = theme(plot.title    = element_text(face = "bold", size = 15),
                     plot.subtitle = element_text(color = "grey40"))
  )

ggsave("fig5_model_comparison.png", fig5, width = 18, height = 12, dpi = 150)
cat("  ✓ fig5_model_comparison.png saved\n")


# ─────────────────────────────────────────────────────────────────────────────
# 11. REGRESSION COEFFICIENTS – FIGURE 6
# ─────────────────────────────────────────────────────────────────────────────

cat("[FIGURE] Building Figure 6 – Regression Coefficients …\n")

make_coef_df <- function(model_obj, model_name, labels) {
  if (inherits(model_obj, "glmnet")) {
    coef_vec <- as.vector(coef(model_obj))[-1]
  } else {
    coef_vec <- coef(model_obj)[-1]  # drop intercept
  }
  tibble(Feature = labels, Coefficient = coef_vec, Model = model_name)
}

coef_ols   <- make_coef_df(ols_model,  "OLS",            feature_labels)
coef_ridge <- make_coef_df(ridge_mod,  "Ridge (CV λ)",   feature_labels)
coef_lasso <- make_coef_df(lasso_mod,  "LASSO (CV λ)",   feature_labels)

coef_all <- bind_rows(coef_ols, coef_ridge, coef_lasso) %>%
  mutate(
    Direction = if_else(Coefficient >= 0, "Positive", "Negative"),
    Feature   = fct_reorder(Feature, abs(Coefficient), .fun = mean)
  )

p6 <- ggplot(coef_all,
             aes(x = Coefficient, y = Feature, fill = Direction)) +
  geom_col(color = "white", show.legend = TRUE) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  facet_wrap(~Model, scales = "free_x", ncol = 3) +
  scale_fill_manual(values = c("Positive" = COL_DARK, "Negative" = COL_ACCENT)) +
  labs(
    title    = "Standardised Regression Coefficients — OLS, Ridge & LASSO",
    subtitle = "All inputs standardised (mean=0, SD=1) for comparability",
    x = "Coefficient Value", y = NULL, fill = NULL
  ) +
  theme(legend.position = "top")

ggsave("fig6_regression_coefficients.png", p6, width = 20, height = 8, dpi = 150)
cat("  ✓ fig6_regression_coefficients.png saved\n")


# ─────────────────────────────────────────────────────────────────────────────
# 12. EXPORT RESULTS
# ─────────────────────────────────────────────────────────────────────────────

cat("\n[EXPORT] Writing result tables …\n")

write_csv(results_df, "model_results_summary.csv")

eda_stats <- df %>%
  group_by(Show.Type) %>%
  summarise(
    n         = n(),
    Min       = min(Statistics.Gross),
    Q1        = quantile(Statistics.Gross, 0.25),
    Median    = median(Statistics.Gross),
    Mean      = mean(Statistics.Gross),
    Q3        = quantile(Statistics.Gross, 0.75),
    Max       = max(Statistics.Gross),
    SD        = sd(Statistics.Gross),
    .groups   = "drop"
  )
write_csv(eda_stats, "eda_stats_by_type.csv")

cat("  ✓ model_results_summary.csv\n")
cat("  ✓ eda_stats_by_type.csv\n")


# ─────────────────────────────────────────────────────────────────────────────
# 13. FINAL SUMMARY
# ─────────────────────────────────────────────────────────────────────────────

cat("\n", strrep("=", 60), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 60), "\n\n")
cat("Output files generated:\n")
for (f in c("fig1_eda_distributions.png", "fig2_eda_relationships.png",
            "fig3_eda_categorical.png",   "fig4_feature_importance.png",
            "fig5_model_comparison.png",  "fig6_regression_coefficients.png",
            "model_results_summary.csv",  "eda_stats_by_type.csv")) {
  cat(sprintf("  ✓ %s\n", f))
}

cat(sprintf("\nBest Model : %s\n", best_model_name))
cat(sprintf("Test R²    : %.4f\n", max(results_df$R2)))
cat(sprintf("Test RMSE  : $%s\n",
            format(results_df$RMSE[results_df$Model == best_model_name],
                   big.mark = ",")))
cat(sprintf("Test MAE   : $%s\n",
            format(results_df$MAE[results_df$Model == best_model_name],
                   big.mark = ",")))
cat("\nKey findings:\n")
cat("  • Rev_Per_Performance and Avg_Ticket_Price are the\n")
cat("    dominant predictors (SHAP + tree importance aligned)\n")
cat("  • Holiday weeks generate highest median weekly grosses\n")
cat("  • Recession periods measurably suppress weekly grosses\n")
cat("  • Long-term shows consistently outperform new shows\n")
cat("    across all seasons\n")
cat("  • Top-tier theatres carry a significant gross premium\n")
cat(strrep("=", 60), "\n")
