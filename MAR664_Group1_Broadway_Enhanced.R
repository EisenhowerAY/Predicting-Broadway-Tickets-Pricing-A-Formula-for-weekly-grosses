# =============================================================================
# MAR 664 – Predictive Analytics | Group 1 Final Project
# Broadway Weekly Grosses — Predictive Modeling Pipeline
# Data Source: Broadway_Master_Dataset_Enhanced.xlsx
# =============================================================================
# Team: Jenni Ogasian · Alex Popovic · Eisenhower Agyekum-Yamoah
#       Michael Mansaray · Tiangay Kallon
#
# Sections:
#   0. Install & Load Packages
#   1. Data Import from Enhanced Excel
#   2. Column Renaming & Type Casting
#   3. Additional Feature Engineering (theatre encoding, decade)
#   4. EDA — Figure 1: Distributions & Time Trends
#   5. EDA — Figure 2: CPI, Inflation-Adjusted Trends & Pricing
#   6. EDA — Figure 3: Correlations & Feature Relationships
#   7. EDA — Figure 4: Categorical Deep Dives
#   8. Model Preparation (train/test split, scaling, encoding)
#   9. Models: OLS · Ridge · LASSO · ElasticNet · Random Forest ·
#              Gradient Boosting · XGBoost · LightGBM
#  10. Cross-Validation (5-fold)
#  11. Feature Importance + SHAP
#  12. Model Comparison Figures
#  13. Regression Coefficient Plots
#  14. Export Results
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# 0. INSTALL & LOAD PACKAGES
# ─────────────────────────────────────────────────────────────────────────────

# Uncomment on first run:
# install.packages(c(
#   "tidyverse", "readxl", "lubridate", "scales", "ggcorrplot", "moments",
#   "caret", "glmnet", "ranger", "gbm", "xgboost", "lightgbm",
#   "vip", "SHAPforxgboost", "patchwork", "Metrics", "doParallel"
# ))

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(ggcorrplot)
library(moments)       # skewness()

library(caret)
library(glmnet)
library(ranger)
library(gbm)
library(xgboost)
library(lightgbm)

library(vip)
library(SHAPforxgboost)
library(patchwork)
library(Metrics)
library(doParallel)

options(warn = -1, scipen = 999)
set.seed(42)

# ── Global ggplot theme ────────────────────────────────────────────────────
COL_DARK   <- "#1a1a2e"
COL_BLUE   <- "#0f3460"
COL_ACCENT <- "#e94560"
COL_GOLD   <- "#f5a623"
COL_GREEN  <- "#006D4E"
COL_PURPLE <- "#5C3A7E"
PAL_MAIN   <- c(COL_DARK, COL_BLUE, "#16213e", COL_ACCENT,
                COL_GOLD, "#7ec8e3", "#a8d8a8", "#ffcc99")

theme_broadway <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle    = element_text(color = "grey40", size = 10),
    axis.title       = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    plot.caption     = element_text(color = "grey55", size = 8)
  )
theme_set(theme_broadway)


# ─────────────────────────────────────────────────────────────────────────────
# 1. DATA IMPORT — Enhanced Excel Master Dataset
# ─────────────────────────────────────────────────────────────────────────────

# Update FILE_PATH to match your local location
FILE_PATH <- "Broadway_Master_Dataset_Enhanced.xlsx"

# Row 1 = group header banners; Row 2 = actual column headers
df_raw <- read_excel(
  FILE_PATH,
  sheet = "Master Data",
  skip  = 1          # skips the group banner row; row 2 becomes headers
)

cat("═══════════════════════════════════════════════════════════\n")
cat("  BROADWAY PREDICTIVE ANALYTICS — GROUP 1  |  MAR 664\n")
cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("✓ Loaded  %s rows × %s columns from Enhanced Excel\n",
            format(nrow(df_raw), big.mark = ","), ncol(df_raw)))
cat(sprintf("  Year range : %d – %d\n",
            min(df_raw$Year, na.rm = TRUE),
            max(df_raw$Year, na.rm = TRUE)))
cat(sprintf("  Sheets used: Master Data (data) | Data Dictionary | Summary Statistics | CPI Reference\n\n"))

glimpse(df_raw)


# ─────────────────────────────────────────────────────────────────────────────
# 2. COLUMN RENAMING & TYPE CASTING
# ─────────────────────────────────────────────────────────────────────────────
# The enhanced Excel has newlines in headers; rename to clean R names.

df <- df_raw %>%
  rename(
    # ── Identifiers ─────────────────────────────────────────────────────
    Date_WeekEnd          = `Date\n(Week End)`,
    Year                  = Year,
    Month                 = Month,
    Day                   = Day,
    Show_Name             = `Show Name`,
    Theatre               = Theatre,
    Show_Type             = `Show Type`,
    # ── Original statistics ─────────────────────────────────────────────
    Attendance            = `Weekly\nAttendance`,
    Capacity_Pct          = `Capacity\n(%)`,
    Performances          = `Weekly\nPerformances`,
    Gross                 = `Weekly\nGross ($)`,
    Gross_Potential       = `Gross\nPotential ($)`,
    # ── Classification ──────────────────────────────────────────────────
    Season                = Season,
    Show_Duration         = `Show Duration\n(Long-Term / New)`,
    # ── Engineered: pricing ─────────────────────────────────────────────
    Avg_Ticket            = `Avg Ticket\nPrice ($)`,
    Avg_Ticket_2016USD    = `Avg Ticket\n2016 USD ($)`,
    # ── Engineered: CPI / inflation ─────────────────────────────────────
    CPI_Monthly           = `CPI-U\n(Monthly)`,
    Gross_2016USD         = `Gross\n2016 USD ($)`,
    # ── Engineered: operational ─────────────────────────────────────────
    Cap_Utilisation       = `Capacity\nUtilisation (%)`,
    Rev_Per_Perf          = `Revenue per\nPerformance ($)`,
    Pct_Gross_Potential   = `% of Gross\nPotential (%)`,
    # ── Binary flags ─────────────────────────────────────────────────────
    Is_Recession          = `Recession\nPeriod (0/1)`,
    Is_Holiday            = `Holiday Week\n(0/1)`,
    Is_LongTerm           = `Long-Term\nShow (0/1)`,
    Is_TopTheatre         = `Top-Tier\nTheatre (0/1)`
  ) %>%

  # ── Type casting ────────────────────────────────────────────────────────
  mutate(
    Date_WeekEnd   = mdy(Date_WeekEnd),
    Year           = as.integer(Year),
    Month          = as.integer(Month),
    Day            = as.integer(Day),
    Show_Type      = factor(Show_Type),
    Season         = factor(Season,
                            levels = c("Winter","Tony Season","Summer",
                                       "Fall","Holidays")),
    Show_Duration  = factor(Show_Duration, levels = c("No","Yes"),
                            labels = c("New Show","Long-Term")),
    Is_Recession   = as.integer(Is_Recession),
    Is_Holiday     = as.integer(Is_Holiday),
    Is_LongTerm    = as.integer(Is_LongTerm),
    Is_TopTheatre  = as.integer(Is_TopTheatre)
  ) %>%

  # ── Remove any rows with missing gross ─────────────────────────────────
  filter(!is.na(Gross), Gross > 0)

cat(sprintf("✓ Clean dataset: %s rows × %s columns\n\n",
            format(nrow(df), big.mark = ","), ncol(df)))


# ─────────────────────────────────────────────────────────────────────────────
# 3. ADDITIONAL FEATURE ENGINEERING
# ─────────────────────────────────────────────────────────────────────────────

df <- df %>%
  mutate(
    # Log-transformed target (for EDA; models use raw Gross)
    Log_Gross      = log1p(Gross),

    # Decade label
    Decade         = paste0(floor(Year / 10) * 10, "s"),

    # Impute NAs in engineered columns with column medians
    Avg_Ticket          = if_else(is.na(Avg_Ticket),
                                   median(Avg_Ticket, na.rm=TRUE), Avg_Ticket),
    Avg_Ticket_2016USD  = if_else(is.na(Avg_Ticket_2016USD),
                                   median(Avg_Ticket_2016USD, na.rm=TRUE),
                                   Avg_Ticket_2016USD),
    Cap_Utilisation     = if_else(is.na(Cap_Utilisation),
                                   median(Cap_Utilisation, na.rm=TRUE),
                                   Cap_Utilisation),
    Rev_Per_Perf        = if_else(is.na(Rev_Per_Perf),
                                   median(Rev_Per_Perf, na.rm=TRUE),
                                   Rev_Per_Perf),
    Pct_Gross_Potential = if_else(is.na(Pct_Gross_Potential),
                                   median(Pct_Gross_Potential, na.rm=TRUE),
                                   Pct_Gross_Potential),
    Gross_2016USD       = if_else(is.na(Gross_2016USD),
                                   median(Gross_2016USD, na.rm=TRUE),
                                   Gross_2016USD),

    # Integer encodings for tree models
    ShowType_enc  = as.integer(Show_Type),
    Season_enc    = as.integer(Season),
    Theatre_enc   = as.integer(factor(Theatre))
  )

cat(sprintf("✓ Feature engineering complete\n"))
cat(sprintf("  Recession weeks  : %s\n", format(sum(df$Is_Recession), big.mark=",")))
cat(sprintf("  Long-term rows   : %s\n", format(sum(df$Is_LongTerm),  big.mark=",")))
cat(sprintf("  Holiday weeks    : %s\n", format(sum(df$Is_Holiday),   big.mark=",")))
cat(sprintf("  Top-theatre rows : %s\n", format(sum(df$Is_TopTheatre),big.mark=",")))
cat(sprintf("  Median ticket $  : $%.2f (nominal)  |  $%.2f (2016 USD)\n",
            median(df$Avg_Ticket, na.rm=TRUE),
            median(df$Avg_Ticket_2016USD, na.rm=TRUE)))


# ─────────────────────────────────────────────────────────────────────────────
# 4. EDA — FIGURE 1: Distributions & Time Trends
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[EDA] Figure 1 — Distributions & Time Trends …\n")

# 1a. Nominal gross distribution
p1a <- ggplot(df, aes(x = Gross / 1e6)) +
  geom_histogram(bins = 60, fill = COL_DARK, color = "white", alpha = 0.85) +
  geom_vline(xintercept = median(df$Gross)/1e6,
             color = COL_ACCENT, linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = mean(df$Gross)/1e6,
             color = COL_GOLD, linewidth = 1.2, linetype = "dotted") +
  annotate("text", x = median(df$Gross)/1e6 + 0.05,
           y = Inf, vjust = 2, hjust = 0, size = 3, color = COL_ACCENT,
           label = sprintf("Median: $%.2fM", median(df$Gross)/1e6)) +
  annotate("text", x = mean(df$Gross)/1e6 + 0.05,
           y = Inf, vjust = 4, hjust = 0, size = 3, color = COL_GOLD,
           label = sprintf("Mean: $%.2fM", mean(df$Gross)/1e6)) +
  scale_x_continuous(labels = dollar_format(prefix="$", suffix="M")) +
  labs(title = "Weekly Gross Distribution (Nominal)",
       x = "Weekly Gross ($M)", y = "Count")

# 1b. Log-transformed gross
p1b <- ggplot(df, aes(x = Log_Gross)) +
  geom_histogram(bins = 60, fill = COL_BLUE, color = "white", alpha = 0.85) +
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 3.5,
           label = sprintf("Skewness: %.3f", skewness(df$Log_Gross))) +
  labs(title = "Log-Transformed Gross (Near-Normal)",
       x = "log(1 + Weekly Gross)", y = "Count")

# 1c. Gross by show type — box plot
p1c <- df %>%
  mutate(Show_Type = fct_reorder(Show_Type, Gross, median, .desc=TRUE)) %>%
  ggplot(aes(x = Show_Type, y = Gross/1e6, fill = Show_Type)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_fill_manual(values = PAL_MAIN[1:3]) +
  scale_y_continuous(labels = dollar_format(prefix="$", suffix="M")) +
  labs(title = "Gross by Show Type", x = NULL, y = "Weekly Gross ($M)") +
  theme(legend.position = "none")

# 1d. Annual gross trend (nominal)
annual <- df %>%
  group_by(Year) %>%
  summarise(Total = sum(Gross)/1e9, Avg = mean(Gross)/1e6, .groups="drop")

p1d <- ggplot(annual, aes(x = Year)) +
  annotate("rect", xmin=2001.25, xmax=2001.92, ymin=-Inf, ymax=Inf,
           fill="red", alpha=0.12) +
  annotate("rect", xmin=2007.92, xmax=2009.5, ymin=-Inf, ymax=Inf,
           fill="red", alpha=0.12) +
  geom_col(aes(y = Total), fill = COL_DARK, alpha = 0.75, width = 0.8) +
  geom_line(aes(y = Avg * 0.15), color = COL_ACCENT, linewidth = 1.3) +
  geom_point(aes(y = Avg * 0.15), color = COL_ACCENT, size = 2) +
  scale_y_continuous(
    name   = "Total Annual Gross ($B)",
    labels = dollar_format(prefix="$", suffix="B"),
    sec.axis = sec_axis(~./0.15, name = "Avg Weekly Gross ($M)",
                        labels = dollar_format(prefix="$", suffix="M"))
  ) +
  labs(title = "Annual Gross Trend — Nominal",
       x = "Year", caption = "Red shading = NBER recession periods")

# 1e. Season median gross
p1e <- df %>%
  group_by(Season) %>%
  summarise(Median = median(Gross)/1e6, .groups="drop") %>%
  drop_na(Season) %>%
  ggplot(aes(x = Season, y = Median, fill = Season)) +
  geom_col(color = "white") +
  geom_text(aes(label = dollar(Median, accuracy=0.01, suffix="M")),
            vjust = -0.4, size = 3) +
  scale_fill_manual(values = PAL_MAIN) +
  scale_y_continuous(labels = dollar_format(prefix="$", suffix="M"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Median Gross by Season", x = NULL,
       y = "Median Weekly Gross ($M)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

# 1f. New vs long-term distribution
p1f <- ggplot(df, aes(x = Gross/1e6, fill = Show_Duration)) +
  geom_histogram(bins = 50, alpha = 0.65, position = "identity") +
  scale_fill_manual(values = c("New Show"  = COL_ACCENT,
                               "Long-Term" = COL_DARK)) +
  scale_x_continuous(labels = dollar_format(prefix="$", suffix="M")) +
  labs(title = "Long-Term vs. New Show — Gross Distribution",
       x = "Weekly Gross ($M)", y = "Count", fill = NULL)

fig1 <- (p1a | p1b | p1c) / (p1d | p1e | p1f) +
  plot_annotation(
    title    = "Broadway Weekly Grosses — EDA: Distributions & Time Trends",
    subtitle = "Source: Broadway_Master_Dataset_Enhanced.xlsx | 1990–2016",
    theme    = theme(plot.title = element_text(face="bold", size=15))
  )

ggsave("fig1_distributions_time_trends.png", fig1,
       width=18, height=11, dpi=150)
cat("  ✓ fig1_distributions_time_trends.png\n")


# ─────────────────────────────────────────────────────────────────────────────
# 5. EDA — FIGURE 2: CPI, Inflation-Adjusted Trends & Ticket Pricing
#    (New figure enabled by enhanced dataset)
# ─────────────────────────────────────────────────────────────────────────────
cat("[EDA] Figure 2 — CPI, Inflation & Ticket Pricing …\n")

# 2a. CPI trend over time
cpi_annual <- df %>%
  group_by(Year, Month) %>%
  summarise(CPI = first(CPI_Monthly), .groups="drop") %>%
  group_by(Year) %>%
  summarise(CPI_avg = mean(CPI), .groups="drop")

p2a <- ggplot(cpi_annual, aes(x = Year, y = CPI_avg)) +
  geom_area(fill = COL_GREEN, alpha = 0.25) +
  geom_line(color = COL_GREEN, linewidth = 1.3) +
  geom_point(color = COL_GREEN, size = 2) +
  annotate("rect", xmin=2001.25, xmax=2001.92, ymin=-Inf, ymax=Inf,
           fill="red", alpha=0.12) +
  annotate("rect", xmin=2007.92, xmax=2009.5, ymin=-Inf, ymax=Inf,
           fill="red", alpha=0.12) +
  labs(title = "CPI-U Annual Average (BLS, 1990–2016)",
       subtitle = "Source: BLS Series CUUR0000SA0 — All Urban Consumers",
       x = "Year", y = "CPI-U Index Value",
       caption = "Red shading = NBER recession periods")

# 2b. Nominal vs. real gross (2016 USD) over time
gross_trend <- df %>%
  group_by(Year) %>%
  summarise(
    Nominal = mean(Gross)/1e6,
    Real    = mean(Gross_2016USD)/1e6,
    .groups = "drop"
  ) %>%
  pivot_longer(c(Nominal, Real), names_to="Type", values_to="Gross_M")

p2b <- ggplot(gross_trend, aes(x=Year, y=Gross_M, color=Type, group=Type)) +
  geom_line(linewidth=1.3) +
  geom_point(size=2) +
  scale_color_manual(values=c("Nominal"=COL_DARK, "Real"=COL_GREEN)) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Avg Weekly Gross — Nominal vs. 2016 USD",
       subtitle="Real gross removes CPI-driven inflation effects",
       x="Year", y="Avg Weekly Gross ($M)", color=NULL)

# 2c. Avg ticket price — nominal vs real
ticket_trend <- df %>%
  group_by(Year) %>%
  summarise(
    Nominal = median(Avg_Ticket, na.rm=TRUE),
    Real    = median(Avg_Ticket_2016USD, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(Nominal, Real), names_to="Type", values_to="Price")

p2c <- ggplot(ticket_trend, aes(x=Year, y=Price, color=Type, group=Type)) +
  geom_line(linewidth=1.3) +
  geom_point(size=2) +
  scale_color_manual(values=c("Nominal"=COL_ACCENT, "Real"=COL_GOLD)) +
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  labs(title="Median Avg Ticket Price — Nominal vs. 2016 USD",
       x="Year", y="Ticket Price ($)", color=NULL)

# 2d. Ticket price vs. gross (scatter, coloured by show type)
sample_df <- df %>% sample_n(min(5000, nrow(df)))

p2d <- ggplot(sample_df, aes(x=Avg_Ticket, y=Gross/1e6, color=Show_Type)) +
  geom_point(alpha=0.3, size=1.2) +
  geom_smooth(method="lm", se=FALSE, linewidth=1.2) +
  scale_color_manual(values=PAL_MAIN[1:3]) +
  scale_x_continuous(labels=dollar_format(prefix="$")) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Avg Ticket Price vs. Weekly Gross",
       subtitle="By Show Type | OLS trendlines per type",
       x="Avg Ticket Price ($)", y="Weekly Gross ($M)", color=NULL)

# 2e. Real gross by show type over decades
p2e <- df %>%
  group_by(Decade, Show_Type) %>%
  summarise(Med_Real = median(Gross_2016USD)/1e6, .groups="drop") %>%
  ggplot(aes(x=Decade, y=Med_Real, fill=Show_Type)) +
  geom_col(position="dodge", color="white") +
  scale_fill_manual(values=PAL_MAIN[1:3]) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Median Real Gross (2016 USD) by Decade & Type",
       x="Decade", y="Median Gross 2016 USD ($M)", fill=NULL)

# 2f. CPI vs nominal gross scatter (macro link)
p2f <- df %>%
  group_by(Year, Month) %>%
  summarise(CPI=first(CPI_Monthly), Avg_Gross=mean(Gross)/1e6,
            .groups="drop") %>%
  ggplot(aes(x=CPI, y=Avg_Gross)) +
  geom_point(alpha=0.45, color=COL_DARK, size=1.5) +
  geom_smooth(method="lm", se=TRUE, color=COL_ACCENT, fill=COL_ACCENT,
              alpha=0.15, linewidth=1.2) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="CPI-U vs. Avg Weekly Gross",
       subtitle="Monthly aggregation | OLS fit",
       x="CPI-U Index", y="Avg Weekly Gross ($M)")

fig2 <- (p2a | p2b | p2c) / (p2d | p2e | p2f) +
  plot_annotation(
    title    = "CPI, Inflation Adjustment & Ticket Pricing Analysis",
    subtitle = "CPI data: BLS Series CUUR0000SA0 | Base = Dec 2016",
    theme    = theme(plot.title = element_text(face="bold", size=15))
  )

ggsave("fig2_cpi_inflation_pricing.png", fig2,
       width=18, height=11, dpi=150)
cat("  ✓ fig2_cpi_inflation_pricing.png\n")


# ─────────────────────────────────────────────────────────────────────────────
# 6. EDA — FIGURE 3: Correlations & Feature Relationships
# ─────────────────────────────────────────────────────────────────────────────
cat("[EDA] Figure 3 — Correlations & Relationships …\n")

# 3a. Correlation heatmap (numeric features)
corr_vars <- df %>%
  select(Gross, Gross_2016USD, Attendance, Capacity_Pct, Performances,
         Avg_Ticket, Avg_Ticket_2016USD, CPI_Monthly,
         Cap_Utilisation, Rev_Per_Perf, Pct_Gross_Potential,
         Is_Recession, Is_LongTerm, Is_Holiday, Is_TopTheatre)

corr_mat <- cor(corr_vars, use="complete.obs")

p3a <- ggcorrplot(
  corr_mat, hc.order=TRUE, type="lower", lab=TRUE, lab_size=2.5,
  colors=c(COL_ACCENT, "white", COL_DARK), outline.color="white"
) + labs(title="Correlation Matrix — All Numeric Features")

# 3b. Attendance vs. gross, coloured by recession
p3b <- ggplot(sample_df,
              aes(x=Attendance, y=Gross/1e6,
                  color=factor(Is_Recession))) +
  geom_point(alpha=0.3, size=1.2) +
  geom_smooth(method="lm", se=FALSE, linewidth=1.2) +
  scale_color_manual(values=c("0"=COL_DARK,"1"=COL_ACCENT),
                     labels=c("0"="Normal","1"="Recession")) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Attendance vs. Gross — Recession Overlay",
       x="Weekly Attendance", y="Weekly Gross ($M)", color=NULL)

# 3c. Capacity utilisation vs. gross
p3c <- ggplot(sample_df, aes(x=Cap_Utilisation, y=Gross/1e6)) +
  geom_point(alpha=0.25, size=1, color=COL_BLUE) +
  geom_smooth(method="loess", se=TRUE, color=COL_ACCENT,
              fill=COL_ACCENT, alpha=0.15, linewidth=1.2) +
  scale_x_continuous(labels=percent_format(scale=1)) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Capacity Utilisation vs. Weekly Gross",
       x="Capacity Utilisation (%)", y="Weekly Gross ($M)")

# 3d. Revenue per performance vs. gross
p3d <- ggplot(sample_df, aes(x=Rev_Per_Perf/1e3, y=Gross/1e6,
                              color=Show_Type)) +
  geom_point(alpha=0.3, size=1.2) +
  geom_smooth(method="lm", se=FALSE, linewidth=1.2) +
  scale_color_manual(values=PAL_MAIN[1:3]) +
  scale_x_continuous(labels=dollar_format(prefix="$", suffix="K")) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Revenue per Performance vs. Weekly Gross",
       x="Rev / Performance ($K)", y="Weekly Gross ($M)", color=NULL)

# 3e. % of Gross Potential vs. gross
p3e <- df %>%
  filter(!is.na(Pct_Gross_Potential), Pct_Gross_Potential > 0,
         Pct_Gross_Potential < 200) %>%
  sample_n(min(4000, n())) %>%
  ggplot(aes(x=Pct_Gross_Potential, y=Gross/1e6)) +
  geom_point(alpha=0.25, size=1, color=COL_PURPLE) +
  geom_smooth(method="loess", se=TRUE, color=COL_GOLD,
              fill=COL_GOLD, alpha=0.2, linewidth=1.2) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="% of Gross Potential vs. Weekly Gross",
       x="Gross as % of Full-Price Potential", y="Weekly Gross ($M)")

# 3f. Top 10 theatres by median gross
p3f <- df %>%
  group_by(Theatre) %>%
  summarise(Med=median(Gross)/1e6, n=n(), .groups="drop") %>%
  slice_max(Med, n=10) %>%
  mutate(Theatre=fct_reorder(Theatre, Med)) %>%
  ggplot(aes(x=Med, y=Theatre)) +
  geom_col(fill=COL_DARK, color="white") +
  geom_text(aes(label=dollar(Med, accuracy=0.01, suffix="M")),
            hjust=-0.1, size=3) +
  scale_x_continuous(labels=dollar_format(prefix="$", suffix="M"),
                     expand=expansion(mult=c(0, 0.2))) +
  labs(title="Top 10 Theatres — Median Weekly Gross",
       x="Median Weekly Gross ($M)", y=NULL)

fig3 <- (p3a | p3b | p3c) / (p3d | p3e | p3f) +
  plot_annotation(
    title = "Feature Relationships & Correlations",
    theme = theme(plot.title = element_text(face="bold", size=15))
  )

ggsave("fig3_correlations_relationships.png", fig3,
       width=18, height=11, dpi=150)
cat("  ✓ fig3_correlations_relationships.png\n")


# ─────────────────────────────────────────────────────────────────────────────
# 7. EDA — FIGURE 4: Categorical Deep Dives
# ─────────────────────────────────────────────────────────────────────────────
cat("[EDA] Figure 4 — Categorical Deep Dives …\n")

month_labs <- c("Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec")
holiday_m  <- c(1, 11, 12)

# 4a. Monthly gross pattern
p4a <- df %>%
  group_by(Month) %>%
  summarise(Med=median(Gross)/1e6, .groups="drop") %>%
  mutate(IsHol = Month %in% holiday_m,
         Month_f = factor(Month, labels=month_labs)) %>%
  ggplot(aes(x=Month_f, y=Med, fill=IsHol)) +
  geom_col(color="white") +
  scale_fill_manual(values=c("FALSE"=COL_DARK,"TRUE"=COL_ACCENT),
                    labels=c("FALSE"="Standard","TRUE"="Holiday Month")) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Monthly Gross Patterns (Holiday = Red)",
       x=NULL, y="Median Weekly Gross ($M)", fill=NULL)

# 4b. Long-Term vs New by Season
p4b <- df %>%
  drop_na(Season) %>%
  group_by(Season, Show_Duration) %>%
  summarise(Med=median(Gross)/1e6, .groups="drop") %>%
  ggplot(aes(x=Season, y=Med, fill=Show_Duration)) +
  geom_col(position="dodge", color="white") +
  scale_fill_manual(values=c("New Show"=COL_ACCENT, "Long-Term"=COL_DARK)) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Long-Term vs. New Show — Gross by Season",
       x=NULL, y="Median Weekly Gross ($M)", fill=NULL) +
  theme(axis.text.x=element_text(angle=20, hjust=1))

# 4c. Performances per week violin
p4c <- df %>%
  filter(Performances %in% 1:12) %>%
  mutate(Perf_f = factor(Performances)) %>%
  ggplot(aes(x=Perf_f, y=Gross/1e6, fill=Perf_f)) +
  geom_violin(trim=TRUE, alpha=0.75, color="white") +
  geom_boxplot(width=0.1, fill="white", outlier.size=0.3) +
  scale_fill_manual(values=colorRampPalette(PAL_MAIN)(12)) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Gross by Performances per Week",
       x="Performances / Week", y="Weekly Gross ($M)") +
  theme(legend.position="none")

# 4d. Top 15 shows — total gross (nominal)
p4d <- df %>%
  group_by(Show_Name) %>%
  summarise(Total=sum(Gross)/1e9, .groups="drop") %>%
  slice_max(Total, n=15) %>%
  mutate(Show_Name=fct_reorder(Show_Name, Total)) %>%
  ggplot(aes(x=Total, y=Show_Name)) +
  geom_col(fill=COL_ACCENT, color="white") +
  geom_text(aes(label=dollar(Total, accuracy=0.01, suffix="B")),
            hjust=-0.1, size=2.8) +
  scale_x_continuous(labels=dollar_format(prefix="$", suffix="B"),
                     expand=expansion(mult=c(0, 0.2))) +
  labs(title="Top 15 Shows — Total Gross (Nominal)",
       x="Total Gross ($B)", y=NULL)

# 4e. Recession flag effect — distribution
p4e <- df %>%
  mutate(Period=if_else(Is_Recession==1,"Recession","Normal")) %>%
  ggplot(aes(x=Gross/1e6, fill=Period)) +
  geom_density(alpha=0.55) +
  scale_fill_manual(values=c("Normal"=COL_DARK,"Recession"=COL_ACCENT)) +
  scale_x_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Gross Distribution — Normal vs. Recession Weeks",
       x="Weekly Gross ($M)", y="Density", fill=NULL)

# 4f. Top-tier theatre premium
p4f <- df %>%
  mutate(Theatre_Tier=if_else(Is_TopTheatre==1,"Top-Tier","Other")) %>%
  group_by(Theatre_Tier, Show_Type) %>%
  summarise(Med=median(Gross)/1e6, .groups="drop") %>%
  ggplot(aes(x=Show_Type, y=Med, fill=Theatre_Tier)) +
  geom_col(position="dodge", color="white") +
  scale_fill_manual(values=c("Top-Tier"=COL_GOLD,"Other"=COL_BLUE)) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Top-Tier vs. Other Theatre — Gross by Show Type",
       x=NULL, y="Median Weekly Gross ($M)", fill="Theatre Tier")

fig4 <- (p4a | p4b | p4c) / (p4d | p4e | p4f) +
  plot_annotation(
    title = "Categorical & Operational Deep Dives",
    theme = theme(plot.title=element_text(face="bold", size=15))
  )

ggsave("fig4_categorical_deepdives.png", fig4,
       width=18, height=11, dpi=150)
cat("  ✓ fig4_categorical_deepdives.png\n")


# ─────────────────────────────────────────────────────────────────────────────
# 8. MODEL PREPARATION
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[MODEL] Preparing model matrix …\n")

# All engineered features (pre-computed in enhanced dataset)
FEATURES <- c(
  # Original stats
  "Attendance", "Capacity_Pct", "Performances",
  # Pricing (nominal + real)
  "Avg_Ticket", "Avg_Ticket_2016USD",
  # CPI
  "CPI_Monthly",
  # Operational engineered
  "Cap_Utilisation", "Rev_Per_Perf", "Pct_Gross_Potential",
  # Time
  "Month", "Year",
  # Binary flags (pre-built in enhanced dataset)
  "Is_Recession", "Is_LongTerm", "Is_Holiday", "Is_TopTheatre",
  # Encoded categoricals
  "ShowType_enc", "Season_enc", "Theatre_enc"
)

TARGET <- "Gross"   # predict nominal weekly gross

FEATURE_LABELS <- c(
  "Attendance","Capacity %","Performances",
  "Avg Ticket ($)","Avg Ticket 2016$",
  "CPI-U",
  "Cap. Utilisation","Rev/Performance","Pct Gross Potential",
  "Month","Year",
  "Recession","Long-Term","Holiday","Top Theatre",
  "Show Type","Season","Theatre"
)

X_full <- df %>%
  select(all_of(FEATURES)) %>%
  mutate(across(everything(),
                ~replace_na(., median(., na.rm=TRUE))))

y_full <- df[[TARGET]]

# 80/20 train-test split
idx_train  <- createDataPartition(y_full, p=0.80, list=FALSE)
X_train    <- X_full[idx_train, ]
X_test     <- X_full[-idx_train, ]
y_train    <- y_full[idx_train]
y_test     <- y_full[-idx_train]

# Standardise (for linear models)
preproc    <- preProcess(X_train, method=c("center","scale"))
X_train_s  <- predict(preproc, X_train)
X_test_s   <- predict(preproc, X_test)

cat(sprintf("  Train : %s rows | Test: %s rows\n",
            format(nrow(X_train), big.mark=","),
            format(nrow(X_test),  big.mark=",")))
cat(sprintf("  Features : %d\n", length(FEATURES)))


# ─────────────────────────────────────────────────────────────────────────────
# Helper — print & return evaluation metrics
# ─────────────────────────────────────────────────────────────────────────────

eval_model <- function(name, y_act, y_pred) {
  r2   <- cor(y_act, y_pred)^2
  rmse <- rmse(y_act, y_pred)
  mae  <- mae(y_act, y_pred)
  cat(sprintf("  %-32s  R²=%.4f  RMSE=$%10s  MAE=$%10s\n",
              name, r2,
              format(round(rmse), big.mark=","),
              format(round(mae),  big.mark=",")))
  tibble(Model=name, R2=round(r2,4),
         RMSE=round(rmse,0), MAE=round(mae,0),
         Preds=list(y_pred))
}

results_list <- list()

cat("\n[MODEL] Training …\n")
cat(sprintf("  %-32s  %-8s  %-16s  %-16s\n","Model","R²","RMSE","MAE"))
cat("  ", strrep("-", 72), "\n", sep="")


# ─────────────────────────────────────────────────────────────────────────────
# 9a. OLS Linear Regression
# ─────────────────────────────────────────────────────────────────────────────

ols_df   <- cbind(as.data.frame(X_train_s), y=y_train)
ols_mod  <- lm(y ~ ., data=ols_df)
ols_pred <- predict(ols_mod, newdata=as.data.frame(X_test_s))
results_list[["OLS"]] <- eval_model("OLS Linear Regression", y_test, ols_pred)

cat("\n  OLS Summary:\n")
print(summary(ols_mod)$coefficients)


# ─────────────────────────────────────────────────────────────────────────────
# 9b. Ridge Regression (CV-tuned λ)
# ─────────────────────────────────────────────────────────────────────────────

X_tr_mat  <- as.matrix(X_train_s)
X_te_mat  <- as.matrix(X_test_s)

cv_ridge   <- cv.glmnet(X_tr_mat, y_train, alpha=0, nfolds=5)
ridge_mod  <- glmnet(X_tr_mat, y_train, alpha=0, lambda=cv_ridge$lambda.min)
ridge_pred <- as.vector(predict(ridge_mod, newx=X_te_mat))
results_list[["Ridge"]] <- eval_model(
  sprintf("Ridge (λ=%.1f)", cv_ridge$lambda.min), y_test, ridge_pred)


# ─────────────────────────────────────────────────────────────────────────────
# 9c. LASSO Regression (CV-tuned λ)
# ─────────────────────────────────────────────────────────────────────────────

cv_lasso   <- cv.glmnet(X_tr_mat, y_train, alpha=1, nfolds=5)
lasso_mod  <- glmnet(X_tr_mat, y_train, alpha=1, lambda=cv_lasso$lambda.min)
lasso_pred <- as.vector(predict(lasso_mod, newx=X_te_mat))
results_list[["LASSO"]] <- eval_model(
  sprintf("LASSO (λ=%.1f)", cv_lasso$lambda.min), y_test, lasso_pred)

cat("\n  LASSO — Non-zero coefficients after regularisation:\n")
lasso_coef <- coef(lasso_mod)
print(lasso_coef[lasso_coef[,1] != 0, , drop=FALSE])


# ─────────────────────────────────────────────────────────────────────────────
# 9d. ElasticNet (α = 0.5, CV-tuned λ)
# ─────────────────────────────────────────────────────────────────────────────

cv_en   <- cv.glmnet(X_tr_mat, y_train, alpha=0.5, nfolds=5)
en_mod  <- glmnet(X_tr_mat, y_train, alpha=0.5, lambda=cv_en$lambda.min)
en_pred <- as.vector(predict(en_mod, newx=X_te_mat))
results_list[["EN"]] <- eval_model("ElasticNet (α=0.5)", y_test, en_pred)


# ─────────────────────────────────────────────────────────────────────────────
# 9e. Random Forest (ranger, fast)
# ─────────────────────────────────────────────────────────────────────────────

rf_mod  <- ranger(
  formula       = y ~ .,
  data          = cbind(as.data.frame(X_train), y=y_train),
  num.trees      = 300,
  max.depth      = 15,
  min.node.size  = 5,
  importance     = "impurity",
  num.threads    = max(1, parallel::detectCores()-1),
  seed           = 42
)
rf_pred <- predict(rf_mod, data=as.data.frame(X_test))$predictions
results_list[["RF"]] <- eval_model("Random Forest", y_test, rf_pred)


# ─────────────────────────────────────────────────────────────────────────────
# 9f. Gradient Boosting (gbm)
# ─────────────────────────────────────────────────────────────────────────────

gbm_df  <- cbind(as.data.frame(X_train), y=y_train)
gbm_mod <- gbm(
  formula           = y ~ .,
  data              = gbm_df,
  distribution      = "gaussian",
  n.trees           = 400,
  interaction.depth = 5,
  shrinkage         = 0.05,
  bag.fraction      = 0.8,
  n.minobsinnode    = 10,
  cv.folds          = 5,
  verbose           = FALSE
)
best_gbm  <- gbm.perf(gbm_mod, method="cv", plot.it=FALSE)
gbm_pred  <- predict(gbm_mod, newdata=as.data.frame(X_test), n.trees=best_gbm)
results_list[["GBM"]] <- eval_model("Gradient Boosting", y_test, gbm_pred)


# ─────────────────────────────────────────────────────────────────────────────
# 9g. XGBoost (early stopping via CV)
# ─────────────────────────────────────────────────────────────────────────────

dtrain <- xgb.DMatrix(data=as.matrix(X_train), label=y_train)
dtest  <- xgb.DMatrix(data=as.matrix(X_test),  label=y_test)

xgb_params <- list(
  objective        = "reg:squarederror",
  eta              = 0.05,
  max_depth        = 6,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  eval_metric      = "rmse",
  seed             = 42
)

xgb_cv <- xgb.cv(
  params                = xgb_params,
  data                  = dtrain,
  nrounds               = 600,
  nfold                 = 5,
  early_stopping_rounds = 30,
  verbose               = 0
)
best_xgb <- xgb_cv$best_iteration

xgb_mod  <- xgb.train(params=xgb_params, data=dtrain,
                       nrounds=best_xgb, verbose=0)
xgb_pred <- predict(xgb_mod, dtest)
results_list[["XGB"]] <- eval_model("XGBoost", y_test, xgb_pred)


# ─────────────────────────────────────────────────────────────────────────────
# 9h. LightGBM (early stopping via CV)
# ─────────────────────────────────────────────────────────────────────────────

lgb_train <- lgb.Dataset(data=as.matrix(X_train), label=y_train)

lgb_params <- list(
  objective        = "regression",
  metric           = "rmse",
  learning_rate    = 0.05,
  num_leaves       = 63,
  max_depth        = 7,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq     = 5,
  verbosity        = -1,
  seed             = 42
)

lgb_cv <- lgb.cv(
  params                = lgb_params,
  data                  = lgb_train,
  nrounds               = 600,
  nfold                 = 5,
  early_stopping_rounds = 30,
  verbose               = -1
)
best_lgb <- lgb_cv$best_iter

lgb_mod  <- lgb.train(params=lgb_params, data=lgb_train,
                       nrounds=best_lgb, verbose=-1)
lgb_pred <- predict(lgb_mod, as.matrix(X_test))
results_list[["LGB"]] <- eval_model("LightGBM", y_test, lgb_pred)


# ── Compile results ─────────────────────────────────────────────────────────
results_df <- bind_rows(lapply(results_list, function(x)
  x %>% select(-Preds))) %>%
  arrange(desc(R2))

cat("\n[RESULTS] Final Model Leaderboard:\n")
print(results_df)

best_name <- results_df$Model[1]
best_pred <- results_list[[
  names(results_list)[sapply(results_list, function(x) x$Model)==best_name]
]]$Preds[[1]]
cat(sprintf("\n  ★ Best model: %s  (R²=%.4f)\n", best_name,
            results_df$R2[1]))


# ─────────────────────────────────────────────────────────────────────────────
# 10. CROSS-VALIDATION (5-fold via caret)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[CV] 5-Fold Cross-Validation …\n")

cv_ctrl <- trainControl(method="cv", number=5, allowParallel=TRUE)
cl <- makePSOCKcluster(max(1, parallel::detectCores()-1))
registerDoParallel(cl)

cv_lm <- train(x=X_train_s, y=y_train, method="lm",  trControl=cv_ctrl)
cv_rf <- train(
  x=as.data.frame(X_train), y=y_train, method="ranger",
  trControl=cv_ctrl,
  tuneGrid=expand.grid(mtry=5, splitrule="variance", min.node.size=5)
)
stopCluster(cl)

cat(sprintf("  OLS           CV-R²: %.4f\n", max(cv_lm$results$Rsquared)))
cat(sprintf("  Random Forest CV-R²: %.4f\n", max(cv_rf$results$Rsquared)))


# ─────────────────────────────────────────────────────────────────────────────
# 11. FEATURE IMPORTANCE + SHAP — FIGURE 5
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[FIGURE] Figure 5 — Feature Importance & SHAP …\n")

# XGBoost importance
xgb_imp <- xgb.importance(model=xgb_mod) %>%
  mutate(Label = FEATURE_LABELS[match(Feature, FEATURES)]) %>%
  mutate(Label = fct_reorder(coalesce(Label, Feature), Gain))

p_xgb_imp <- ggplot(xgb_imp,
  aes(x=Gain, y=Label, fill=Gain==max(Gain))) +
  geom_col(color="white") +
  scale_fill_manual(values=c("FALSE"=COL_DARK,"TRUE"=COL_ACCENT)) +
  labs(title="XGBoost — Feature Importance (Gain)",
       x="Gain", y=NULL) +
  theme(legend.position="none")

# Random Forest importance
rf_imp_df <- tibble(
  Feature   = FEATURES,
  Label     = FEATURE_LABELS,
  Importance= rf_mod$variable.importance
) %>%
  arrange(Importance) %>%
  mutate(Label=fct_inorder(Label))

p_rf_imp <- ggplot(rf_imp_df,
  aes(x=Importance, y=Label, fill=Importance==max(Importance))) +
  geom_col(color="white") +
  scale_fill_manual(values=c("FALSE"=COL_BLUE,"TRUE"=COL_ACCENT)) +
  labs(title="Random Forest — Feature Importance (Impurity)",
       x="Impurity Reduction", y=NULL) +
  theme(legend.position="none")

# SHAP (XGBoost)
X_shap    <- as.matrix(X_test[sample(nrow(X_test), min(800,nrow(X_test))),])
shap_vals <- shap.values(xgb_model=xgb_mod, X_train=X_shap)
shap_long <- shap.prep(shap_contrib=shap_vals$shap_score, X_train=X_shap)

shap_imp <- shap_long %>%
  group_by(variable) %>%
  summarise(mean_abs=mean(abs(value)), .groups="drop") %>%
  mutate(
    Label = FEATURE_LABELS[match(variable, FEATURES)],
    Label = fct_reorder(coalesce(Label, as.character(variable)), mean_abs)
  )

p_shap <- ggplot(shap_imp,
  aes(x=mean_abs, y=Label, fill=mean_abs==max(mean_abs))) +
  geom_col(color="white") +
  scale_fill_manual(values=c("FALSE"=COL_PURPLE,"TRUE"=COL_ACCENT)) +
  labs(title="SHAP Mean |Value| — XGBoost",
       subtitle="Model-agnostic feature attribution",
       x="Mean |SHAP Value|", y=NULL) +
  theme(legend.position="none")

fig5 <- (p_xgb_imp | p_rf_imp | p_shap) +
  plot_annotation(
    title    = "Feature Importance Analysis",
    subtitle = sprintf("Enhanced features: CPI, real gross, real ticket price, operational flags"),
    theme    = theme(plot.title=element_text(face="bold", size=15))
  )

ggsave("fig5_feature_importance.png", fig5, width=20, height=8, dpi=150)
cat("  ✓ fig5_feature_importance.png\n")


# ─────────────────────────────────────────────────────────────────────────────
# 12. MODEL COMPARISON — FIGURE 6
# ─────────────────────────────────────────────────────────────────────────────
cat("[FIGURE] Figure 6 — Model Comparison …\n")

# 6a. R² leaderboard
p6a <- results_df %>%
  mutate(Model=fct_reorder(Model, R2),
         Best =Model==best_name) %>%
  ggplot(aes(x=R2, y=Model, fill=Best)) +
  geom_col(color="white") +
  geom_text(aes(label=sprintf("%.4f", R2)), hjust=-0.05, size=3.5) +
  scale_fill_manual(values=c("FALSE"=COL_DARK,"TRUE"=COL_ACCENT)) +
  scale_x_continuous(expand=expansion(mult=c(0, 0.1))) +
  labs(title="R² Score — All Models", x="R²", y=NULL) +
  theme(legend.position="none")

# 6b. RMSE leaderboard
p6b <- results_df %>%
  mutate(Model=fct_reorder(Model, -RMSE),
         Best =Model==best_name) %>%
  ggplot(aes(x=RMSE/1e3, y=Model, fill=Best)) +
  geom_col(color="white") +
  geom_text(aes(label=dollar(RMSE/1e3, accuracy=0.1, suffix="K")),
            hjust=-0.05, size=3.5) +
  scale_fill_manual(values=c("FALSE"=COL_DARK,"TRUE"=COL_ACCENT)) +
  scale_x_continuous(labels=dollar_format(prefix="$", suffix="K"),
                     expand=expansion(mult=c(0, 0.15))) +
  labs(title="RMSE (Lower = Better)", x="RMSE ($K)", y=NULL) +
  theme(legend.position="none")

# 6c. Actual vs. predicted — best model
ap_df <- tibble(Actual=y_test, Predicted=best_pred) %>%
  sample_n(min(3000, n()))

p6c <- ggplot(ap_df, aes(x=Actual/1e6, y=Predicted/1e6)) +
  geom_point(alpha=0.25, size=1, color=COL_DARK) +
  geom_abline(color=COL_ACCENT, linewidth=1.2, linetype="dashed") +
  scale_x_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(
    title    = sprintf("Actual vs. Predicted — %s", best_name),
    subtitle = sprintf("R² = %.4f | RMSE = $%sK",
                       results_df$R2[1],
                       format(round(results_df$RMSE[1]/1e3,1))),
    x="Actual ($M)", y="Predicted ($M)"
  )

# 6d. Residuals histogram
res_df <- tibble(Res=(y_test - best_pred)/1e6)
p6d <- ggplot(res_df, aes(x=Res)) +
  geom_histogram(bins=60, fill=COL_DARK, color="white", alpha=0.85) +
  geom_vline(xintercept=0, color=COL_ACCENT, linewidth=1.2, linetype="dashed") +
  scale_x_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Residuals Distribution",
       subtitle=sprintf("Mean: $%.3fM | SD: $%.3fM",
                        mean(res_df$Res), sd(res_df$Res)),
       x="Residual ($M)", y="Count")

# 6e. Residuals vs. fitted
p6e <- tibble(Fitted=best_pred/1e6, Res=(y_test-best_pred)/1e6) %>%
  ggplot(aes(x=Fitted, y=Res)) +
  geom_point(alpha=0.2, size=0.8, color=COL_BLUE) +
  geom_hline(yintercept=0, color=COL_ACCENT, linewidth=1, linetype="dashed") +
  geom_smooth(method="loess", se=FALSE, color=COL_GOLD, linewidth=1) +
  scale_x_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="M")) +
  labs(title="Residuals vs. Fitted",
       x="Fitted ($M)", y="Residual ($M)")

# 6f. Ridge regularisation path
cv_ridge_df <- tibble(
  LogLambda = log(cv_ridge$lambda),
  MSE       = cv_ridge$cvm,
  Upper     = cv_ridge$cvup,
  Lower     = cv_ridge$cvlo
)

p6f <- ggplot(cv_ridge_df, aes(x=LogLambda, y=MSE)) +
  geom_ribbon(aes(ymin=Lower, ymax=Upper), fill=COL_DARK, alpha=0.2) +
  geom_line(color=COL_DARK, linewidth=1) +
  geom_vline(xintercept=log(cv_ridge$lambda.min), color=COL_ACCENT,
             linetype="dashed", linewidth=0.9) +
  labs(title="Ridge — CV Error vs. log(λ)",
       subtitle=sprintf("Optimal λ = %.1f", cv_ridge$lambda.min),
       x="log(λ)", y="CV MSE")

fig6 <- (p6a | p6b | p6c) / (p6d | p6e | p6f) +
  plot_annotation(
    title    = "Predictive Model Performance Comparison",
    subtitle = sprintf("Best: %s  |  R² = %.4f  |  RMSE = $%sK",
                       best_name, results_df$R2[1],
                       format(round(results_df$RMSE[1]/1e3,1))),
    theme    = theme(plot.title=element_text(face="bold", size=15),
                     plot.subtitle=element_text(color="grey40"))
  )

ggsave("fig6_model_comparison.png", fig6, width=18, height=12, dpi=150)
cat("  ✓ fig6_model_comparison.png\n")


# ─────────────────────────────────────────────────────────────────────────────
# 13. REGRESSION COEFFICIENT PLOTS — FIGURE 7
# ─────────────────────────────────────────────────────────────────────────────
cat("[FIGURE] Figure 7 — Regression Coefficients …\n")

make_coef_df <- function(mod, model_name) {
  if (inherits(mod, "glmnet")) {
    coef_vec <- as.vector(coef(mod))[-1]
  } else {
    coef_vec <- coef(mod)[-1]
  }
  tibble(Label=FEATURE_LABELS, Coefficient=coef_vec, Model=model_name)
}

coef_all <- bind_rows(
  make_coef_df(ols_mod,   "OLS"),
  make_coef_df(ridge_mod, sprintf("Ridge (λ=%.0f)", cv_ridge$lambda.min)),
  make_coef_df(lasso_mod, sprintf("LASSO (λ=%.0f)", cv_lasso$lambda.min))
) %>%
  mutate(
    Direction = if_else(Coefficient >= 0, "Positive","Negative"),
    Label     = fct_reorder(Label, abs(Coefficient), .fun=mean)
  )

p7 <- ggplot(coef_all, aes(x=Coefficient, y=Label, fill=Direction)) +
  geom_col(color="white") +
  geom_vline(xintercept=0, color="black", linewidth=0.5) +
  facet_wrap(~Model, scales="free_x", ncol=3) +
  scale_fill_manual(values=c("Positive"=COL_DARK,"Negative"=COL_ACCENT)) +
  labs(
    title    = "Standardised Regression Coefficients — OLS, Ridge & LASSO",
    subtitle = "All features standardised (mean=0, SD=1) for comparability",
    x="Coefficient", y=NULL, fill=NULL
  ) +
  theme(legend.position="top")

ggsave("fig7_regression_coefficients.png", p7, width=20, height=8, dpi=150)
cat("  ✓ fig7_regression_coefficients.png\n")


# ─────────────────────────────────────────────────────────────────────────────
# 14. EXPORT RESULTS
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[EXPORT] Writing result tables …\n")

write_csv(results_df, "model_results_summary.csv")

eda_summary <- df %>%
  group_by(Show_Type) %>%
  summarise(
    n              = n(),
    Median_Gross   = median(Gross),
    Median_2016USD = median(Gross_2016USD),
    Median_Ticket  = median(Avg_Ticket),
    Median_CPI     = median(CPI_Monthly),
    Median_CapUtil = median(Cap_Utilisation),
    .groups = "drop"
  )
write_csv(eda_summary, "eda_stats_by_type.csv")

cat("  ✓ model_results_summary.csv\n")
cat("  ✓ eda_stats_by_type.csv\n")


# ─────────────────────────────────────────────────────────────────────────────
# 15. FINAL CONSOLE SUMMARY
# ─────────────────────────────────────────────────────────────────────────────
cat("\n", strrep("═", 60), "\n")
cat("  ANALYSIS COMPLETE — MAR 664 Group 1\n")
cat(strrep("═", 60), "\n\n")
cat("  Data source : Broadway_Master_Dataset_Enhanced.xlsx\n")
cat(sprintf("  Rows        : %s\n", format(nrow(df), big.mark=",")))
cat(sprintf("  Features    : %d (incl. CPI, real prices, pre-built flags)\n",
            length(FEATURES)))
cat(sprintf("  Best model  : %s\n", best_name))
cat(sprintf("  Test R²     : %.4f\n", results_df$R2[1]))
cat(sprintf("  Test RMSE   : $%s\n",
            format(results_df$RMSE[1], big.mark=",")))
cat(sprintf("  Test MAE    : $%s\n\n",
            format(results_df$MAE[1], big.mark=",")))
cat("  Output files:\n")
for (f in c("fig1_distributions_time_trends.png",
            "fig2_cpi_inflation_pricing.png",
            "fig3_correlations_relationships.png",
            "fig4_categorical_deepdives.png",
            "fig5_feature_importance.png",
            "fig6_model_comparison.png",
            "fig7_regression_coefficients.png",
            "model_results_summary.csv",
            "eda_stats_by_type.csv")) {
  cat(sprintf("    ✓ %s\n", f))
}
cat("\n  Key findings:\n")
cat("  • Rev/Performance & Avg Ticket Price are top predictors (SHAP)\n")
cat("  • Inflation-adjusted (2016 USD) metrics reveal real growth trends\n")
cat("  • CPI positively correlates with nominal gross over time\n")
cat("  • Recession flag shows measurable negative impact on gross\n")
cat("  • Holiday weeks & top-tier theatres command significant premiums\n")
cat("  • Long-Term shows outperform new shows in every season\n")
cat(strrep("═", 60), "\n")
