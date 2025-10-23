# Set working directory


# Installing packages

options(repos = c(CRAN = "https://cloud.r-project.org"))  # set CRAN mirror

pkgs <- c("lubridate", "readr", "dplyr", "stringr",
          "janitor", "tidyr", "ggplot2", "purrr",
          "cluster","tibble", "ggrepel")

# install only the ones not already installed
invisible(lapply(setdiff(pkgs, rownames(installed.packages())), install.packages))

# (optional) load them
# invisible(lapply(pkgs, require, character.only = TRUE))

# Load libraries

library(readr)
library(lubridate)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(ggplot2)
library(cluster)
library(purrr)
library(tibble)
library(ggrepel)

# Descriptive statistics

# Load data set from CSV

walmart_data <- read_csv("Walmart.csv", show_col_types = FALSE) |>
  clean_names()  # lower_snake_case
walmart_data

# ---------- minimal cleaning & revenue ----------
# Robust date parser for strings like "3/31/2024 21:46" or "9/13/2024 0:45"

parse_txn_dt <- function(x) {
  lubridate::parse_date_time(
    x,
    orders = c("mdy HMS", "mdy HM", "mdy", "Y-m-d HMS", "Y-m-d HM", "Y-m-d")
  )
}

walmart_descriptive_statistics <- walmart_data |>
  mutate(
    transaction_date = parse_txn_dt(transaction_date),
    customer_id      = as.character(customer_id),
    customer_age     = suppressWarnings(as.numeric(customer_age)),
    customer_gender  = str_to_title(as.character(customer_gender))
  ) |>
  # normalize common gender spellings
  mutate(customer_gender = case_when(
    customer_gender %in% c("M","Male")   ~ "Male",
    customer_gender %in% c("F","Female") ~ "Female",
    TRUE                                  ~ "Other/Unknown"
  ))

# --- 1) Collapse to one row per customer (robust to repeats) ---
# gender = modal (most frequent) non-NA value; age = last observed non-NA by time

mode_non_na <- function(x){
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

customers_descriptive_statistics <- walmart_descriptive_statistics |>
  arrange(customer_id, transaction_date) |>
  group_by(customer_id) |>
  summarise(
    gender = mode_non_na(customer_gender),
    age    = {x <- customer_age[!is.na(customer_age)]; ifelse(length(x)==0, NA, tail(x, 1))},
    .groups = "drop"
  )

# --- 2) Gender counts & percentages ---

gender_summary <- customers_descriptive_statistics |>
  count(gender, name = "n_customers") |>
  mutate(
    pct_customers = round(100 * n_customers / sum(n_customers), 2)
  ) |>
  arrange(desc(n_customers))

print(gender_summary)

# tibble with: gender, n_customers, pct_customers

# --- 3) Age bins: 0–9, 10–19, 20–29, ... (counts + %) ---

max_age <- suppressWarnings(max(customers_descriptive_statistics$age, na.rm = TRUE))
upper   <- 10 * ceiling((max_age + 1) / 10)   # ensures top bin ends with 9, 19, 29, ...
breaks  <- seq(0, upper, by = 10)
labels  <- paste0(breaks[-length(breaks)], "–", breaks[-1] - 1)

age_binned <- customers_descriptive_statistics |>
  mutate(
    age_bin = cut(age, breaks = breaks, right = FALSE,
                  include.lowest = TRUE, labels = labels),
    age_bin = ifelse(is.na(age_bin), "Unknown age", as.character(age_bin)),
    age_bin = factor(age_bin, levels = c(labels, "Unknown age"))
  ) |>
  count(age_bin, name = "n_customers") |>
  mutate(pct_customers = round(100 * n_customers / sum(n_customers), 2)) |>
  arrange(match(age_bin, levels(age_bin)))

print(age_binned)

# tibble with: age_bin, n_customers, pct_customers

# Total revenue by store location

walmart_store <- read_csv("Walmart (Dataset).csv", show_col_types = FALSE) |>
  clean_names() |>
  mutate(
    store_location = str_squish(as.character(store_location)),
    qty  = suppressWarnings(as.numeric(quantity_sold)),
    price = suppressWarnings(as.numeric(unit_price)),
    revenue = qty * price
  )

# Revenue by store location (+ percent of total)

walmart_store_revenue <- walmart_store |>
  mutate(
    store_location = if_else(is.na(store_location) | store_location == "",
                             "Unknown", store_location),
    revenue = coalesce(revenue, 0)
  ) |>
  group_by(store_location) |>
  summarise(total_revenue = sum(revenue, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total_revenue)) |>
  mutate(pct_of_total = round(100 * total_revenue / sum(total_revenue), 2))

print(walmart_store_revenue)

# RFM

rfm_base <- walmart_data %>%
  # keep only the columns we truly need for RFM
  transmute(
    customer_id = as.character(customer_id),
    transaction_date = parse_txn_dt(transaction_date),
    revenue = as.numeric(quantity_sold) * as.numeric(unit_price)
  ) %>%
  # drop rows with missing essentials
  filter(!is.na(customer_id), !is.na(transaction_date)) %>%
  mutate(revenue = ifelse(is.na(revenue), 0, revenue))

# If you want to exclude zero/negative revenue rows, uncomment:
# rfm_base <- rfm_base %>% filter(revenue > 0)

# ---------- 3) customer-level R, F, M ----------

analysis_date <- as.Date(max(rfm_base$transaction_date, na.rm = TRUE)) + 1

rfm_table <- rfm_base %>%
  group_by(customer_id) %>%
  summarise(
    last_purchase = max(transaction_date, na.rm = TRUE),
    frequency     = n(),
    monetary      = sum(revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    recency_days = as.integer(as.Date(analysis_date) - as.Date(last_purchase))
  ) %>%
  select(customer_id, recency_days, frequency, monetary)

# ---------- 4) RFM scores (1–5) ----------
# Recency: lower days = better (so invert with 6 - n tile)

rfm_scored <- rfm_table %>%
  mutate(
    r_score = 6 - ntile(recency_days, 5),
    f_score = ntile(frequency, 5),
    m_score = ntile(monetary, 5),
    rfm_score  = paste0(r_score, f_score, m_score),
    rfm_total  = r_score + f_score + m_score
  )

# Segmentation

rfm_scored <- rfm_scored %>%
  mutate(
    segment = case_when(
      # Loyal: high on all three
      r_score >= 4 & f_score >= 4 & m_score >= 4                    ~ "Loyal",
      
      # New: very recent but still light engagement/spend
      r_score >= 4 & f_score <= 2 & m_score <= 2                    ~ "New Customers",
      
      # Promissing: decent recency with with activity/value
      r_score == 3 & f_score >= 3 & m_score >= 3                    ~ "Promising",  # (label per request)
      
      # Hibernating: stale and low activity/value
      r_score <= 2 & f_score <= 2 & m_score <= 2                    ~ "Hibernating",
      
      # Everything else
      TRUE                                                          ~ "Mixed"
    )
  )

rfm_segment_summary <- rfm_scored %>%
  mutate(segment = replace_na(segment, "Unknown")) %>%   # just in case
  count(segment, name = "n_customers") %>%
  arrange(desc(n_customers)) %>%
  mutate(pct_of_total = round(100 * n_customers / sum(n_customers), 2))

print(rfm_segment_summary)

# Promotion

walmart_promotion <- walmart_data |>
  mutate(
    qty   = suppressWarnings(as.numeric(quantity_sold)),
    price = suppressWarnings(as.numeric(unit_price)),
    revenue = qty * price,
    promotion_type = str_trim(as.character(promotion_type)),
    # normalize promo labels (guard against typos/case)
    promotion_type = case_when(
      str_to_lower(promotion_type) %in% c("bogo", "buy one get one") ~ "BOGO",
      str_to_lower(promotion_type) %in% c("percentage discount","percent discount","% discount") ~ "Percentage Discount",
      is.na(promotion_type) | promotion_type == "" | str_to_lower(promotion_type) == "none" ~ "None",
      TRUE ~ promotion_type
    ),
    store_location = str_squish(as.character(store_location)),
    customer_id = as.character(customer_id)
  )

# ------------------------------------------------------------------------------
# A) PROMOTION-TYPE DESCRIPTIVES (global)
# ------------------------------------------------------------------------------

promo_summary <- walmart_promotion |>
  mutate(revenue = coalesce(revenue, 0)) |>
  group_by(promotion_type) |>
  summarise(
    txns = n(),
    total_revenue = sum(revenue, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    pct_txns    = round(100 * txns / sum(txns), 2),
    pct_revenue = round(100 * total_revenue / sum(total_revenue), 2)
  ) |>
  arrange(desc(txns))

print(promo_summary)

# Columns: promotion_type, txns, pct_txns, total_revenue, pct_revenue

# ------------------------------------------------------------------------------
# B) PROMOTION-TYPE DESCRIPTIVES BY STORE LOCATION
#    (within each store, percent of txns and revenue by promo type)
# ------------------------------------------------------------------------------

promo_by_store <- walmart_promotion |>
  mutate(
    store_location = if_else(is.na(store_location) | store_location == "", "Unknown", store_location),
    revenue = coalesce(revenue, 0)
  ) |>
  group_by(store_location, promotion_type) |>
  summarise(
    txns = n(),
    total_revenue = sum(revenue, na.rm = TRUE),
    .groups = "drop_last"
  ) |>
  mutate(
    pct_txns_in_store    = round(100 * txns / sum(txns), 2),
    pct_revenue_in_store = round(100 * total_revenue / sum(total_revenue), 2)
  ) |>
  ungroup() |>
  arrange(store_location, desc(txns))

print(promo_by_store)

# Example columns: store_location, promotion_type, txns, pct_txns_in_store, total_revenue, pct_revenue_in_store

# Segmentation
# ------------------------------------------------------------------------------
# C) PROMO-BASED SEGMENTATION (Heavy / Medium / Light)
#    Numerator counts ONLY BOGO or Percentage Discount; "None" is excluded.
#    Denominator = ALL transactions for that customer.
# ------------------------------------------------------------------------------

PROMO_SET <- c("BOGO","Percentage Discount")

promo_segments <- walmart_promotion |>
  mutate(
    is_promo = promotion_type %in% PROMO_SET
  ) |>
  group_by(customer_id) |>
  summarise(
    total_txn     = n(),
    promo_txn     = sum(is_promo, na.rm = TRUE),
    promo_txn_rate = if_else(total_txn > 0, promo_txn / total_txn, NA_real_),
    .groups = "drop"
  ) |>
  # ---- choose cutoffs (edit if you want different thresholds) ----
mutate(
  promo_segment = case_when(
    promo_txn_rate >= 0.60 ~ "Heavy Promotion User",
    promo_txn_rate >= 0.20 ~ "Medium Promotion User",
    !is.na(promo_txn_rate) ~ "Light Promotion User",
    TRUE                   ~ "Unknown"
  )
)

# quick distribution of segments

promo_segment_summary <- promo_segments |>
  count(promo_segment, name = "n_customers") |>
  mutate(pct_customers = round(100 * n_customers / sum(n_customers), 2)) |>
  arrange(desc(n_customers))

print(promo_segment_summary)

# (Optional) If you prefer *data-driven* tiers (tertiles), swap the rule above with:
#   mutate(promo_segment = case_when(
#     is.na(promo_txn_rate) ~ "Unknown",
#     TRUE ~ c("Light Promotion User", "Medium Promotion User", "Heavy Promotion User")[ntile(promo_txn_rate, 3)]
#   ))

# Churn rate

# Average recency days

churn_days <- mean(rfm_scored$recency_days)

# Customer churn

CHURN_DAYS <- churn_days

# use the SAME analysis_date you used to compute recency_days

rfm_churn <- rfm_table %>%
  mutate(
    last_purchase = as.Date(analysis_date) - recency_days,
    churn_flag      = recency_days > CHURN_DAYS,
    days_over_churn = pmax(recency_days - CHURN_DAYS, 0L),
    est_churn_date  = as.Date(analysis_date) - recency_days + CHURN_DAYS
  )

rfm_scored <- rfm_scored %>%
  left_join(select(rfm_churn, customer_id, churn_flag, est_churn_date, days_over_churn),
            by = "customer_id")

# Overall churn %

overall_churn <- rfm_churn %>%
  summarise(
    customers = n(),
    churned   = sum(churn_flag, na.rm = TRUE),
    churn_pct = 100 * churned / customers
  )
overall_churn

# By segment customer churn

by_segment <- rfm_scored %>%
  select(customer_id, segment) %>%
  left_join(select(rfm_churn, customer_id, churn_flag), by = "customer_id") %>%
  group_by(segment) %>%
  summarise(n = n(),
            churned = sum(churn_flag, na.rm = TRUE),
            churn_pct = 100 * churned / n,
            .groups = "drop") %>%
  arrange(desc(churn_pct))
by_segment

# ---------- Build one-row-per-customer attributes ----------

churn_store_gender_age <- walmart_data %>%
  mutate(
    transaction_date = parse_txn_dt(transaction_date),
    customer_id      = as.character(customer_id),
    customer_gender  = str_to_title(as.character(customer_gender)),
    customer_gender  = case_when(
      customer_gender %in% c("M","Male")   ~ "Male",
      customer_gender %in% c("F","Female") ~ "Female",
      TRUE                                  ~ "Other/Unknown"
    ),
    customer_age     = suppressWarnings(as.numeric(customer_age)),
    store_location   = str_squish(as.character(store_location))
  ) %>%
  arrange(customer_id, transaction_date) %>%
  group_by(customer_id) %>%
  summarise(
    gender         = mode_non_na(customer_gender),
    age            = {x <- customer_age[!is.na(customer_age)]; ifelse(length(x)==0, NA, tail(x, 1))},
    store_location = mode_non_na(store_location),
    .groups = "drop"
  ) %>%
  mutate(
    store_location = if_else(is.na(store_location) | store_location == "", "Unknown", store_location)
  )

# ---------- Make decade age bins (0–9, 10–19, …) ----------

max_age <- suppressWarnings(max(churn_store_gender_age$age, na.rm = TRUE))
upper   <- 10 * ceiling((max_age + 1) / 10)
breaks  <- seq(0, upper, by = 10)
labels  <- paste0(breaks[-length(breaks)], "–", breaks[-1] - 1)

churn_store_gender_age <- churn_store_gender_age %>%
  mutate(
    age_bin = cut(age, breaks = breaks, right = FALSE,
                  include.lowest = TRUE, labels = labels),
    age_bin = ifelse(is.na(age_bin), "Unknown age", as.character(age_bin)),
    age_bin = factor(age_bin, levels = c(labels, "Unknown age"))
  )

# -------------------- Store location --------------------

churn_by_location <- rfm_churn %>%
  select(customer_id, churn_flag) %>%
  left_join(select(churn_store_gender_age, customer_id, store_location), by = "customer_id") %>%
  mutate(store_location = replace_na(store_location, "Unknown")) %>%
  group_by(store_location) %>%
  summarise(
    n       = n(),
    churned = sum(churn_flag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    churn_rate    = if_else(n > 0, 100 * churned / n, NA_real_),
    total_churned = sum(churned, na.rm = TRUE),           # recycled to all rows
    churn_pct     = if_else(total_churned > 0, 100 * churned / total_churned, NA_real_)
  ) %>%
  select(-total_churned) %>%
  arrange(desc(churn_pct))

churn_by_location

# -------------------- Gender --------------------

churn_by_gender <- rfm_churn %>%
  select(customer_id, churn_flag) %>%
  left_join(select(churn_store_gender_age , customer_id, gender), by = "customer_id") %>%
  mutate(gender = replace_na(gender, "Other/Unknown")) %>%
  group_by(gender) %>%
  summarise(
    n       = n(),
    churned = sum(churn_flag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    churn_rate    = if_else(n > 0, 100 * churned / n, NA_real_),
    total_churned = sum(churned, na.rm = TRUE),
    churn_pct     = if_else(total_churned > 0, 100 * churned / total_churned, NA_real_)
  ) %>%
  select(-total_churned) %>%
  arrange(desc(churn_pct))

churn_by_gender

# -------------------- Age bins --------------------
# Keep age_bin ordering from cust_attrs (factor levels)

age_levels <- if (is.factor(churn_store_gender_age$age_bin)) levels(churn_store_gender_age$age_bin) else NULL

churn_by_agebin <- rfm_churn %>%
  select(customer_id, churn_flag) %>%
  left_join(select(churn_store_gender_age, customer_id, age_bin), by = "customer_id") %>%
  mutate(age_bin = replace_na(as.character(age_bin), "Unknown age"),
         age_bin = factor(age_bin, levels = c(age_levels))) %>%
  group_by(age_bin) %>%
  summarise(
    n       = n(),
    churned = sum(churn_flag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    churn_rate    = if_else(n > 0, 100 * churned / n, NA_real_),
    total_churned = sum(churned, na.rm = TRUE),
    churn_pct     = if_else(total_churned > 0, 100 * churned / total_churned, NA_real_)
  ) %>%
  select(-total_churned) %>%
  { if (!is.null(age_levels)) arrange(., factor(age_bin, levels = age_levels)) else arrange(., age_bin) }

churn_by_agebin

# Hypothesis testing

# transaction-level revenue

tx <- walmart_data |>
  transmute(
    customer_id      = as.character(customer_id),
    transaction_date = parse_txn_dt(transaction_date),
    quantity         = as.numeric(quantity_sold),
    unit_price       = as.numeric(unit_price),
    revenue          = quantity * unit_price,
    customer_income  = as.numeric(customer_income)
  ) |>
  filter(!is.na(customer_id), !is.na(transaction_date))

# customer-level RFM (we only need monetary for this hypothesis)

rfm_customer <- tx |>
  group_by(customer_id) |>
  summarise(
    monetary = sum(revenue, na.rm = TRUE),
    frequency = n(),
    # income can repeat across rows; use median per customer (robust)
    customer_income = median(customer_income, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!is.na(customer_income), is.finite(customer_income))

# OPTIONAL: winsorise extreme spenders if needed (commented)
# q <- quantile(rfm_customer$monetary, c(.01, .99), na.rm = TRUE)
# rfm_customer <- rfm_customer %>% mutate(monetary = pmin(pmax(monetary, q[1]), q[2]))

# We'll keep a log version for parametric tests

rfm_customer <- rfm_customer %>%
  mutate(monetary_log = log1p(monetary))

# ---------- 2) Primary test: Spearman rank correlation ----------
# H0: rho = 0 ; H1 (one-sided): rho > 0  (income increases with monetary)

spearman_res <- cor.test(
  rfm_customer$customer_income,
  rfm_customer$monetary,
  method = "spearman",
  alternative = "greater",
  exact = FALSE
)
spearman_res

# ---------- 3) Complementary test: High vs Low income (Welch t-test) ----------
# Compare top-25% income vs bottom-25% income on log-monetary

qs <- quantile(rfm_customer$customer_income, c(.25, .75), na.rm = TRUE)
two_group <- rfm_customer %>%
  filter(customer_income <= qs[1] | customer_income >= qs[2]) %>%
  mutate(income_group = if_else(customer_income >= qs[2], "High", "Low"))

tt_res <- t.test(monetary_log ~ income_group, data = two_group, alternative = "greater")
tt_res

# Also report a nonparametric confirmatory test

wilcox_res <- wilcox.test(monetary ~ income_group, data = two_group, alternative = "greater")
wilcox_res

# ---------- 4) Simple regression (effect size & controls optional) ----------
# Base model

m1 <- lm(monetary_log ~ customer_income, data = rfm_customer)
summary(m1)

# OPTIONAL: add frequency as a control (sometimes spend co-moves with visits)

m2 <- lm(monetary_log ~ customer_income + log1p(frequency), data = rfm_customer)
summary(m2)

# Tidy one table

list(
  spearman = broom::tidy(spearman_res),
  t_test   = broom::tidy(tt_res),
  wilcox   = broom::tidy(wilcox_res),
  OLS_base = broom::tidy(m1),
  OLS_ctrl = broom::tidy(m2)
)

# K-means clustering

# make a clean 1-row-per-customer table of promo rates

promo_rate <- promo_segments %>%
  transmute(customer_id = as.character(customer_id),
            promo_txn_rate)

# join into your RFM table

rfm_scored_k <- rfm_scored %>%
  mutate(customer_id = as.character(customer_id)) %>%
  left_join(promo_rate, by = "customer_id")

# (optional) if you want NAs → 0 for customers with no promo usage computed

rfm_scored_k <- rfm_scored_k %>%
  mutate(promo_txn_rate = tidyr::replace_na(promo_txn_rate, 0))

# ---------- 1) Build features (RFM scores + behavior) ----------

feat_cols <- c("r_score","f_score","m_score","promo_txn_rate")

dat_feat <- rfm_scored_k %>%
  mutate(
    promo_txn_rate  = tidyr::replace_na(promo_txn_rate, 0)
  ) %>%
  select(customer_id, all_of(feat_cols)) %>%
  mutate(across(all_of(feat_cols), as.numeric)) %>%
  drop_na(all_of(feat_cols))

# Scale (very important for k-means)

X <- scale(dat_feat[, feat_cols])
dX <- dist(X)

# ---------- 2) Score K candidates ----------

set.seed(42)
k_range <- 2:10

# Elbow (Total within-cluster SS)

wss <- sapply(k_range, function(k) kmeans(X, centers = k, nstart = 50)$tot.withinss)

# Silhouette (higher = better separation)

sil <- sapply(k_range, function(k) {
  km <- kmeans(X, centers = k, nstart = 50)
  mean(silhouette(km$cluster, dX)[, 3])
})

# Gap statistic (Tibshirani et al.)

set.seed(123)
gap_obj <- clusGap(X, FUN = kmeans, K.max = max(k_range), B = 50, nstart = 25)
gap_df  <- tibble(
  k   = seq_len(nrow(gap_obj$Tab)),
  gap = gap_obj$Tab[, "gap"],
  se  = gap_obj$Tab[, "SE.sim"]
)

# ---------- 3) The number of optimal clusters ----------

# Heuristic elbow pick = largest drop in WSS

k_elbow <- k_range[which.min(diff(wss)) + 1]

# Silhouette pick

k_sil <- k_range[which.max(sil)]

# Gap statistic pick

k_gap <- with(gap_obj, maxSE(Tab[, "gap"], Tab[, "SE.sim"], method = "Tibs2001SEmax"))

# ---------- 4) Choose a final K (consensus) ----------

cands <- c(k_elbow, k_sil, k_gap)
k_final <- as.integer(names(sort(table(cands), decreasing = TRUE)[1]))
if (sum(cands == k_final) == 1L) k_final <- k_sil  # tie-breaker: prefer silhouette

message(sprintf("Elbow: %d | Silhouette: %d | Gap: %d  ->  FINAL K = %d",
                k_elbow, k_sil, k_gap, k_final))

# ---------- 5) Visuals ----------

elbow_df <- tibble(k = k_range, wss = wss)
sil_df   <- tibble(k = k_range, sil = sil)

p1 <- ggplot(elbow_df, aes(k, wss)) + geom_line() + geom_point() +
  geom_vline(xintercept = k_elbow, linetype = 2) +
  labs(title = "Elbow", x = "k", y = "Total within-cluster SS") + theme_minimal()

p2 <- ggplot(sil_df, aes(k, sil)) + geom_line() + geom_point() +
  geom_vline(xintercept = k_sil, linetype = 2) +
  labs(title = "Average silhouette", x = "k", y = "Silhouette width") + theme_minimal()

p3 <- ggplot(gap_df, aes(k, gap)) + geom_line() + geom_point() +
  geom_errorbar(aes(ymin = gap - se, ymax = gap + se), width = 0.1) +
  geom_vline(xintercept = k_gap, linetype = 2) +
  labs(title = "Gap statistic", x = "k", y = "Gap") + theme_minimal()

print(p1); print(p2); print(p3)

# ---------- 6) Fit final model and (optionally) re plot clusters ----------

set.seed(99)
km <- kmeans(X, centers = k_final, nstart = 50)

df_km <- dat_feat %>% mutate(cluster = factor(km$cluster))

# ---------- 7) Cluster plot re plot clusters ----------

# i) PCA projection (X is your scaled feature matrix)

pc <- prcomp(X, center = FALSE, scale. = FALSE)
var_expl <- round(100 * (pc$sdev^2 / sum(pc$sdev^2))[1:2], 1)

plot_df <- df_km %>%
  mutate(
    cluster = factor(cluster),
    PC1 = pc$x[, 1],
    PC2 = pc$x[, 2]
  )

# ii) convex hulls per cluster (skip clusters with <3 points)

hulls <- plot_df %>%
  group_by(cluster) %>%
  filter(n() >= 3) %>%
  slice(chull(PC1, PC2)) %>%
  ungroup()

# iii) project k-means centers into PC space and build centroid labels

centers_pc <- km$centers %*% pc$rotation
centers_df <- tibble(
  cluster = factor(seq_len(nrow(centers_pc))),
  PC1 = centers_pc[, 1],
  PC2 = centers_pc[, 2],
  label = paste0("Cluster ", cluster)
)

# OPTIONAL: if you have a rule-based `segment` in df_km and want majority names:
# majority_map <- df_km %>%
#   count(cluster, segment, sort = TRUE) %>%
#   group_by(cluster) %>% slice_max(n, with_ties = FALSE) %>%
#   ungroup() %>% transmute(cluster = factor(cluster), label = segment)
# centers_df <- centers_df %>% left_join(majority_map, by = "cluster")

# iv) polished plot

k_used   <- nlevels(plot_df$cluster)
feat_list <- c("r_score","f_score","m_score and promo_txn_rate")

# darker blue canvas

bg_blue <- "#FFFFFF"

# map cluster levels → white, yellow, green (in that order)

levs <- levels(plot_df$cluster)
base_pal <- c("#e88b00", "#1C00c8", "#179C13")  # white, yellow, green
cluster_pal <- setNames(base_pal[seq_along(levs)], levs)

ggplot(plot_df, aes(PC1, PC2)) +
  # soft convex hulls
  geom_polygon(data = hulls, aes(group = cluster, fill = cluster),
               alpha = 0.28, color = NA) +
  # points: filled circles with subtle black outline for visibility
  geom_point(aes(fill = cluster),
             shape = 21, color = scales::alpha("black", 0.5),
             stroke = 0.2, size = 1.9, alpha = 0.85) +
  # centroids
  geom_point(data = centers_df, aes(PC1, PC2), shape = 8, size = 4, color = "black") +
  # centroid labels
  geom_label_repel(
    data = centers_df, aes(PC1, PC2, label = label),
    fill = "white", color = "black", alpha = 0.95,
    label.size = 0, size = 3.6, box.padding = 0.25, point.padding = 0.3,
    show.legend = FALSE, max.overlaps = Inf
  ) +
  # manual palette so clusters are white / yellow / green
  scale_fill_manual(values = cluster_pal, guide = "none") +
  labs(
    title = sprintf("Customer Clustering with K-means (K = %d)", k_used),
    subtitle = sprintf("Features: %s. Stars = Centroids; Shaded Areas = Convex Hulls.",
                       paste(feat_list, collapse = ", ")),
    x = sprintf("Dim1 (%.1f%% variance)", var_expl[1]),
    y = sprintf("Dim2 (%.1f%% variance)", var_expl[2]),
    caption = "Note: RFM scores are discrete (1–5), creating bands. Hulls can overlap in 2D even when clusters separate in the full space."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = scales::alpha("white", 0.45), linewidth = 0.3),
    panel.background = element_rect(fill = bg_blue, color = NA),
    plot.background  = element_rect(fill = bg_blue, color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

# k-means interpretation

profile_raw <- plot_df |>
  dplyr::group_by(cluster) |>
  dplyr::summarise(
    n = dplyr::n(),
    r = mean(r_score), f = mean(f_score), m = mean(m_score),
    promo_txn = mean(promo_txn_rate),
    .groups = "drop"
  )

# z-scores (above/below overall average)

profile_z <- as.data.frame(X) |>
  stats::setNames(c("r","f","m","promo_txn")) |>
  dplyr::mutate(cluster = df_km$cluster) |>
  dplyr::group_by(cluster) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), mean), .groups = "drop")
