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