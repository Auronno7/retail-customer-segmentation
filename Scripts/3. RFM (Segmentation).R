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

# RFM

# 1) Load data set from CSV

walmart_data <- read_csv("Walmart.csv", show_col_types = FALSE) |>
  clean_names()  # lower_snake_case
walmart_data

# ---------- 2) minimal cleaning & revenue ----------
# Robust date parser for strings like "3/31/2024 21:46" or "9/13/2024 0:45"

parse_txn_dt <- function(x) {
  lubridate::parse_date_time(
    x,
    orders = c("mdy HMS", "mdy HM", "mdy", "Y-m-d HMS", "Y-m-d HM", "Y-m-d")
  )
}

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
