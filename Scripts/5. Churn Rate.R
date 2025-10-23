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

# Churn rate

# Customer churn

CHURN_DAYS <- 90

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
