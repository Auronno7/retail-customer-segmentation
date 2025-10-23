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