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

# Total revenue by store location

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