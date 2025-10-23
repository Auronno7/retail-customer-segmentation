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