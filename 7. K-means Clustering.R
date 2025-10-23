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

# z-scores (above/below overall average)

profile_z <- as.data.frame(X) |>
  stats::setNames(c("r","f","m","promo_txn")) |>
  dplyr::mutate(cluster = df_km$cluster) |>
  dplyr::group_by(cluster) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), mean), .groups = "drop")
