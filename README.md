Project Overview

This repository contains an end-to-end, reproducible pipeline for building an integrated customer segmentation and behavioural analytics framework on Walmart-style transactional data. The project combines RFM (Recency–Frequency–Monetary) scoring and promotion use rate with k-means clustering, promotion-use profiling, churn flags, and hypothesis testing to translate raw sales logs into actionable customer segmentations for targeting, retention, and offer design.

Secured 1st place as a three-member team, competing against 10 other teams at the project show.
I worked on data wrangling, RFM, promotion behaviour, k-means clustering, churn rate, and hypothesis tests (Spearman + simple linear regression) in R (RStudio); visualizations in R, Power BI, and Tableau.

The analysis uses the Walmart.csv file (see Datasets/). Some data may be synthetic/anonymized; insights are presented as decision support and should be validated on operational data before deployment.

Requirements

R ≥ 4.3 and RStudio.
Packages used: tidyverse, lubridate, janitor, cluster, factoextra/ggplot2, etc. (install-on-run inside scripts)

Data Files You Must Have (Important)

The R scripts will not run without Datasets/Walmart.csv.
The Tableau workbooks in Visualizations/ require Datasets/Visualization (Dataset).xlsx.
If Tableau prompts for a missing source: Data Source > Edit Connection > browse to Visualization (Dataset).xlsx.

How to reproduce the analysis

Option A: One-click Run (Mother Script)
1. Ensure Datasets/Walmart.csv exists in your RStudio Working Directory.
2. Open RStudio
3. Run Combined Analysis.R script
This executes all numbered steps that are in the Scripts folder in one go (1…7), creates features, segments, clusters, churn, and hypothesis tests with a single script.

Option B: Run Step-by-step
1. Ensure Datasets/Walmart.csv exists in your RStudio Working Directory.
2. Open RStudio
3. Run each scripts individually in chronological order like I labled them 1. Descriptive Statistics.R, 2. Total Revenue by Store Location.R, 7. K-means Clustering.R etc.
This is useful if you don't need/want to do all the analysis (e.g. you can choose to do RFM (Segmentation) but ignore K-means Clustering)

Opening the Tableau Dashboards

1. Double-click any .twbx in Visualizations/.
2. If Tableau can’t find the data, point it to Visualization (Dataset).xlsx.

Original Data Source

Walmart Dataset (Kaggle): https://www.kaggle.com/datasets/ankitrajmishra/walmart
