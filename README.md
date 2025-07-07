# ⚽ Global Football Analysis
This project analyzes football match data from leagues around the world to uncover meaningful trends in team performance and match outcomes. Leveraging R and spatial visualization tools, it draws insights from thousands of matches played across dozens of countries.

## 📂 Repository Structure
- `/data/` – Raw and cleaned datasets
- `/scripts/` – R scripts for data wrangling and analysis
- `/figures/` – Visualizations and maps
- `/studies/` – Individual analysis modules (e.g., home win %, league comparisons)
- `/reports/` – Markdown or HTML reports

### 📊 Featured Study: No Place like Home?
**A data-driven look at how impactful home advantage truly is in football.**

This study visualizes the percentage of matches won by the home team across 27 countries.

## 📦 Dependencies / Setup
#### Getting Started
To run the analysis, you'll need:

- R (≥ 4.2)
- R packages: `dplyr`, `ggplot2`, `sf`, `rnaturalearth`, `rnaturalearthhires`, `viridis`, `devtools`

To install the high-resolution map data:
```r
devtools::install_github("ropensci/rnaturalearthhires")

