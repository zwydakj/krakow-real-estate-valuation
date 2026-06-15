# Interactive PropTech Analytics: Kraków Real Estate

## Overview
This repository contains an interactive R/Shiny application designed for real estate valuation and urban spatial analysis in Kraków. It combines statistical modeling with geospatial visualization to analyze property prices and evaluate urban walkability based on the "15-minute city" concept.

## Key Features
* **Property Valuation Model:** Uses statistical modeling (predictive regression) to estimate real estate prices based on property attributes.
* **Urban Walkability Analysis:** Evaluates the "15-minute city" framework by calculating spatial distances to essential urban amenities.
* **Geospatial Visualization:** Features an interactive map of Kraków's administrative districts for visual exploration of the housing market.
* **Interactive UI:** Built with R/Shiny reactive programming to ensure a smooth, user-friendly, and responsive dashboard experience.

## How to Run
1. Ensure you have **R** and **RStudio** installed.
2. Clone this repository to your local machine.
3. Open the project in RStudio.
4. Install required R packages:
   `install.packages(c("shiny", "bslib", "tidyverse", "plotly", "sf", "lubridate", "DT", "shinyjs", "shinyWidgets"))`
5. Open `app.R` and click the **"Run App"** button in RStudio.

## Data Sources
* **Real Estate Listings:** The foundational dataset was sourced from [Kaggle - Apartment Prices in Poland](https://www.kaggle.com/datasets/krzysztofjamroz/apartment-prices-in-poland). The raw data underwent feature engineering to enable the geospatial and PropTech features of this application.
* **Geospatial Boundaries:** The district map of Kraków was sourced from [andilabs/krakow-dzielnice-geojson](https://github.com/andilabs/krakow-dzielnice-geojson) and adapted for the visual mapping components of this project.

---

## Disclaimer
This project is strictly for portfolio and educational purposes.
