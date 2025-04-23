# 🌐 SPAR & JEE Global Health Security Dashboard

This interactive Shiny dashboard tracks the preparedness and health security performance of WHO Member States using **SPAR** (State Party Self-Assessment Annual Reporting) and **JEE** (Joint External Evaluation) data.

🔗 **[Live App](https://shanlong.shinyapps.io/spar-dashboard/)**

## 🧠 Purpose

To help WHO technical officers, country counterparts, and policy makers:
- Monitor SPAR and JEE scores over time
- Identify gaps and trends in national capacity building
- Visualize global and regional preparedness
- Explore free-text comments and keywords

## 📊 Key Features

- 📍 GIS-based global preparedness maps
- 📈 SPAR & JEE trend comparisons across years
- 📌 Country-specific SPAR/JEE strengths and weaknesses
- 🔍 Wordcloud & keyword analysis on free-text comments
- 🧱 Implementation status, involved areas, and multi-sectoral analysis
- 📂 Downloadable tables and plots for further use

## 📦 Data Sources

- **SPAR datasets (2021–2023)** from WHO IHR Monitoring Team
- **JEE edition 3 scores**
- WHO internal documents and regional submissions
- Manually cleaned free-text comments

## 💻 Tech Stack

- `shiny` `echarts4r` `data.table` `plotly` `wordcloud2`
- `ggmosaic` for categorical data visualization
- `tidyverse` for data wrangling
- `shinyapps.io` for deployment

## 🔧 How to Use

1. Clone the repo
2. Run `app.R` or use `shiny::runApp()` in RStudio
3. Use the sidebar to filter by year, region, country
4. Switch between tabs to explore different aspects of the data

## 🙋 Author

Shanlong Ding  
Data Analyst (P3), WHO WPRO  
✉️ [your.email@who.int]

---

> This project is part of WHO WPRO internal technical product suite. Feedback and collaboration welcome.
