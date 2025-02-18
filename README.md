# NBA Player Trend Tracker

An Interactive Shiny App for Analyzing NBA Player Performance Against Prop Lines

This Shiny application allows users to explore NBA player performance trends over the most recent two seasons, providing insights into how players perform against custom prop lines. Track points, rebounds, assists, and various combo stats with dynamic visualizations, performance summaries, and downloadable game logs.

---

## Features

- **Interactive Performance Trends:**  
  Track NBA players' performance for points, rebounds, assists, steals, blocks, and turnovers.
  
- **Customizable Prop Lines:**  
  Input a betting line to see how often a player goes over or under.

- **Combo Stats:**  
  Analyze combo statistics like points + assists, points + rebounds, assists + rebounds, and more.

- **Clear Visualizations:**  
  Interactive bar charts with color-coded indicators to easily identify performance relative to the prop line.

- **Timeframe Filtering:**  
  Choose between the last 5, 10, or 20 games, or view season-wide trends for the current or previous season.

- **Leaderboard Tab:**  
  Compare players' average performance across different stats.

- **Download Game Logs:**  
  Export player game logs in CSV or Excel format for further analysis.

- **Excludes All-Star Games:**  
  Automatically filters out All-Star and exhibition games to ensure data accuracy.

---

## Installation Guide

1. **Install R and RStudio** *(if not installed)*:  
   - [Download R](https://cran.r-project.org/)  
   - [Download RStudio](https://www.rstudio.com/products/rstudio/download/)

2. **Download the Project Files**:  
   - Clone or download this repository to your local machine.

3. **Install Required Packages**:  
   Run the provided installation script in RStudio:
   ```markdown
   source("install_packages.R")  

5. Run the Application:
   Open trendNBAapp.R and click Run App in RStudio or run:
   ```markdown
   library(shiny)
   runApp("nba_trend")

---

## How to Use the App


1. **Select a Player:**  
   Use the search box to find any NBA player from the last two seasons.

2. **Choose a Timeframe:**  
   - Last 5, 10, or 20 games  
   - This season  
   - Last season  

3. **Select a Statistic:**  
   Choose from core stats (points, rebounds, assists, steals, blocks, turnovers) or combination stats (e.g., points + assists).

4. **Enter a Betting Line:**  
   Input a numeric prop line to compare the player's historical performance.

5. **Analyze the Trends:**  
   View a bar chart with color-coded bars indicating performance above or below the line.  
   Hover over bars to see exact game details.

6. **Download Game Logs:**  
   Click the download button to save game logs in CSV or Excel format.

7. **Leaderboard Tab:**  
   Compare player averages across different stats to identify top performers.

---

## Important Notes

- The app uses `hoopR` to pull NBA data dynamically. Ensure you have a working internet connection.  
- Data is limited to the last two seasons and excludes exhibition and All-Star games.

---

## Acknowledgments

- Built with [R Shiny](https://shiny.rstudio.com/)  
- NBA data sourced using the [hoopR](https://hoopr.sportsdataverse.org/) package  







