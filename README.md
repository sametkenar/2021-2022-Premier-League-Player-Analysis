# 2021–2022 Premier League Player Analysis

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![ggplot2](https://img.shields.io/badge/ggplot2-Data%20Visualization-blue?style=for-the-badge)
![Statistics](https://img.shields.io/badge/Statistics-Hypothesis%20Testing-orange?style=for-the-badge)
![Regression](https://img.shields.io/badge/Regression-SLR%20%26%20MLR-green?style=for-the-badge)
![Football Analytics](https://img.shields.io/badge/Football%20Analytics-Premier%20League-purple?style=for-the-badge)

## Overview

This project analyzes player-level performance data from the **2021–2022 English Premier League season** using statistical analysis, visualization, hypothesis testing, regression modeling, and ANOVA.

The Premier League is one of the most competitive football leagues in the world, and individual player statistics can reveal important patterns about team performance, attacking contribution, playing time, discipline, and positional differences.

The main goal of this project is to investigate whether player statistics such as goals, assists, expected goals, expected assists, red cards, yellow cards, age, minutes played, and position are statistically related to player and team performance during the 2021–2022 Premier League season.

---

## Project Objectives

This project aims to answer questions such as:

- How are Premier League players distributed across teams, nations, and positions?
- Do champion team players have higher expected goals than players from other teams?
- Are Manchester City and Liverpool players significantly different in terms of expected assists?
- Do more than 50% of players play more than 1,000 minutes?
- Are player position distributions different between top-half and bottom-half teams?
- Do yellow cards differ by player position?
- Are non-penalty expected goals plus assists different across positions?
- How strongly are goals related to expected goals?
- Which player statistics explain matches played or starting appearances?

---

## Dataset

The project uses the `Football_Player_Stats.csv` dataset, which contains player-level statistics for the 2021–2022 Premier League season.

### Main Variables

| Variable | Description |
|---|---|
| `Player` | Player name |
| `Team` | Premier League club |
| `Nation` | Player nationality |
| `Pos` | Player position |
| `Age` | Player age |
| `MP` | Matches played |
| `Starts` | Number of starts |
| `Min` | Minutes played |
| `90s` | Full 90-minute equivalents |
| `Gls` | Goals |
| `Ast` | Assists |
| `G-PK` | Non-penalty goals |
| `PK` | Penalty goals |
| `PKatt` | Penalty attempts |
| `CrdY` | Yellow cards |
| `CrdR` | Red cards |
| `xG` | Expected goals |
| `npxG` | Non-penalty expected goals |
| `xA` | Expected assists |
| `npxG+xA` | Non-penalty expected goals plus expected assists |

---

## Methods Used

The analysis applies several statistical and data visualization methods:

- Descriptive statistics
- Bar charts and boxplots
- Data grouping and aggregation
- One-sample and two-sample hypothesis tests
- Z-tests for means and proportions
- Simple Linear Regression
- Multiple Linear Regression
- One-Way ANOVA
- Model diagnostics
- Correlation analysis

---

## Repository Structure

```text
.
├── README.md
├── Football_Player_Stats.csv
├── 2021_2022_Premier_League_Player_Analysis.pdf
├── Descriptive_of_Categoricals.R
├── Hypothesis_Testings_1_2.R
├── Hypothesis_Testings_3_4.R
├── Anova_1.R
├── Anova_2.R
├── SLR_and_MLR.R
├── Plots/
└── .Rhistory
