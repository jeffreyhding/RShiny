## NBA Player Stat Visualizer
**Description:** Individual NBA player game stats and career progression, updated daily

Interactive web app can be found here: https://jeffreyd.shinyapps.io/nba-players/

This app is a work in progress. I plan on adding more comprehensive filters, advanced stats, pages focused on current season stats, player comparisons, and more! If there's something you'd like to see, feel free to reach out: jhding@berkeley.edu

___

The data is provided by [Eoin Moore](https://www.linkedin.com/in/eoin-moore-a336838/) and can be found here:
https://www.kaggle.com/datasets/eoinamoore/historical-nba-data-and-player-box-scores.

Due to the limitations of Shiny web apps, the dataset used in the published app is filtered to include only NBA players with at least 20,000 career minutes played or a draft year of 2013 or later. If you would like to use the full dataset and run the app yourself in RStudio, follow the instructions below:

<br>

  1. Visit Moore's Kaggle [link](https://www.kaggle.com/datasets/eoinamoore/historical-nba-data-and-player-box-scores) and download the `Players.csv` and `PlayerStatistics.csv` files. The game data is updated at the end of each game day (usually around 1:00 AM PST), so you'll need to redownload the files you want daily to access the most up-to-date stats.
  2. Open up `nba-stats.qmd` and run the first two cells to clean the raw files and save them as new CSVs.
  3. **(OPTIONAL)** Run the third cell to filter out players to match the published Shiny app. If you would like to filter a different set of players, the variable names for the data frame are listed in lines 79-120. Note that each row is a player's statistics from a single game.
  4. Open up `nba-players.R` and check lines 19-20 to make sure you're loading the right CSV. Your app should be good to go!


___

**Notes:**
- The cleaned CSVs for the full and published datasets are named `stats.csv` and `recent-stats.csv`, respectively. Modify lines 139 and 151 in `nba-stats.qmd` and lines 19-20 in `nba-players.R` accordingly if you'd like to save them under a different name.
- The last two cells of `nba-stats.qmd` are for calculating rolling stat totals over players' careers. For reference on how the `cumulative_stat_sum` function is used, see line 255 of `nba-players.R`.
