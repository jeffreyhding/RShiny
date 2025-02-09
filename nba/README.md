## NBA Player Stat Visualizer
**Description:** individual NBA player game stats and career progression, updated daily

Interactive web app can be found here: https://jeffreyd.shinyapps.io/nba-players/

___

The data is provided by [Eoin Moore](https://www.linkedin.com/in/eoin-moore-a336838/) and can be found 
[here](https://www.kaggle.com/datasets/eoinamoore/historical-nba-data-and-player-box-scores).

Due to the limitations of Shiny web apps, the dataset used in the published app is filtered down to NBA players with at least 20,000 career minutes played or a draft year of 2013 or later. If you would like to use the full dataset and run the app yourself in RStudio, see below:

  1. Visit Moore's Kaggle link [here](https://www.kaggle.com/datasets/eoinamoore/historical-nba-data-and-player-box-scores) and download the `Players.csv` and `PlayerStatistics.csv` files. The game data is updated at the end of each game day, so you'll have to redownload the files you want daily for the most up-to-date stats.
  2. Open up the `nba-stats.qmd` and run the first two cells to clean the raw files.
  3. **OPTIONAL** The third cell cuts down the data to the published version. The last two cells are for calculating rolling stat totals over players' careers. For reference on how the `cumulative_stat_sum` function is used, see line 255 of `nba-players.R`.

