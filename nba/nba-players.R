# Title: NBA Player Stat Visualizer
# Description: individual NBA player stats and career progression
# Author: Jeffrey Ding
# Last update: 6-14-2025

# ===============================================
# Packages
# ===============================================
library(shiny)
library(tidyverse)
library(plotly)   
library(lubridate)  
library(DT)     


# ===============================================
# Data
# ===============================================
# stats <- read.csv("stats.csv")  # uncomment if you're using the full data
stats <- read.csv("recent_stats.csv")
stats = stats |>
  group_by(personId) |> 
  mutate(teamsPlayedFor = paste(unique(playerteamName), collapse = ", ")) |> 
  ungroup() |>
  mutate(fantasyPoints = 
           points +
           threePointersMade +
           -1 * fieldGoalsAttempted +
           2 * fieldGoalsMade +
           -1 * freeThrowsAttempted +
           freeThrowsMade +
           reboundsTotal +
           2 * assists +
           4 * steals +
           4 * blocks +
           -2 * turnovers)

counting_stats <- c("points", "reboundsTotal", "assists", "steals", "blocks", "turnovers", "fieldGoalsMade", "fieldGoalsAttempted",
                    "threePointersMade", "threePointersAttempted", "freeThrowsMade", "freeThrowsAttempted")


# ===============================================
# Functions
# ===============================================
cumulative_stat_sum = function(df, stat) {
  df <- df |>
    mutate(
      temp_col1 = cumsum(ifelse(!is.na(!!sym(stat)), !!sym(stat), 0)),
      temp_col1 = ifelse(is.na(!!sym(stat)), lag(temp_col1), temp_col1)
    ) |>
    mutate(temp_col2 = temp_col1 / careerGamesPlayed)
  
  colnames(df)[colnames(df) == "temp_col1"] <- paste0("career", toupper(str_sub(stat, 1, 1)), str_sub(stat, 2, nchar(stat)))
  colnames(df)[colnames(df) == "temp_col2"] <- paste0(stat, "PerGame")
  
  return(df)
}

default_order <- stats |> 
  group_by(fullName) |> 
  summarise(careerPoints = sum(points, na.rm = TRUE)) |> 
  arrange(desc(careerPoints)) |> 
  pull(fullName)



# ===============================================
# UI
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("NBA Player Stat Visualizer"),
  
  # -------------------------------------------------------
  # Input widgets 
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      
      # Select player
      selectizeInput(inputId = "player",
                     label = "Select player",
                     choices = NULL,
                     selected = "Stephen Curry",
                     multiple = FALSE,  # Disables multiple selections
                     options = list(
                       placeholder = "Choose a player",
                       maxItems = 1,
                       create = FALSE)),  # Disable creating new options
      
      
      # Tab 1: Select timeframe
      conditionalPanel(
        condition = "input.tabselected == 1",
        selectInput(inputId = "timeframe",
                    label = "Select timeframe",
                    choices = c("View recent games", "Select season"),
                    selected = "View recent games"),
        
        # Last x games
        conditionalPanel(
          condition = "input.timeframe == 'View recent games'",
          sliderInput(inputId = "recent_games",
                      label = "Last __ games",
                      min = 5,
                      max = 20,
                      value = 10,
                      step = 1)),
        
        conditionalPanel(
          condition = "input.timeframe == 'Select season'",
          selectizeInput(inputId = "selected_season",
                         label = "Select season",
                         choices = NULL,
                         multiple = FALSE))
      ),
      
      # Tab 2: Select progression type
      conditionalPanel(
        condition = "input.tabselected == 2",
        radioButtons(inputId = "progression_type",
                     label = "Select progression type",
                     choices = c("Rolling career average", "Season-by-season average"),
                     selected = "Rolling career average")
      ),
      
      
      # Select stat
      selectInput(inputId = "stat",
                  label = "Select stat",
                  choices = c("Points" = "points", "3-Pointers" = "threePointersMade", 
                              "Free Throws" = "freeThrowsMade", "Rebounds" = "reboundsTotal", "Assists" = "assists", 
                              "Steals" = "steals", "Blocks" = "blocks", "Turnovers" = "turnovers", "Minutes" = "minutes",
                              "Fantasy Points (ESPN Default)" = "fantasyPoints"),
                  selected = "points"),
      
      
      # Tab 1 Widgets
      conditionalPanel(
        condition = "input.tabselected == 1",
        # Select opponent
        selectizeInput(inputId = "opponent",
                       label = "Select opponent team",
                       choices = NULL,
                       selected = NULL,
                       multiple = FALSE,
                       options = list(
                         placeholder = "Choose an opponent",
                         maxItems = 1,
                         create = FALSE)),
        
        # Select location
        radioButtons(inputId = "location",
                     label = "Filter location",
                     choices = c("All", 
                                 "Home" = TRUE, 
                                 "Away" = FALSE),
                     selected = "All"),
        
        # Select outcome
        radioButtons(inputId = "outcome",
                     label = "Filter outcome",
                     choices = c("All", 
                                 "Win" = TRUE, 
                                 "Loss" = FALSE),
                     selected = "All"),
        
        # Conditional color overlay options
        conditionalPanel(
          condition = "input.location == 'All' && input.outcome == 'All'",
          radioButtons(inputId = "color_overlay",
                       label = "Select color overlay",
                       choices = c("None", 
                                   "Home/Away" = "home",
                                   "Win/Loss" = "win"),
                       selected = "None")),
        
        conditionalPanel(
          condition = "input.location == 'All' && input.outcome != 'All'",
          radioButtons(inputId = "color_overlay",
                       label = "Select color overlay",
                       choices = c("None", 
                                   "Home/Away" = "home"),
                       selected = "None")),
        
        conditionalPanel(
          condition = "input.location != 'All' && input.outcome == 'All'",
          radioButtons(inputId = "color_overlay",
                       label = "Select color overlay",
                       choices = c("None", 
                                   "Win/Loss" = "win"),
                       selected = "None"))
      ),
      
      # Tab 2 Widgets
      
    ), # closes sidebarPanel
    
    
    # -------------------------------------------------------
    # Main Panel with 2 Tabs: 
    # Tab 1: Recent Game Data
    # Tab 2: Player Career Progression
    # -------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # Tab 1
        tabPanel(title = "Game Data",
                 value = 1,
                 fluidPage(
                   tags$h3(tags$strong(textOutput("t1_title")), style = "font-size: 20px;")
                 ),
                 tableOutput(outputId="t1_t1"),
                 plotlyOutput(outputId = "t1_p1"),
                 fluidPage(
                   textOutput("t1_p1_caption"))
        ),
        
        # Tab 2
        # Rolling data, allow for season-by-seaason
        tabPanel(title = "Career Progression",
                 value = 2,
                 fluidPage(
                   tags$h3(tags$strong(textOutput("t2_title")), style = "font-size: 20px;")
                 ),
                 tableOutput(outputId = "t2_t1"),
                 plotlyOutput(outputId = "t2_p1"),
                 checkboxInput(inputId = "trendline",
                               label = "Overlay trendline"),
                 fluidPage(
                   textOutput("t2_p1_caption"))
                 # dataTableOutput(outputId="t1_t1")
        ),
        
        # Selected tab
        id = "tabselected"
      ) # closes tabsetPanel
    ) # closes mainPanel
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)


# ===============================================
# Server logic
# ===============================================
server <- function(input, output, session) {
  # Update player select input
  observe({
    updateSelectizeInput(session, "player", 
                         choices = default_order,  # Update the choices with the player names
                         selected = "Stephen Curry",  # Default selection
                         server = TRUE)  # Server-side rendering
  })
  
  # Update opponent select input based on player selection
  observe({
    req(input$player)
    
    opponent_teams <- stats |> 
      filter(fullName == input$player) |>
      mutate(Opponent = paste(opponentteamCity, opponentteamName)) |> 
      pull(Opponent) |> 
      unique() |> 
      sort()
    
    updateSelectizeInput(session, "opponent", 
                         choices = c("All teams", opponent_teams),
                         selected = "All teams",  
                         server = TRUE)  
  })
  
  # Update season select input based on player selection
  observe({
    req(input$player)
    
    season_numbers <- stats |>
      filter(fullName == input$player) |>
      pull(gameSeason) |>
      unique() |>
      sort(decreasing = TRUE)
    
    season_choices <- setNames(
      season_numbers,
      paste0(season_numbers - 1, "-", season_numbers)
    )
    
    updateSelectizeInput(session, "selected_season",
                         choices = season_choices,
                         selected = max(season_numbers))  # Default to most recent
  })
  
  
  # ------------------------------------------------
  # Auxiliary data frames
  # ------------------------------------------------
  # Primary filtered player data
  player_stats <- reactive({
    player_stats = stats |> 
      filter(fullName == input$player,
             !is.na(minutes))
    
    if(input$tabselected == 1) {
      # Opponent filter
      if(input$opponent != "All teams") {
        player_stats = player_stats |>
          filter(paste(opponentteamCity, opponentteamName) == input$opponent)
      }
      
      # Home/Away filter
      if(input$location != "All") {
        player_stats = player_stats |>
          filter(home == input$location)
      }
      
      # Win/Loss filter
      if(input$outcome != "All") {
        player_stats = player_stats |>
          filter(win == input$outcome)
      }
    } 
    
    player_stats <- cumulative_stat_sum(player_stats, input$stat)
  })
  
  # Recent games data for plotting and stats
  recent_games_data <- reactive({
    req(player_stats())
    
    if (input$timeframe == "View recent games") {
      player_stats() |>
        arrange(desc(gameDate)) |>
        head(input$recent_games)
    } else {
      filtered_data <- player_stats() |>
        filter(gameSeason == input$selected_season)
      
      filtered_data
    }
  })
  
  # Find the season with the highest average for the selected stat
  highest_season_avg <- reactive({
    req(player_stats())
    
    player_stats() |>
      group_by(gameSeason) |>
      filter(n() >= 10) |> # Only consider seasons with at least 10 games
      summarize(avg = mean(.data[[input$stat]], na.rm = TRUE)) |>
      arrange(desc(avg)) |>
      head(1)
  })
  
  # Player bio information
  clean_stats <- reactive({
    clean_stats = stats |>
      filter(fullName == input$player) |>
      mutate(
        "Name" = fullName,
        "Position" = position,
        "Height" = paste0(height %/% 12, "\' ", height %% 12, "\""),
        "Weight" = paste(bodyWeight, "lbs"),
        "Teams Played For" = teamsPlayedFor,
        "Season" = paste(gameSeason - 1, gameSeason, sep="-"),
        "Date" = gameDate,
        "Team" = paste(playerteamCity, playerteamName),
        "Opponent" = paste(opponentteamCity, opponentteamName),
        "Location" = case_when(
          home == TRUE ~ "Home",
          home == FALSE ~ "Away"
        ),
        "Outcome" = case_when(
          win == TRUE ~ "Win",
          win == FALSE ~ "Loss"
        ),
        "MIN" = minutes,
        "PTS" = points,
        "FG" = paste0(fieldGoalsMade, "-", fieldGoalsAttempted),
        "3PT" = paste0(threePointersMade, "-", threePointersAttempted),
        "FT" = paste0(freeThrowsMade, "-", freeThrowsAttempted),
        "DREB" = reboundsDefensive,
        "OREB" = reboundsOffensive,
        "REB" = reboundsTotal,
        "AST" = assists,
        "STL" = steals,
        "BLK" = blocks,
        "TO" = turnovers,
        "PF" = foulsPersonal,
        "+/-" = plusMinusPoints,
        "TS%" = round(trueShootingPercentage * 100, 2),
        "eFG%" = round(effectiveFGPercentage * 100, 2)
      )
    clean_stats = clean_stats[, c("Name", "Position", "Height", "Weight", "Teams Played For", "Season", "Date", 
                                  "Team", "Opponent", "Location", "Outcome", "MIN", 
                                  "PTS", "FG", "3PT", "FT", "DREB", "OREB", "REB", 
                                  "AST", "STL", "BLK", "TO", "PF", "+/-", "TS%", "eFG%")]
  })
  
  # ------------------------------------------------
  # Tab 1: Output
  # ------------------------------------------------
  output$t1_title <- renderText({
    stat_name = case_when(
      input$stat == "threePointersMade" ~ "3-Pointers Made",
      input$stat == "freeThrowsMade" ~ "Free Throws Made",
      input$stat == "reboundsTotal" ~ "Total Rebounds",
      input$stat == "fantasyPoints" ~ "Fantasy Points",
      TRUE ~ paste0(toupper(str_sub(input$stat, 1, 1)), str_sub(input$stat, 2, nchar(input$stat)))
    )
    
    # Data frame
    title_stats <- player_stats() |>
      mutate(opponent = case_when(
        home == TRUE ~ paste(playerteamName, "vs.", opponentteamName),
        home == FALSE ~ paste(playerteamName, "@", opponentteamName))
      )
    
    # Tab title
    if(input$timeframe == "View recent games") {
      text <- paste0(input$player, "'s ", stat_name, " in the Last ", input$recent_games, " Games")
    } else {
      season_text <- paste0(as.numeric(input$selected_season) - 1, "-", input$selected_season)
      text <- paste0(input$player, "'s ", stat_name, " in the ", season_text, " Season")
    }
    
    if (input$location != "All") {
      text <- paste0(text, " ", ifelse(input$location == "TRUE", "Home", "Away"))
    }
    if (input$outcome != "All") {
      text <- paste0(text, " ", ifelse(input$outcome == "TRUE", "Wins", "Losses"))
    }
    if (input$opponent != "All teams") {
      text <- paste0(text, " vs. ", input$opponent)
    }
    
    text
  })
  
  
  output$t1_t1 <- renderTable({
    # Career average
    career_avg <- mean(player_stats()[[input$stat]], na.rm = TRUE)
    
    # Latest season average
    latest_season <- max(player_stats()$gameSeason, na.rm = TRUE)
    latest_season_avg <- player_stats() |>
      filter(gameSeason == latest_season) |>
      summarize(avg = mean(.data[[input$stat]], na.rm = TRUE)) |> 
      pull(avg)
    
    # Highest season average
    best_season_data <- highest_season_avg()
    best_season <- best_season_data$gameSeason[1]
    best_season_avg <- best_season_data$avg[1]
    
    # Timeframe average (recent games or selected season)
    timeframe_avg <- mean(recent_games_data()[[input$stat]], na.rm = TRUE)
    
    # Create column names
    current_latest = ifelse(latest_season == 2025, "Current", "Latest")
    latest_season_colname <- paste0(current_latest, " Season (", latest_season - 1, "-", latest_season, ")")
    best_season_colname <- paste0("Best Season (", best_season - 1, "-", best_season, ")")
    
    if (input$timeframe == "View recent games") {
      timeframe_colname <- paste0("Last ", input$recent_games, " Games")
    } else if (input$selected_season == latest_season) {
      # Skip the timeframe column if it's the same as latest season
      table <- data.frame(
        col1 = career_avg,
        col2 = best_season_avg,
        col3 = latest_season_avg
      )
      colnames(table) <- c("Career Average", best_season_colname, latest_season_colname)
      return(table)
    } else {
      timeframe_colname <- paste0("Season Average (", as.numeric(input$selected_season) - 1, "-", input$selected_season, ")")
    }
    
    # Create table with all columns 
    table <- data.frame(
      col1 = career_avg,
      col2 = best_season_avg,
      col3 = latest_season_avg,
      col4 = timeframe_avg
    )
    colnames(table) <- c("Career Average", best_season_colname, latest_season_colname, timeframe_colname)
    
    table
  })
  
  
  output$t1_p1 <- renderPlotly({
    stat_name = case_when(
      input$stat == "threePointersMade" ~ "3-Pointers Made",
      input$stat == "freeThrowsMade" ~ "Free Throws Made",
      input$stat == "reboundsTotal" ~ "Total Rebounds",
      input$stat == "fantasyPoints" ~ "Fantasy Points",
      TRUE ~ paste0(toupper(str_sub(input$stat, 1, 1)), str_sub(input$stat, 2, nchar(input$stat)))
    )
    stat_abbr = switch(input$stat,
                       "points" = "PTS",
                       "threePointersMade" = "3PM",
                       "freeThrowsMade" = "FTM",
                       "reboundsTotal" = "TRB",
                       "assists" = "AST",
                       "steals" = "STL",
                       "blocks" = "BLK",
                       "turnovers" = "TO",
                       "fantasyPoints" = "FPTS",
                       "minutes" = "MIN")
    
    # Data from recent games with tooltip data
    plot_data <- recent_games_data() |>
      mutate(
        opponent = case_when(
          home == TRUE ~ paste(playerteamName, "vs.", opponentteamName),
          home == FALSE ~ paste(playerteamName, "@", opponentteamName)
        ),
        text = paste0(opponent,
                      "<br>", stat_abbr, ": ", .data[[input$stat]],
                      "<br>Minutes: ", minutes,
                      "<br>Date: ", format(as.Date(gameDate), "%b %d, %Y"),
                      "<br>Type: ", gameType)
      )
    
    # Average of selected timeframe
    timeframe_avg = mean(plot_data[[input$stat]], na.rm = TRUE)
    timeframe_avg = round(timeframe_avg, 1)
    
    # Bar graph title
    if(input$timeframe == "View recent games") {
      title_p1 <- paste0(stat_name, " (Last ", input$recent_games)
    } else {
      if(is.null(input$date_range) || 
         (input$date_range[1] == min(player_stats()$gameDate[player_stats()$gameSeason == input$selected_season], na.rm = TRUE) && 
          input$date_range[2] == max(player_stats()$gameDate[player_stats()$gameSeason == input$selected_season], na.rm = TRUE))) {
        title_p1 <- paste0(stat_name, " (", as.numeric(input$selected_season), "-", input$selected_season, " Season")
      } else {
        start_date <- format(as.Date(input$date_range[1]), "%b %d")
        end_date <- format(as.Date(input$date_range[2]), "%b %d")
        title_p1 <- paste0(stat_name, " (", start_date, " to ", end_date)
      }
    }
    
    if (input$location != "All") {
      title_p1 <- paste0(title_p1, " ", ifelse(input$location == "TRUE", "Home", "Away"))
    }
    if (input$outcome != "All") {
      title_p1 <- paste0(title_p1, " ", ifelse(input$outcome == "TRUE", "Wins", "Losses"))
    } else {
      title_p1 <- paste0(title_p1, " Games")
    }
    if (input$opponent != "All teams") {
      title_p1 <- paste0(title_p1, " vs. ", input$opponent)
    }
    title_p1 <- paste0(title_p1, ")")
    
    
    # Bar graph legend labels
    plot_data$legend_label <- case_when(
      input$color_overlay == "None" || input$location != "All" & input$outcome != "All" ~ "All",
      input$color_overlay == "win" ~ ifelse(plot_data$win == TRUE, "Win", "Loss"),
      input$color_overlay == "home" ~ ifelse(plot_data$home == TRUE, "Home", "Away")
    )
    legend_title <- switch(input$color_overlay,
                           "win" = "Outcome",
                           "home" = "Location")
    
    #  Bar graph
    p1 = plot_data |> 
      ggplot(aes(x = gameDate,
                 y = .data[[input$stat]],
                 fill = legend_label,
                 text = text)) +
      geom_bar(stat = "identity", 
               color = "black", 
               linewidth=0.5) +
      scale_fill_manual(values = c("Win" = "#00bf6d", "Loss" = "#FF2B6F", 
                                   "Home" = "#00bf6d", "Away" = "#FF2B6F",
                                   "All" = "skyblue"),
                        name = legend_title) +
      geom_hline(yintercept = timeframe_avg, 
                 linetype="dotted") +
      annotate(geom = "text",
               x = min(plot_data$gameDate), 
               y = timeframe_avg * 1.1, 
               label = timeframe_avg) +
      labs(title = title_p1,
           x = "Game Date",
           y = stat_name) +
      theme(plot.background = element_rect(fill = "seashell2"),
            panel.background = element_rect(fill = "seashell"),
            panel.grid.major = element_line(color = "grey70"),
            axis.line = element_line(color = "black"),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p1, tooltip="text")
  })
  
  output$t1_p1_caption <- renderText({
    "Click and drag to zoom in on interactive plot"
  })
  
  
  # ------------------------------------------------
  # Tab 2: Output
  # ------------------------------------------------
  output$t2_title <- renderText({
    stat_name = case_when(
      input$stat == "threePointersMade" ~ "3-Pointers Made",
      input$stat == "freeThrowsMade" ~ "Free Throws Made",
      input$stat == "reboundsTotal" ~ "Total Rebounds",
      input$stat == "fantasyPoints" ~ "Fantasy Points",
      TRUE ~ paste0(toupper(str_sub(input$stat, 1, 1)), str_sub(input$stat, 2, nchar(input$stat)))
    )
    
    # Tab title
    text <- paste0(input$player, "'s Career Progression in ", stat_name, " Per Game")
    text
  })
  
  
  output$t2_t1 <- renderTable({
    bio = clean_stats()[, c("Name", "Position", "Height", "Weight", "Teams Played For")]
    head(bio, 1)
  })
  
  
  output$t2_p1 <- renderPlotly({
    stat_name = case_when(
      input$stat == "threePointersMade" ~ "3-Pointers Made",
      input$stat == "freeThrowsMade" ~ "Free Throws Made",
      input$stat == "reboundsTotal" ~ "Total Rebounds",
      input$stat == "fantasyPoints" ~ "Fantasy Points",
      TRUE ~ paste0(toupper(str_sub(input$stat, 1, 1)), str_sub(input$stat, 2, nchar(input$stat)))
    )
    stat_abbr = switch(input$stat,
                       "points" = "PTS",
                       "threePointersMade" = "3PM",
                       "freeThrowsMade" = "FTM",
                       "reboundsTotal" = "TRB",
                       "assists" = "AST",
                       "steals" = "STL",
                       "blocks" = "BLK",
                       "turnovers" = "TO",
                       "fantasyPoints" = "FPTS",
                       "minutes" = "MIN")
    
    season_data <- player_stats() |>
      group_by(gameSeason) |>
      summarize(games_played = n(),
                avg_minutes = mean(minutes, na.rm = TRUE),
                avg_stat = mean(.data[[input$stat]], na.rm = TRUE)) |>
      ungroup() |>
      mutate(season_label = paste0(gameSeason - 1, "-", gameSeason),
             text = paste0("Season: ", season_label,
                           "<br>Games played (incl. playoffs): ", games_played,
                           "<br>MPG: ", round(avg_minutes, 2),
                           "<br>", stat_abbr, "/G: ", round(avg_stat, 2)))
    
    per_game_stat = paste0(input$stat, "PerGame")
    
    if (input$progression_type == "Rolling career average") {
      title_p1 = paste0("Average ", stat_name, " Per Game (Rolling)")
    } else {
      title_p1 = paste0("Average ", stat_name, " Per Game (Season-by-Season)")
    }
    
    if (input$stat == "fantasyPoints") {
      y_end = 70
      y_gap = 2
    } else if (input$stat %in% c("steals", "blocks", "turnovers")) {
      y_end = 20
      y_gap = 0.5
    } else {
      y_end = 60
      y_gap = 1
    }
    
    if (input$progression_type == "Rolling career average") {
      # Rolling averages plot
      p1_points <- player_stats() |>
        group_by(gameSeason) |>
        slice_min(gameDate, n = 1) |>
        ungroup() |>
        mutate(y_value = .data[[per_game_stat]],
               text = paste0("Start of ", gameSeason - 1, "-", gameSeason, " season",
                             "<br>Game number: ", careerGamesPlayed,
                             "<br>", stat_abbr, "/G: ", round(y_value, 2)))
      
      latest_point <- player_stats() |>
        filter(gameDate == max(gameDate, na.rm = TRUE)) |>
        mutate(y_value = .data[[per_game_stat]],
               text = paste0("Most recent game: ", format(as.Date(gameDate), "%b %d, %Y"),
                             "<br>Career games played: ", careerGamesPlayed,
                             "<br>Career ", stat_abbr, "/G: ", round(y_value, 2)))
      
      p1_points <- bind_rows(p1_points, latest_point)
      
      
      p1 <- player_stats() |>
        ggplot(aes(x = careerGamesPlayed, y = .data[[per_game_stat]])) +
        geom_line(color = "skyblue", linewidth = 1.2) +
        geom_point(data = p1_points, aes(x = careerGamesPlayed, y = y_value, text = text),
                   inherit.aes = FALSE,
                   color = "steelblue") +
        labs(title = title_p1,
             x = "Games Played",
             y = paste0("Career ", stat_abbr, "/G")) +
        scale_x_continuous(breaks = seq(0, 2000, 100)) +
        scale_y_continuous(breaks = seq(0, y_end, y_gap),
                           expand = c(0, 1)) +
        theme(plot.background = element_rect(fill = "seashell2"),
              panel.background = element_rect(fill = "seashell"),
              panel.grid.major = element_line(color = "grey70"),
              axis.line = element_line(color = "black"),
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      if (input$trendline) {
        p1 <- p1 +
          geom_smooth(method = "gam", formula = y ~ s(x, k=5), se = FALSE,
                      color = "steelblue", linetype = "dotted", linewidth = 0.8)
      }
      
    } else {
      # Season by season plot
      p1 <- season_data |>
        ggplot(aes(x = season_label, y = avg_stat, group = 1, text = text)) +
        geom_line(color = "skyblue", linewidth = 1.2) +
        geom_point(color = "steelblue") +
        labs(title = title_p1,
             x = "Season",
             y = paste0(stat_abbr, "/G")) +
        theme(plot.background = element_rect(fill = "seashell2"),
              panel.background = element_rect(fill = "seashell"),
              panel.grid.major = element_line(color = "grey70"),
              axis.line = element_line(color = "black"),
              axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    ggplotly(p1, tooltip = "text")
  })
  
  output$t2_p1_caption <- renderText({
    "Click and drag to zoom in on interactive plot"
  })
}  # closes server

# Run the application 
shinyApp(ui = ui, server = server)
