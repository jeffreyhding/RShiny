# Title: NBA Player Stat Visualizer
# Description: individual NBA player stats and career progression
# Author: Jeffrey Ding
# Date: 2-8-2025

# ===============================================
# Packages
# ===============================================
library(shiny)
library(tidyverse)  # for data manipulation and graphics
library(plotly)     # for nicer plots
library(lubridate)    # for working with dates
library(DT)         # to render Data Tables nicely


# ===============================================
# Data
# ===============================================
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
                     selected = "LeBron James",
                     multiple = FALSE,  # Disables multiple selections
                     options = list(
                       placeholder = "Choose a player",
                       maxItems = 1,
                       create = FALSE)),  # Disable creating new options
      
      # Select stat
      selectInput(inputId = "stat",
                  label = "Select stat",
                  choices = c("Points" = "points", "3-Pointers" = "threePointersMade", 
                              "Free Throws" = "freeThrowsMade", "Rebounds" = "reboundsTotal", "Assists" = "assists", 
                              "Steals" = "steals", "Blocks" = "blocks", "Turnovers" = "turnovers", "Minutes" = "minutes",
                              "Fantasy Points (ESPN Default)" = "fantasyPoints"),
                  selected = "PTS"),
      
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
      )
    ), # closes sidebarPanel
    
    
    # -------------------------------------------------------
    # Main Panel with 2 Tabs: 
    # Tab 1: Single Player Data
    # Tab 2: Player Comparison
    # Tab 3: 
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
                 plotlyOutput(outputId = "t1_p1")
        ),
        
        # Tab 2
        # Rolling data, allow for season-by-seaason
        tabPanel(title = "Career Progression",
                 value = 2,
                 fluidPage(
                   tags$h3(tags$strong(textOutput("t2_title")), style = "font-size: 20px;")
                 ),
                 tableOutput(outputId = "t2_t1"),
                 plotlyOutput(outputId = "t2_p1")
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
  observe({
    updateSelectizeInput(session, "player", 
                         choices = default_order,  # Update the choices with the player names
                         selected = "LeBron James",  # Default selection
                         server = TRUE)  # Server-side rendering
  })
  
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
  
  # ------------------------------------------------
  # Auxiliary table
  # ------------------------------------------------
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
        
      # Win/Loss fitler
      if(input$outcome != "All") {
        player_stats = player_stats |>
          filter(win == input$outcome)
      }
    } 
    
    player_stats <- cumulative_stat_sum(player_stats, input$stat)
  })
  
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
      input$stat == "reboundTotal" ~ "Total Rebounds",
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
    text <- paste0(input$player, "'s ", stat_name, " in") 
    if (input$location != "All") {
      text <- paste0(text, " ", ifelse(input$location == "TRUE", "Home", "Away"))
    }
    if (input$outcome != "All") {
      text <- paste0(text, " ", ifelse(input$outcome == "TRUE", "Wins", "Losses"))
    } else {
      text <- paste0(text, " Games")
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
    
    # Last 10 games average
    last_10_avg <- player_stats() |>
      arrange(desc(gameDate)) |> 
      head(10) |> 
      summarize(avg = mean(.data[[input$stat]], na.rm = TRUE)) |> 
      pull(avg)
    
    latest_season_colname = paste0("Season Average (", latest_season - 1, "-", latest_season, ")")
    if (latest_season != 2025) {
      latest_season_colname = paste0("Latest ", latest_season_colname)
    }
    
    table <- data.frame(
      col1 = career_avg,
      col2 = latest_season_avg,
      col3 = last_10_avg
    )
    colnames(table) <- c("Career Average", latest_season_colname, "Last 10 Average")
    
    table
  })
  
  output$t1_p1 <- renderPlotly({
    stat_name = case_when(
      input$stat == "threePointersMade" ~ "3-Pointers Made",
      input$stat == "freeThrowsMade" ~ "Free Throws Made",
      input$stat == "reboundTotal" ~ "Total Rebounds",
      input$stat == "fantasyPoints" ~ "Fantasy Points",
      TRUE ~ paste0(toupper(str_sub(input$stat, 1, 1)), str_sub(input$stat, 2, nchar(input$stat)))
    )
    stat_abbr = switch(input$stat,
                       "points" = "PTS",
                       "threePointersMade" = "3P",
                       "freeThrowsMade" = "FT",
                       "reboundsTotal" = "TRB",
                       "assists" = "AST",
                       "steals" = "STL",
                       "blocks" = "BLK",
                       "turnovers" = "TO")
    
    # Data from last 10 games with tooltip data
    last_10_games <- player_stats() |>
      arrange(desc(gameDate)) |>
      head(10) |>
      mutate(
        opponent = case_when(
          home == TRUE ~ paste(playerteamName, "vs.", opponentteamName),
          home == FALSE ~ paste(playerteamName, "@", opponentteamName)
        ),
        text = paste0(opponent,
                      "<br>", input$stat, ": ", .data[[input$stat]],
                      "<br>mins: ", minutes,
                      "<br>type: ", gameType)
      )
    
    # Last 10 games average
    last_10_avg = last_10_games |> 
      summarize(avg = mean(.data[[input$stat]], na.rm = TRUE)) |> 
      pull(avg)
    last_10_avg = round(last_10_avg, 1)
    
    
    # Bar graph title
    title_p1 <- paste0(stat_name, " (Last 10") 
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
    last_10_games$legend_label <- case_when(
      input$color_overlay == "None" || input$location != "All" & input$outcome != "All" ~ "All",
      input$color_overlay == "win" ~ ifelse(last_10_games$win == TRUE, "Win", "Loss"),
      input$color_overlay == "home" ~ ifelse(last_10_games$home == TRUE, "Home", "Away")
    )
    legend_title <- switch(input$color_overlay,
                           "win" = "Outcome",
                           "home" = "Location")
    
    #  Bar graph
    p1 = last_10_games |> 
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
      geom_hline(yintercept = last_10_avg, 
                 linetype="dotted") +
      annotate(geom = "text",
               x = min(last_10_games$gameDate), 
               y = last_10_avg * 1.1, 
               label = last_10_avg) +
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
  
  
  # ------------------------------------------------
  # Tab 2: Output
  # ------------------------------------------------
  output$t2_title <- renderText({
    stat_name = case_when(
      input$stat == "threePointersMade" ~ "3-Pointers Made",
      input$stat == "freeThrowsMade" ~ "Free Throws Made",
      input$stat == "reboundTotal" ~ "Total Rebounds",
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
                       "threePointersMade" = "3P",
                       "freeThrowsMade" = "FT",
                       "reboundsTotal" = "TRB",
                       "assists" = "AST",
                       "steals" = "STL",
                       "blocks" = "BLK",
                       "turnovers" = "TO",
                       "fantasyPoints" = "FPTS")
    
    per_game_stat = paste0(input$stat, "PerGame")
    
    title_p1 = paste0("Rolling Career ", stat_name, " Per Game")
    
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

    # Smoothed line
    p1 = player_stats() |>
      ggplot(mapping = aes(x = careerGamesPlayed, 
                           y = .data[[per_game_stat]])) +
      geom_smooth(color="deepskyblue") +
      labs(title = title_p1,
           x = "Games Played",
           y = paste0("Career ", stat_abbr, "/G")) +
      scale_x_continuous(breaks = seq(0, 2000, 100)) +
      scale_y_continuous(breaks = seq(0, y_end, y_gap),
                         expand = c(0, 1)) +
      theme(plot.background = element_rect(fill="seashell2"),
            panel.background = element_rect(fill="seashell"),
            panel.grid.major = element_line(color="grey70"),
            axis.line = element_line(color="black"),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p1)
  })
  
  # output$t1_t1 <==- renderDataTable({
  #   table = clean_stats()[, c("Season", "Date", 
  #                        "Team", "Opponent", "Location", "Outcome", "MIN", 
  #                        "PTS", "FG", "3PT", "FT", "DREB", "OREB", "REB", 
  #                        "AST", "STL", "BLK", "TO", "PF", "+/-", "TS%", "eFG%")]
  #   table
  # })
  
}  # closes server

# Run the application 
shinyApp(ui = ui, server = server)
