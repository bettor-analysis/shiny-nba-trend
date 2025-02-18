# NBA Player Stats Dashboard
# Adam Wickwire - Bettor Analysis 
# 2025 All Rights Reserved
# 
# This app includes:
# - A multi-tab UI (Dashboard, Leaderboard, About)
# - Single player selection (defaults to the top scoring player)
# - Timeframe filtering using only preset options (last 5, 10, 20 games; this season; last season)
# - A bar chart styled like the original version, with an optional 3-game moving average trend
# - A summary row above the game log table with key metrics (Over Count with percentage, Average, Max, Min)
# - Leaderboard for comparative analysis (using season averages, including combo stats)
# - Export raw game logs (CSV or Excel)
# - Excludes games where the team_name is "All-Star", "Team Kenny", "Team Candace", "Team Chuck", or "Team Shaq"
#
# To use:
# 1. Save this file in a folder.
# 2. Install the required packages.
# 3. Run the app using runApp("your_app_folder").
#
# Required packages: shiny, shinythemes, reactable, htmltools, ggplot2,
# dplyr, hoopR, tidyverse, plotly, shinycssloaders, openxlsx

library(shiny)
library(shinythemes)
library(reactable)
library(htmltools)
library(ggplot2)
library(dplyr)
library(hoopR)
library(tidyverse)
library(plotly)
library(shinycssloaders)
library(openxlsx)

# Use the helper function from hoopr to determine the current season.
current_season <- most_recent_nba_season()
last_season <- current_season - 1

# ---------------------------
# UI
# ---------------------------
ui <- navbarPage(
  title = "NBA Player Stats Dashboard",
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .container-fluid { 
        padding-left: 5px; 
        padding-right: 5px; 
      }
      .reactable { width: 100% !important; }
      .reactable-header { 
        text-align: center !important;
        font-weight: bold !important;
      }
      .reactable-cell {
        text-align: center !important;
      }
    "))
  ),
  
  # Dashboard Tab
  tabPanel(
    "Dashboard",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectizeInput(
          inputId = "dash_player",
          label = "Select Player:",
          choices = NULL,
          multiple = FALSE,
          options = list(placeholder = "Search and select a player")
        ),
        selectInput(
          inputId = "dash_timeframe",
          label = "Select Timeframe:",
          choices = c(
            "Last 5 Games" = "last_5",
            "Last 10 Games" = "last_10",
            "Last 20 Games" = "last_20",
            "This Season" = "this_season",
            "Last Season" = "last_season"
          ),
          selected = "last_10"
        ),
        selectInput(
          inputId = "dash_stat",
          label = "Select Statistic:",
          choices = c(
            "Points" = "points",
            "Rebounds" = "rebounds",
            "Assists" = "assists",
            "Steals" = "steals",
            "Blocks" = "blocks",
            "Turnovers" = "turnovers",
            "Field Goals Made" = "field_goals_made",
            "Field Goals Attempted" = "field_goals_attempted",
            "Three Pointers Made" = "three_point_field_goals_made",
            "Free Throws Made" = "free_throws_made",
            "Points + Assists" = "points_assists",
            "Points + Rebounds" = "points_rebounds",
            "Assists + Rebounds" = "assists_rebounds",
            "Points + Assists + Rebounds" = "points_assists_rebounds"
          ),
          selected = "points"
        ),
        numericInput(
          inputId = "dash_bet_line",
          label = "Enter Betting Line:",
          value = 5,
          min = 0
        ),
        checkboxInput(
          inputId = "dash_show_trend",
          label = "Show 3-Game Moving Average Trend",
          value = TRUE
        ),
        selectInput(
          inputId = "dash_download_format",
          label = "Download Format:",
          choices = c("CSV", "Excel"),
          selected = "CSV"
        ),
        downloadButton("dash_downloadData", "Download Game Logs")
      ),
      mainPanel(
        fluidRow(
          column(12, withSpinner(plotlyOutput("dash_statPlot")))
        ),
        br(),
        fluidRow(
          column(12, uiOutput("dash_summary"))
        ),
        br(),
        fluidRow(
          column(12, withSpinner(reactableOutput("dash_statTable")))
        )
      )
    )
  ),
  
  # Leaderboard Tab
  tabPanel(
    "Leaderboard",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(
          inputId = "lb_season",
          label = "Select Season:",
          choices = c("This Season" = "this_season", "Last Season" = "last_season"),
          selected = "this_season"
        ),
        selectInput(
          inputId = "lb_stat",
          label = "Select Statistic:",
          choices = c(
            "Points" = "points",
            "Rebounds" = "rebounds",
            "Assists" = "assists",
            "Steals" = "steals",
            "Blocks" = "blocks",
            "Turnovers" = "turnovers",
            "Points + Assists" = "points_assists",
            "Points + Rebounds" = "points_rebounds",
            "Assists + Rebounds" = "assists_rebounds",
            "Points + Assists + Rebounds" = "points_assists_rebounds"
          ),
          selected = "points"
        )
      ),
      mainPanel(
        withSpinner(reactableOutput("lb_leaderboard"))
      )
    )
  ),
  
  # About Tab
  tabPanel(
    "About",
    fluidPage(
      h3("About This App"),
      p("This enhanced dashboard allows you to explore NBA player statistics interactively."),
      p("Select a player, filter by recent games or season, and choose a statistic—including combination stats—to view a bar chart with color-coded performance indicators."),
      p("A summary row above the game log table displays key metrics (with an over count and percentage), and the Leaderboard tab provides a comparative view of player averages."),
      p(a("Video Tutorial", href = "https://www.youtube.com/@bettoranalysis", target = "_blank")),
      h4("Installation and Usage Instructions:"),
      tags$ul(
        tags$li("Install R and RStudio."),
        tags$li("Install the required packages (see code for package list)."),
        tags$li("Download the code into a single folder."),
        tags$li("Run the app using runApp('your_app_folder').")
      ),
      p("If you have any questions, please consult the documentation included with the app.")
    )
  )
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # Load historical NBA player data for the last two seasons.
  nba_player <- tryCatch({
    load_nba_player_box(seasons = current_season:last_season)
  }, error = function(e) {
    showNotification("Error loading NBA player data. Please check the data source.", type = "error")
    NULL
  })
  
  # Preprocess data once loaded.
  nba_data <- reactive({
    req(nba_player)
    required_cols <- c("game_date", "season", "athlete_display_name", "team_name")
    missing_cols <- setdiff(required_cols, names(nba_player))
    if(length(missing_cols) > 0){
      showNotification(paste("Missing columns:", paste(missing_cols, collapse = ", ")), type = "error")
      return(NULL)
    }
    data <- nba_player
    data$game_date <- as.Date(data$game_date)
    data$season <- as.integer(data$season)
    data
  })
  
  # Update available players based on the selected timeframe.
  observe({
    req(nba_data())
    df <- nba_data()
    current_season_selected <- if(input$dash_timeframe == "last_season") last_season else current_season
    # Exclude unwanted team names.
    df <- df %>% filter(!(team_name %in% c("All-Star", "Team Kenny", "Team Candace", "Team Chuck", "Team Shaq")))
    players <- df %>% filter(season == current_season_selected) %>% pull(athlete_display_name) %>% unique() %>% sort()
    current_selection <- isolate(input$dash_player)
    valid_selection <- intersect(current_selection, players)
    if(length(valid_selection) == 0 && length(players) > 0){
      df_season <- df %>% filter(season == current_season_selected)
      default_player <- df_season %>% group_by(athlete_display_name) %>% summarise(avg_points = mean(points, na.rm = TRUE)) %>% arrange(desc(avg_points)) %>% slice(1) %>% pull(athlete_display_name)
      valid_selection <- default_player
    }
    updateSelectizeInput(session, "dash_player", choices = players, selected = valid_selection, server = TRUE)
  })
  
  # Reactive: Filter data based on Dashboard inputs.
  filtered_data <- reactive({
    req(nba_data(), input$dash_player, input$dash_stat, input$dash_timeframe)
    df <- nba_data()
    # Exclude unwanted team names.
    df <- df %>% filter(!(team_name %in% c("All-Star", "Team Kenny", "Team Candace", "Team Chuck", "Team Shaq")))
    current_season_selected <- if(input$dash_timeframe == "last_season") last_season else current_season
    df <- df %>% filter(season == current_season_selected, athlete_display_name == input$dash_player) %>% arrange(game_date)
    if(input$dash_timeframe %in% c("last_5", "last_10", "last_20")){
      n_games <- switch(input$dash_timeframe, "last_5" = 5, "last_10" = 10, "last_20" = 20)
      df <- df %>% arrange(desc(game_date)) %>% head(n_games) %>% arrange(game_date)
    }
    if(nrow(df) == 0){
      showNotification("No data available for the selected options.", type = "error")
      return(NULL)
    }
    df
  })
  
  # Reactive: Compute graph data using the chosen stat (including combination stats).
  graph_data <- reactive({
    df <- filtered_data()
    req(df)
    stat <- input$dash_stat
    if(stat == "points_assists"){
      df$stat_value <- df$points + df$assists
    } else if(stat == "points_rebounds"){
      df$stat_value <- df$points + df$rebounds
    } else if(stat == "assists_rebounds"){
      df$stat_value <- df$assists + df$rebounds
    } else if(stat == "points_assists_rebounds"){
      df$stat_value <- df$points + df$assists + df$rebounds
    } else {
      df$stat_value <- df[[stat]]
    }
    df
  })
  
  # Reactive: Prepare table data using raw filtered_data.
  table_data <- reactive({
    df <- filtered_data()
    req(df)
    base_stats <- c("points", "rebounds", "assists", "steals", "blocks", "turnovers",
                    "field_goals_made", "field_goals_attempted", "three_point_field_goals_made", "free_throws_made")
    if(input$dash_stat %in% base_stats){
      df$over_bet_line <- ifelse(!is.na(df[[input$dash_stat]]) & df[[input$dash_stat]] >= input$dash_bet_line, "Over", "Under")
    } else {
      df$over_bet_line <- NA
    }
    df
  })
  
  # Render the Statistic Plot using graph_data.
  output$dash_statPlot <- renderPlotly({
    df <- graph_data()
    req(df)
    df$game_date_str <- format(df$game_date, "%Y-%m-%d")
    if(input$dash_show_trend && nrow(df) >= 3){
      df <- df %>% arrange(game_date)
      df$moving_avg <- as.numeric(stats::filter(df$stat_value, rep(1/3, 3), sides = 1))
    } else {
      df$moving_avg <- NA
    }
    timeframe_text <- if(input$dash_timeframe %in% c("last_5", "last_10", "last_20")){
      paste("Last", switch(input$dash_timeframe, "last_5" = "5", "last_10" = "10", "last_20" = "20"), "Games")
    } else {
      paste("Season", if(input$dash_timeframe == "last_season") last_season else current_season)
    }
    bet_line <- input$dash_bet_line
    p <- ggplot(df, aes(x = reorder(game_date_str, game_date), y = stat_value,
                        text = paste("Game Date:", game_date_str, "<br>", input$dash_stat, ":", stat_value))) +
      geom_bar(stat = "identity", aes(fill = stat_value >= bet_line)) +
      geom_hline(yintercept = bet_line, linetype = "dashed", color = "blue") +
      scale_fill_manual(
        values = c("TRUE" = "#2ECC71", "FALSE" = "#E74C3C"),
        labels = c("TRUE" = paste("Over", bet_line), "FALSE" = paste("Under", bet_line)),
        guide = guide_legend(title = NULL)
      ) +
      labs(
        title = paste(input$dash_player, "-", input$dash_stat, "(", timeframe_text, ")"),
        x = "Game Date",
        y = input$dash_stat
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
    if(input$dash_show_trend && nrow(df) >= 3){
      p <- p + geom_line(aes(y = moving_avg, group = 1), color = "darkorange", size = 1)
    }
    ggplotly(p, tooltip = "text")
  })
  
  # Render the Game Log Table using table_data.
  output$dash_statTable <- renderReactable({
    df <- table_data()
    req(df)
    cols <- c("game_date", "opponent_team_name", "minutes", "points", "rebounds", "assists", "steals", "blocks", "turnovers")
    df <- df %>% arrange(desc(game_date)) %>% mutate(game_date = as.character(game_date))
    reactable(
      df %>% select(all_of(cols), over_bet_line),
      defaultPageSize = nrow(df),
      highlight = TRUE,
      striped = TRUE,
      bordered = TRUE,
      columns = list(
        over_bet_line = colDef(
          name = paste(input$dash_stat, "vs Bet Line"),
          align = "center",
          cell = function(value){
            if(is.na(value)) htmltools::tags$span("NA")
            else if(value == "Over") htmltools::tags$span(style = "color: #2ECC71; font-weight: bold;", value)
            else htmltools::tags$span(style = "color: #E74C3C; font-weight: bold;", value)
          }
        )
      ),
      theme = reactableTheme(
        cellPadding = "8px",
        headerStyle = list(backgroundColor = "#f7f7f8", fontSize = "0.9rem", fontWeight = "bold", textAlign = "center"),
        cellStyle = list(textAlign = "center")
      )
    )
  })
  
  # Render Summary Row above the table.
  output$dash_summary <- renderUI({
    df <- graph_data()
    req(df)
    total_games <- nrow(df)
    over_count <- sum(df$stat_value >= input$dash_bet_line, na.rm = TRUE)
    over_percentage <- round((over_count / total_games) * 100, 1)
    avg_stat <- round(mean(df$stat_value, na.rm = TRUE), 1)
    max_stat <- max(df$stat_value, na.rm = TRUE)
    min_stat <- min(df$stat_value, na.rm = TRUE)
    HTML(paste0(
      '<div style="display: flex; justify-content: space-around; margin-bottom: 10px;">',
      '<div style="background: #f7f7f8; padding: 10px; flex: 1; margin: 5px; text-align: center;"><h4>Over Count</h4><p>', over_count, ' of ', total_games, ' (', over_percentage, '%)</p></div>',
      '<div style="background: #f7f7f8; padding: 10px; flex: 1; margin: 5px; text-align: center;"><h4>Average ', input$dash_stat, '</h4><p>', avg_stat, '</p></div>',
      '<div style="background: #f7f7f8; padding: 10px; flex: 1; margin: 5px; text-align: center;"><h4>Max ', input$dash_stat, '</h4><p>', max_stat, '</p></div>',
      '<div style="background: #f7f7f8; padding: 10px; flex: 1; margin: 5px; text-align: center;"><h4>Min ', input$dash_stat, '</h4><p>', min_stat, '</p></div>',
      '</div>'
    ))
  })
  
  # Download Handler for raw game logs.
  output$dash_downloadData <- downloadHandler(
    filename = function(){
      if(input$dash_download_format == "Excel"){
        paste0("nba_game_logs_", Sys.Date(), ".xlsx")
      } else {
        paste0("nba_game_logs_", Sys.Date(), ".csv")
      }
    },
    content = function(file){
      df <- filtered_data()
      if(is.null(df)) return(NULL)
      if(input$dash_download_format == "Excel"){
        openxlsx::write.xlsx(df, file)
      } else {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  
  # Leaderboard Tab: Compute leaderboard data using the selected stat (including combo stats).
  leaderboard_data <- reactive({
    req(nba_data(), input$lb_stat, input$lb_season)
    df <- nba_data()
    # Exclude unwanted team names.
    df <- df %>% filter(!(team_name %in% c("All-Star", "Team Kenny", "Team Candace", "Team Chuck", "Team Shaq")))
    current_season_selected <- if(input$lb_season == "last_season") {
      last_season
    } else {
      current_season
    }
    df <- df %>% filter(season == current_season_selected)
    stat <- input$lb_stat
    if(stat == "points_assists"){
      df$combo <- df$points + df$assists
    } else if(stat == "points_rebounds"){
      df$combo <- df$points + df$rebounds
    } else if(stat == "assists_rebounds"){
      df$combo <- df$assists + df$rebounds
    } else if(stat == "points_assists_rebounds"){
      df$combo <- df$points + df$assists + df$rebounds
    } else {
      df$combo <- df[[stat]]
    }
    lb <- df %>% group_by(athlete_display_name) %>% 
      summarise(avg_stat = round(mean(combo, na.rm = TRUE), 1),
                games = n()) %>% arrange(desc(avg_stat))
    lb
  })
  
  output$lb_leaderboard <- renderReactable({
    lb <- leaderboard_data()
    req(nrow(lb) > 0)
    reactable(
      lb,
      columns = list(
        athlete_display_name = colDef(name = "Player", align = "center"),
        avg_stat = colDef(name = paste("Avg", input$lb_stat), align = "center"),
        games = colDef(name = "Games", align = "center")
      ),
      defaultPageSize = nrow(lb),
      striped = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      theme = reactableTheme(
        cellPadding = "8px",
        headerStyle = list(backgroundColor = "#f7f7f8", fontSize = "0.9rem", fontWeight = "bold", textAlign = "center"),
        cellStyle = list(textAlign = "center")
      )
    )
  })
}

# ---------------------------
# Run the Application 
# ---------------------------
shinyApp(ui = ui, server = server)