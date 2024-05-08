box::use(
  here,
  dplyr,
  DT,
  readr,
  tidyr,
  recipes,
  shiny[fluidPage, sidebarLayout, updateSelectInput, sidebarPanel, numericInput, selectInput, downloadButton, mainPanel, shinyApp, observe, downloadHandler],
  parsnip,
  workflows,
  broom,
  tibble,
  stringr,
  wehoop_stats = . / get_game_stats,
  wehoop_analytics = . / analytic_goals
)

team_names <- as.list(unique(wehoop_stats$get_boxscores(2023)$team_display_name))

ui <- fluidPage(
  titlePanel("WBB Metric Goals"),
  sidebarLayout(
    sidebarPanel(
      selectInput("name", "Team Name", team_names, selected = "Clemson Tigers", multiple = FALSE),
      numericInput("year", "Season", 2024),
      selectInput("game_ids", "Scheduled Games", c(), multiple = FALSE),
      br(),
      downloadButton("download_data", "Download as CSV")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Opponent Games", DT$DTOutput("stat_table")),
                  tabPanel("Relevant Team Metrics",
                           fluidRow(
                             column(width = 12, plotOutput("regression_plot")),
                             column(width = 12, DT$DTOutput("metric_table"))
                           )
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    boxscores <- wehoop_stats$get_boxscores(input$year)
    team_name <- input$name
    team_data <- boxscores |>
      dplyr$filter(stringr::str_detect(team_display_name, team_name))
    opp_info <- split(team_data$game_id, paste0(team_data$game_date, ": ", team_data$team_display_name_opponent))
    updateSelectInput(session, "game_ids", choices = opp_info, selected = NULL)
  
    game_stats <- reactive({
      boxscores <- wehoop_stats$get_boxscores(input$year)
      team_data <- boxscores |>
        dplyr$filter(stringr::str_detect(team_display_name, input$name))
      opp_data <- team_data |>
        dplyr$filter(game_id == input$game_ids)
      opp_name <- opp_data$team_display_name_opponent
      game_date <- opp_data$game_date
      input_data <- wehoop_stats$get_past_opps(boxscores, opp_name, as.Date(game_date))
      wehoop_stats$get_game_stats(input_data)
    })
    
    metrics <- reactive({
      wehoop_analytics$get_relevant_metrics(game_stats())
    })
    
    output$stat_table <- DT$renderDT ({
      game_stats()
    })
    
    output$metric_table <- DT$renderDT ({
      metrics()
    }, selection = 'single')
    
    output$regression_plot <- renderPlot ({
      ndx <- ifelse(is.null(input$metric_table_rows_selected), 1, input$metric_table_rows_selected) 
      plot(game_stats()[[metrics()$metric[ndx]]], game_stats()$teamMargin,
           main = paste(game_stats()$teamName[1], ": ", metrics()$metric[ndx], " vs. Score Differential"),
           xlab = metrics()$metric[ndx], ylab = "Score Differential",
           pch = 19, frame = FALSE)
      abline(lm(game_stats()$teamMargin ~ game_stats()[[metrics()$metric[ndx]]]), col = "blue")
    })
    
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("game_stats_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(game_stats(), file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
  })
}

shinyApp(ui, server)