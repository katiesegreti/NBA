function(input, output, session) {
  
  
  output$testing <- renderUI({
    #str(player_stats_1())
  })
  
  #team selector
  output$team_selector <- renderUI({
    selectInput(
      inputId = "team_selector",
      label = "Select team:",
      choices = team_names
    )
    
  })
  #get players on selected team
  #gonna need to change tm to current_team
  selected_team <- reactive({
    req(input$team_selector)
    players_2021a %>% 
    filter(tm == input$team_selector) %>%
      select(player, pos)
  })
  #long team name
  selected_team_fullname <- reactive({
    req(input$team_selector)
    names(team_names)[team_names == input$team_selector]
  })
  
  #player selector
  output$player_selector <- renderUI({
    req(input$team_selector)
    selectInput(
      inputId = "player_selector",
      label = "Select player:",
      choices = c("", selected_team()$player)
    )
  })
  
  #text message to select a player from the table
  output$player_selection <- renderText({
    req(selected_team())
    "Select a player to see their stats"
  })
  
  
  #reactable for selected team
  # output$team_table <- renderReactable({
  #   reactable(selected_team(),
  #             defaultPageSize = 20,
  #             onClick = "select",
  #             selection = "single",
  #             columns = list(
  #               player = colDef(
  #                 name = "Player"
  #               ),
  #               pos = colDef(
  #                 name = "Position",
  #                 maxWidth = 80
  #               ),
  #               dob = colDef(
  #                 name = "DOB",
  #                 format = colFormat(date = TRUE, locales = "en-US"),
  #                 maxWidth = 100
  #               ),
  #               height = colDef(
  #                 name = "Height",
  #                 maxWidth = 80
  #               ),
  #               weight = colDef(
  #                 name = "Weight",
  #                 maxWidth = 80
  #               ),
  #               college = colDef(
  #                 name = "College/Country"
  #               )
  #             ),
  #             highlight = TRUE,
  #             striped = TRUE,
  #             bordered = TRUE,
  #             theme = reactableTheme(
  #               borderColor = "#FF671F",
  #               stripedColor = "#f6f8fa",
  #               highlightColor = "lightblue"
  #             ))
  # })
  
  selected_player <- reactive({
    req(selected_team())
    getReactableState("team_table", "selected")
  })
  selected_name <- reactive({
    req(input$player_selector)
    #selected_team()$player[selected_player()]
    input$player_selector
  })
  selected_bio <- reactive({
    req(selected_name())
    nba_bio_2021 %>%
      filter(player == selected_name()) 
  })
  
  #get stats/history for selected player
  player_stats_1 <- reactive({
    req(selected_name())
    today_with_history %>%
      filter(player == selected_name()) %>%
      select(season, tm, age, g, gs, mp, fg, fga, fgpercent, x3p, x3pa, x3ppercent,
             x2p, x2pa, x2ppercent, ft, fta, ftpercent, orb, drb, trb, ast,
             stl, blk, tov, pf, pts) %>%
      arrange(desc(season))
  })
  #player's name
  output$just_name <- renderUI({
    req(input$player_selector)
    fluidRow(
      column(
        width = 12,
        h3(selected_name())
      )
    )
  })
  
  #bio details
  output$player_bio <- renderUI({
    req(input$player_selector)
    fluidRow(
      column(
        width = 4,
        p(paste0(selected_team_fullname()), " • ", selected_bio()$position[1])
      ),
      column(
        width = 4,
        p(paste0("Height: ", selected_bio()$ht[1])),
        HTML(paste0("Age:", br(), selected_bio()$age[1]))
      ),
      column(
        width = 4,
        p(paste0("Weight: ", selected_bio()$wt[1])),
        HTML(paste0("College:", br(), selected_bio()$college[1]))
      )
    )
  })
  
  #history chart metric selector
  output$metric_history <- renderUI({
    req(selected_name())
    selectInput(
      inputId = "metric_history",
      label = "Select stat for chart:",
      choices = c("points", "FG%", "3P%", "rebounds", "assists", "blocks", "steals")
    )
  })
  
  #comparison chart metric selector
  output$metric_compare <- renderUI({
    req(selected_name())
    selectInput(
      inputId = "metric_compare",
      label = "Select stat for charts:",
      choices = c("points", "FG%", "3P%", "rebounds", "assists", "blocks", "steals")
    )
  })
  
  metric_to_compare <- reactive({
    req(input$metric_compare)
    input$metric_compare
  })
  

  
  #make chart for player history
  output$history_chart <- renderPlot({
    req(input$metric_history)
    if(input$metric_history == "points") {
      avgpoint_chart(selected_name())
    } else if(input$metric_history == "FG%") {
      fgpct_chart(selected_name())
    } else if(input$metric_history == "3P%") {
      threepct_chart(selected_name())
    } else if(input$metric_history == "rebounds") {
      rebound_chart(selected_name())
    } else if(input$metric_history == "assists") {
      assist_chart(selected_name())
    } else if(input$metric_history == "blocks") {
      block_chart(selected_name())
    } else if(input$metric_history == "steals") {
      steal_chart(selected_name())
    }
    
  })
    
  #make team comparison chart
  output$comparison_team <- renderPlot({
    req(metric_to_compare())
    if(input$player_selector != "" & !is.null(input$player_selector)) {
      if(input$metric_compare == "points"){
        team_compare_player_pts(selected_name(), input$team_selector, selected_team_fullname())
      } else if(metric_to_compare() == "FG%") {
        team_compare_player_fg(selected_name(), input$team_selector, selected_team_fullname())
      } else if(metric_to_compare() == "3P%") {
        team_compare_player_3p(selected_name(), input$team_selector, selected_team_fullname())
      } else if(metric_to_compare() == "rebounds") {
        team_compare_player_rebounds(selected_name(), input$team_selector, selected_team_fullname())
      } else if(metric_to_compare() == "assists") {
        team_compare_player_assists(selected_name(), input$team_selector, selected_team_fullname())
      } else if(metric_to_compare() == "blocks") {
        team_compare_player_blocks(selected_name(), input$team_selector, selected_team_fullname())
      } else if(metric_to_compare() == "steals") {
        team_compare_player_steals(selected_name(), input$team_selector, selected_team_fullname())
      }
      
    }
    
  })

  #make league comparison chart
  output$comparison_league <- renderPlot({
    req(metric_to_compare())
    if(input$player_selector != "" & !is.null(input$player_selector)) {
      if(metric_to_compare() == "points") {
        league_compare_player_pts(selected_name())
      } else if(metric_to_compare() == "FG%") {
        league_compare_player_fg(selected_name())
      } else if(metric_to_compare() == "3P%") {
        league_compare_player_3p(selected_name())
      } else if(metric_to_compare() == "rebounds") {
        league_compare_player_rebounds(selected_name())
      } else if(metric_to_compare() == "assists") {
        league_compare_player_assists(selected_name())
      } else if(metric_to_compare() == "blocks") {
        league_compare_player_blocks(selected_name())
      } else if(metric_to_compare() == "steals") {
        league_compare_player_steals(selected_name())
      }
    }
  })
  
  #shots chart
  output$shots_chart <- renderPlot({
    req(selected_name())
    if(input$player_selector != "" & !is.null(input$player_selector)) {
      shooting_chart(selected_name())
    }
  })
  
  # tabs for player stats
  output$player_tabs <- renderUI({
    req(selected_name())
    tabsetPanel(
      id = "playerstats",
      tabPanel("stats history",
               shiny::tags$br(),
               fluidRow(
                 column(
                   width = 6,
                   #reactable for selected player
                   output$player_table <- renderReactable({
                     reactable(player_stats_1(),
                               columns = list(
                                 season = colDef(
                                   name = "Season",
                                   maxWidth = 70,
                                   style = sticky_style,
                                   headerStyle = sticky_style
                                 ),
                                 tm = colDef(
                                   name = "Team",
                                   maxWidth = 67
                                 ),
                                 age = colDef(
                                   name = "Age",
                                   maxWidth = 55
                                 ),
                                 g = colDef(
                                   name = "G",
                                   maxWidth = 45
                                 ),
                                 gs = colDef(
                                   name = "GS",
                                   maxWidth = 45
                                 ),
                                 mp = colDef(
                                   name = "MP",
                                   maxWidth = 55
                                 ),
                                 fg = colDef(
                                   name = "FGM",
                                   maxWidth = 65
                                 ),
                                 fga = colDef(
                                   name = "FGA",
                                   maxWidth = 65
                                 ),
                                 fgpercent = colDef(
                                   name = "FG%",
                                   maxWidth = 65,
                                   format = colFormat(percent = TRUE, digits = 1)
                                 ),
                                 x3p = colDef(
                                   name = "3PM",
                                   maxWidth = 65
                                 ),
                                 x3pa = colDef(
                                   name = "3PA",
                                   maxWidth = 65
                                 ),
                                 x3ppercent = colDef(
                                   name = "3P%",
                                   maxWidth = 65,
                                   format = colFormat(percent = TRUE, digits = 1)
                                 ),
                                 x2p = colDef(
                                   name = "2PM",
                                   maxWidth = 65
                                 ),
                                 x2pa = colDef(
                                   name = "2PA",
                                   maxWidth = 65
                                 ),
                                 x2ppercent = colDef(
                                   name = "2P%",
                                   maxWidth = 65,
                                   format = colFormat(percent = TRUE, digits = 1)
                                 ),
                                 ft = colDef(
                                   name = "FTM",
                                   maxWidth = 65
                                 ),
                                 fta = colDef(
                                   name = "FTA",
                                   maxWidth = 65
                                 ),
                                 ftpercent = colDef(
                                   name = "FT%",
                                   maxWidth = 65,
                                   format = colFormat(percent = TRUE, digits = 1)
                                 ),
                                 orb = colDef(
                                   name = "OREB",
                                   maxWidth = 65
                                 ),
                                 drb = colDef(
                                   name = "DREB",
                                   maxWidth = 65
                                 ),
                                 trb = colDef(
                                   name = "REB",
                                   maxWidth = 65
                                 ),
                                 ast = colDef(
                                   name = "AST",
                                   maxWidth = 65
                                 ),
                                 stl = colDef(
                                   name = "STL",
                                   maxWidth = 65
                                 ),
                                 blk = colDef(
                                   name = "BLK",
                                   maxWidth = 65
                                 ),
                                 tov = colDef(
                                   name = "TOV",
                                   maxWidth = 65
                                 ),
                                 pf = colDef(
                                   name = "PF",
                                   maxWidth = 65
                                 ),
                                 pts = colDef(
                                   name = "PTS",
                                   maxWidth = 65
                                 )
                               ),
                               highlight = TRUE,
                               striped = TRUE,
                               bordered = TRUE,
                               fullWidth = FALSE,
                               theme = reactableTheme(
                                 borderColor = "#FF671F",
                                 stripedColor = "#f6f8fa",
                                 highlightColor = "lightblue"
                               )
                     )
                   })
                   
                 )
               ),
               
               shiny::tags$br(),
               uiOutput("metric_history"),
               fluidRow(
                 column(
                   width = 12,
                   plotOutput("history_chart") 
                 )
               )
               
      ),
      tabPanel("league/team comparisons",
               #select input for comparison metrics
               uiOutput("metric_compare"),
               fluidRow(
                 column(
                   width = 12,
                   plotOutput("comparison_team")
                 )
               ),
               shiny::tags$br(),
               fluidRow(
                 column(
                   width = 12,
                   plotOutput("comparison_league")
                 )
               )
               
               
      )#,
      # tabPanel("shooting",
      #          shiny::tags$br(),
      #          fluidRow(
      #            column(
      #              width = 8,
      #              plotOutput("shots_chart")
      #            )
      #          )
      #          )
    )
  })
  
}