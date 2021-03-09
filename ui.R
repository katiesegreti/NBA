ui <- fluidPage(
  theme = shinytheme("paper"),
  #themeSelector(),
  
  # App title
  headerPanel("NBA players"),
  p("Check out an NBA player"),
  
  # team filter
  fluidRow(
    column(
      width = 4,
      uiOutput("team_selector")
    ),
    column(
      width = 4,
      uiOutput("player_selector")
    )
  ),
  
  #player bio details
  fluidRow(
    column(
      width = 10,
      uiOutput("just_name"),
      uiOutput("player_bio")
    ),
    uiOutput("testing")
  ),
  
  
  #testing
  #uiOutput("testing"),
  shiny::tags$br(),
  #player tabs
  uiOutput("player_tabs")

)