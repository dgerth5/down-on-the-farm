library(shiny)
library(mgcv)
library(dplyr)
library(shinydashboard)
library(shinyjs)

mod2 <- readRDS("elev_mod.rds")

ui <- dashboardPage(
  dashboardHeader(title = ""),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
      .main-header {
        height: 50px;
      }
    "))),
    fluidRow(
      column(12, h2("Expected IVB – Including Elevation", align = "center", style = "margin-top: 20px;")),
      column(2, selectInput("hand", "Hand", c("R", "L"))),
      column(2, numericInput("velocity", "Velocity", value = 90)),
      column(2, numericInput("spinRate", "Spin Rate", value = 2200)),
      column(2, numericInput("horRelease", "Horizontal Release", value = -1)),
      column(2, numericInput("verRelease", "Vertical Release", value = 5.5)),
      column(2, actionButton("calculate", "Calculate"))
    ),
    fluidRow(
      column(2, numericInput("spinDirection", "Spin Direction", value = 300)),
      column(2, numericInput("extension", "Extension", value = 5.5)),
      column(2, numericInput("elevation_mlb", "Elevation-MLB", value = 100)),
      column(2, numericInput("elevation_aaa", "Elevation-AAA", value = 400))
    ),
    fluidRow(
      column(4, textOutput("result_mlb")),
      column(4, textOutput("result_aaa")),
      column(4, textOutput("result_diff"))
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculate, {
    inputData_mlb <- data.frame(
      adj_sd = if(input$hand == "L") 360 - input$spinDirection else input$spinDirection,
      pitchData.breaks.spinRate = input$spinRate,
      pitchData.extension = input$extension,
      pitchData.startSpeed = input$velocity,
      adj_x0 = if(input$hand == "L") -1 * input$horRelease else input$horRelease,
      pitchData.coordinates.z0 = input$verRelease,
      Elevation = input$elevation_mlb
    )
    
    inputData_aaa <- inputData_mlb
    inputData_aaa$Elevation <- input$elevation_aaa
    
    result_mlb <- predict(mod2, newdata = inputData_mlb)
    result_aaa <- predict(mod2, newdata = inputData_aaa)
    
    output$result_mlb <- renderText({ paste("Expected IVB-MLB: ", round(result_mlb, 2)) })
    output$result_aaa <- renderText({ paste("Expected IVB-AAA: ", round(result_aaa, 2)) })
    output$result_diff <- renderText({ paste("IVB Diff: ", round(result_mlb - result_aaa, 2)) })
    
    runjs("$('#result_mlb').css({'color':'blue','font-size':'200%','font-weight':'bold'});
       $('#result_aaa').css({'color':'red','font-size':'200%','font-weight':'bold'});
       $('#result_diff').css({'color':'black','font-size':'200%','font-weight':'bold'});")
  })
}

shinyApp(ui = ui, server = server)
