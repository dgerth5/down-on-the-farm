library(readr)
library(tidyverse)

fsl2023 <- read_csv("fsl2023.csv")

unique(fsl2023$home_league_name)

fsl_tot <- fsl2023 %>%
  filter(home_league_name == "Florida State League") %>%
  select(matchup.pitcher.fullName,details.type.description,pitchData.startSpeed,pitchData.coordinates.pfxX,pitchData.coordinates.pfxZ) %>%
  rename("Name" = matchup.pitcher.fullName,
         "PitchType" = details.type.description,
         "Velocity" = pitchData.startSpeed,
         "HMov" = pitchData.coordinates.pfxX,
         "VMov" = pitchData.coordinates.pfxZ) %>%
  mutate(HMov = HMov,
         VMov = VMov) %>%
  arrange("Name") %>%
  drop_na()

fsl_agg <- fsl2023 %>%
  filter(home_league_name == "Florida State League") %>%
  group_by(matchup.pitcher.fullName, details.type.description) %>%
  summarise(velo = mean(pitchData.startSpeed, na.rm = TRUE),
            hmov = mean(pitchData.coordinates.pfxX, na.rm = TRUE),
            vmov = mean(pitchData.coordinates.pfxZ, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na() %>%
  rename("Name" = matchup.pitcher.fullName,
         "PitchType" = details.type.description,
         "Velocity" = velo,
         "HMov" = hmov,
         "VMov" = vmov)

library(shiny)
library(dplyr)
library(ggplot2)
library(gt)
library(gridExtra)

# Assuming fsl_tot and fsl_agg are already prepared

# UI
ui <- fluidPage(
  titlePanel("Florida State League Pitching Summary"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_player", "Select Player:", choices = sort(unique(fsl_tot$Name)))  # Sort the player names alphabetically
    ),
    mainPanel(
      gt_output("pitchTable"),
      plotOutput("pitchPlot")
    )
  )
)

server <- function(input, output) {
  
  # Filtered data for the plots
  filtered_plot_data <- reactive({
    fsl_tot %>% filter(Name == input$selected_player)
  })
  
  # Filtered data for the gt table with alphabetical sorting
  filtered_table_data <- reactive({
    fsl_agg %>%
      filter(Name == input$selected_player) %>%
      arrange(PitchType)  # Sort PitchType alphabetically
  })
  
  # GT table output
  output$pitchTable <- render_gt({
    data <- filtered_table_data()
    
    # Create gt table
    data %>%
      gt() %>%
      tab_header(
        title = md("**Aggregated Pitching Data**"),
        subtitle = md(paste("Player:", input$selected_player))
      ) %>%
      cols_label(
        PitchType = "Pitch Type",
        Velocity = "Velocity (mph)",
        HMov = "Horizontal Movement (in)",
        VMov = "Vertical Movement (in)"
      ) %>%
      fmt_number(
        columns = vars(Velocity, HMov, VMov),
        decimals = 1
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(everything())
      )
  })
  
  # Plot output
  output$pitchPlot <- renderPlot({
    data <- filtered_plot_data()
    
    # Velocity distribution
    velocity_plot <- ggplot(data, aes(x = Velocity, fill = PitchType)) +
      geom_density(alpha = 0.7) +
      labs(title = "Pitch Velocity Distribution", x = "Velocity (mph)", y = "Density") +
      theme_minimal()
    
    # Horizontal and Vertical Movement Plot
    movement_plot <- ggplot(data, aes(x = HMov, y = VMov, color = PitchType)) +
      geom_point(size = 3) +
      labs(title = "Pitch Movement", x = "Horizontal Movement (in)", y = "Vertical Movement (in)") +
      theme_minimal()
    
    # Combine plots
    gridExtra::grid.arrange(velocity_plot, movement_plot, ncol = 2)
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

