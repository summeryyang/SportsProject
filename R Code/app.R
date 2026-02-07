library(shiny)
library(shinydashboard)
library(bslib)
library(plotly)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(bsicons)
library(DT)
library(rsconnect)
library(readxl)
library(reactable)
library(tidymodels)
library(gt)
library(scales)
library(xgboost)

# Load Salary Data
nba.salary_fullpred <- read_csv("https://raw.githubusercontent.com/summeryyang/SportsProject/main/nba.salary_fullpred.csv")

nba.salary_fullpred$Predicted_Salary <- round(nba.salary_fullpred$Predicted_Salary)

nba.salary_fullpred <- nba.salary_fullpred %>%
  mutate(Difference = Salary - Predicted_Salary) 

# Download variable importance .rds file to a temporary location
temp_file <- tempfile(fileext = ".rds")
download.file("https://raw.githubusercontent.com/summeryyang/SportsProject/main/nbasalary.imp.xgb.rds", temp_file)

# read .rds file
nbasalary.imp.xgb <- readRDS(temp_file)

# Workflow
temp_file2 <- tempfile(fileext = ".rds")
download.file("https://raw.githubusercontent.com/summeryyang/SportsProject/main/fit_workflow.rds", temp_file2)
fit_workflow <- readRDS(temp_file2)


# Define UI
ui <- navbarPage(
  title = "NBA Salary Analysis",
  theme = bs_theme(preset = "yeti",
                   primary = "#E74C3C",  
                   secondary = "#3498db",  
                   navbar_bg = "#2E4DA7"  # Set the navigation bar background color (does not work :( )
                   ),
  
  # Home Tab
  tabPanel(
    "Home",
    
    # Title 
    div(style = "text-align: center; font-size: 48px; font-weight: bold; color: #2c3e50;",
        "Welcome to the NBA Salary Analysis App"),
    
    # Subtitle 
    div(style = "text-align: center; font-size: 22px; font-style: italic; color: #34495e;",
        p("Explore insights into player salaries and make predictions using your own stats!")),
    
    # Dashboard Description
    div(style = "font-size: 16px; line-height: 1.6; color: #7f8c8d; margin: 20px 0;",
        p("In the Dashboard tab, filter by Season, Team, and Position to view the Average and Overall Differences between Predicted and Actual Aalaries. 
          For the Rank box, you can compare a team's Salary Difference to other teams and players in the same position for that season, with rank 1 being the smallest Salary Difference and 30 being the biggest. 
      The table displays Player, Predicted Salary, Actual Salary, and Salary Difference, while the scatterplot visualizes the Actual vs Predicted Salary values.")),
    
    # Salary predict description
    div(style = "font-size: 16px; line-height: 1.6; color: #7f8c8d; margin: 20px 0;",
        p("In the Salary Prediction tab, you can upload your own stats to generate a salary prediction for yourself. 
          The variables for your data all need to be named correctly, so download the template provided to avoid any errors! The variable importance plot shows the top 10 factors that incluence salary.")),
    
    # Image
    div(style = "text-align: center; margin: 20px 0;",
        img(src = "https://wallpapers.com/images/hd/best-nba-zn90xecrm49mlwbx.jpg", height = "600px", width = "1000px", style = "border: 5px solid #ecf0f1; border-radius: 10px;"),
        p(style = "font-size: 14px; color: #7f8c8d;", 
          "Image credit: ", 
          a(href = "https://wall.alphacoders.com/big.php?i=467394", 
            target = "_blank", 
            "https://wall.alphacoders.com/big.php?i=467394")))
  ),
  
  
  # Dashboard Tab
  tabPanel(
    "Dashboard",
    page_sidebar(
      # Sidebar Layout
      sidebar = sidebar(
        title = "Dashboard Controls",
        
        # Season Input
        selectInput(
          "varSeason", 
          label = "Select Season",
          choices = c("All Seasons", unique(nba.salary_fullpred$Season)),
          selected = unique(nba.salary_fullpred$Season)[1] 
        ),
        
        # Team Input
        selectInput(
          "varTeam", 
          label = "Select Team",
          choices = sort(unique(nba.salary_fullpred$Team)),
          selected = sort(unique(nba.salary_fullpred$Team))[1]  
        ),
        
        # Position Input
        selectInput(
          "varPos", 
          label = "Select Position",
          choices = c("All Positions", "C", "PF", "PG", "SF", "SG"),
          selected = "All Positions"
        ),
        
        div(
          style = "position: bottom; text-align: center",
          img(src = "https://cdn.freebiesupply.com/images/large/2x/nba-logo-transparent.png", width = "80%")
        )
      ),      
  
          
      # Dashboard Layout
      layout_columns(
        fill = FALSE,
        
        # Value Boxes
        value_box(
          title = "Average Difference",
          value = textOutput("avg"),
          showcase = bsicons::bs_icon("currency-dollar", style = "color: #2E4DA7;"),
          style = "color: #2E4DA7; border: 1.5px solid #2E4DA7; padding: 20px; font-size: 18px; font-weight: bold;"
        ),
        value_box(
          title = "Overall Difference",
          value = textOutput("overall"),
          showcase = bsicons::bs_icon("currency-dollar", style = "color: #2E4DA7;"),
          style = "color: #2E4DA7; border: 1.5px solid #2E4DA7; padding: 20px; font-size: 18px; font-weight: bold;"
        ),
        value_box(
          title = "Rank",
          value = textOutput("rank"),
          showcase = bsicons::bs_icon("trophy", style = "color: #2E4DA7;"),
          style = "color: #2E4DA7; border: 1.5px solid #2E4DA7; padding: 20px; font-size: 18px; font-weight: bold;"
        )
      ),
      
      # Plots
      layout_columns(
        card(
          card_header("Table of Actual vs. Predicted Salary", style = "background-color: #2E4DA7; color: white; font-weight: bold;"),
          reactableOutput("tablePlot")
        ),
        card(
          card_header("Scatterplot of Actual vs. Predicted Salary", style = "background-color: #2E4DA7; color: white; font-weight: bold;"),
          conditionalPanel(condition = "input.varSeason == 'All Seasons'",
                           textInput("playerSearch", label = NULL, value = "", placeholder = "Enter player name")),
          plotlyOutput("scatterPlot")
        )
      )
    )
  ),
  
  # Predict Salary Tab
  tabPanel(
    "Salary Prediction",
    page_sidebar(
      # Sidebar Layout
      sidebar = sidebar(
        title = "Upload File",
        
        # File Upload Input
        fileInput(
          "fileUpload", 
          label = "Upload CSV File:",
          accept = c(".csv")
        ),
        
        actionButton("predictButton", "Generate Predictions"),
        
        # Download Template Button
        div(style = "font-size: 14px",
            downloadButton(
              "downloadTemplate", 
              label = "Template.csv"
          ),
          p("Leave Salary and logsalary blank!"),
          p("Once data is uploaded, press the 'Generate Predictions' button to generate the table.")
        ),
        
        div(
          style = "position: bottom; text-align: center",
          img(src = "https://cdn.freebiesupply.com/images/large/2x/nba-logo-transparent.png", width = "80%")
        )
      ),
      
      # Main Panel Layout
      layout_columns(
        fill = FALSE,
        
        # Prediction Output and Variable Importance 
        card(
          card_header("Predicted Salary", style = "background-color: #2E4DA7; color: white; font-weight: bold;"),
          gt_output("predictionTable")  
        ),
        card(
          card_header("Variable Importance", style = "background-color: #2E4DA7; color: white; font-weight: bold;"),
          plotOutput("importancePlot")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive filtered data based on user inputs
  filtered_data <- reactive({
    data <- nba.salary_fullpred
    
    # If "All Seasons" is selected, do not filter by season
    if (input$varSeason != "All Seasons") {
      data <- data %>% filter(Season == input$varSeason)
    }
    
    if (!is.null(input$varTeam)) {
      data <- data %>% filter(Team == input$varTeam)
    }
    if (input$varPos != "All Positions") {
      data <- data %>% filter(Pos == input$varPos)
    }
    
    data
  })
  
  # Calculate Average Difference
  output$avg <- renderText({
    data <- filtered_data()
    
    avg_diff <- round(mean(data$Difference, na.rm = TRUE))
    paste0("$", formatC(avg_diff, format = "f", big.mark = ",", digits = 0))
  })
  
  # Calculate Overall Difference
  output$overall <- renderText({
    data <- filtered_data()
    
    overall_diff <- round(sum(data$Difference, na.rm = TRUE))
    paste0("$", formatC(overall_diff, format = "f", big.mark = ",", digits = 0))
  })
  
  # Calculate Rank
  output$rank <- renderText({
    # Filter data based on selected season and position
    rank_data <- nba.salary_fullpred %>%
      filter(
        (input$varSeason == "All Seasons" | Season == input$varSeason) & 
          (input$varPos == "All Positions" | Pos == input$varPos)
      ) %>%
      group_by(Team) %>%
      summarise(OverallDifference = abs(sum(Difference, na.rm = TRUE))) %>%
      ungroup() %>%
      arrange(OverallDifference)  # Rank in descending order
    
    # Get the rank for the selected team
    selected_team <- rank_data %>%
      filter(Team == input$varTeam)
    
    if (nrow(selected_team) == 0) {
      return("No data available")
    }
    
    # Find the rank
    team_rank <- which(rank_data$Team == input$varTeam)
    total_teams <- nrow(rank_data)
    
    paste0(team_rank, " out of ", total_teams)
  })
  
  
  # Table of Actual vs. Predicted Salary
  output$tablePlot <- renderReactable({
    data <- filtered_data()
    
    # Conditionally include the Season column
    if (input$varSeason == "All Seasons") {
      table_data <- data %>%
        select(Player, Season, Salary, Predicted_Salary, Difference) %>%
        arrange(desc(abs(Difference)))  # Arrange by absolute value of Difference
      
      columns <- list(
        Player = colDef(name = "Player", filterable = TRUE),
        Season = colDef(name = "Season"),
        Predicted_Salary = colDef(name = "Predicted Salary", cell = function(value) scales::dollar(value)),
        Salary = colDef(name = "Actual Salary", cell = function(value) scales::dollar(value)),
        Difference = colDef(name = "Difference", cell = function(value) scales::dollar(value), style = function(value) {
          normalized <- (abs(value) - min(abs(table_data$Difference))) /
            (max(abs(table_data$Difference)) - min(abs(table_data$Difference)))
          color <- grDevices::colorRampPalette(c("white", "#ff0000"))(100)[round(normalized * 99) + 1]
          list(background = color)
        })
      )
    } else {
      table_data <- data %>%
        select(Player, Salary, Predicted_Salary, Difference) %>%
        arrange(desc(abs(Difference)))  # Arrange by absolute value of Difference
      
      columns <- list(
        Player = colDef(name = "Player", filterable = TRUE),
        Predicted_Salary = colDef(name = "Predicted Salary", cell = function(value) scales::dollar(value)),
        Salary = colDef(name = "Actual Salary", cell = function(value) scales::dollar(value)),
        Difference = colDef(name = "Difference", cell = function(value) scales::dollar(value), style = function(value) {
          normalized <- (abs(value) - min(abs(table_data$Difference))) /
            (max(abs(table_data$Difference)) - min(abs(table_data$Difference)))
          color <- grDevices::colorRampPalette(c("white", "#ff0000"))(100)[round(normalized * 99) + 1]
          list(background = color)
        })
      )
    }
    
    reactable(
      table_data,
      columns = columns,
      defaultPageSize = 10,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE
    )
  })
  
  # Scatterplot of Actual vs. Predicted Salary
  output$scatterPlot <- renderPlotly({
    data <- filtered_data()
    
    # Filter by player search input
    if (input$playerSearch != "") {
      data <- data %>%
        filter(grepl(input$playerSearch, Player, ignore.case = TRUE))
    }
    
    p <- ggplot(data, aes(x = Predicted_Salary, y = Salary)) +
      geom_point(aes(text = paste(Player,
                                  "<br>Actual Salary: $", format(Salary, big.mark = ","),
                                  "<br>Predicted Salary: $", format(Predicted_Salary, big.mark = ","),
                                  "<br>Difference: $", format(Difference, big.mark = ","))),
                 color = "#3498db", size = 2) +
      geom_abline(slope = 1, intercept = 0,color = "#e74c3c") + 
    labs(
        x = "Predicted Salary",
        y = "Actual Salary"
      ) +
      theme_minimal() +
      scale_x_continuous(labels = dollar_format()) +
      scale_y_continuous(labels = dollar_format())
    
    ggplotly(p, tooltip = "text")
  })
 
  # Store the uploaded data
  uploaded_data <- reactive({
    req(input$fileUpload)  
    read.csv(input$fileUpload$datapath)  # Read uploaded CSV
  })
  
  # Reactive expression to handle the prediction 
  predictions <- eventReactive(input$predictButton, {
    
    shiny::validate(
      need(input$fileUpload, "ERROR! Please upload data to generate predictions.")
    )
    
    if (nrow(uploaded_data()) == 0) {
      showModal(modalDialog(
        title = "Error",
        "ERROR! Please upload data to generate predictions.",
        footer = modalButton("Close"),
        size = "s",
        easyClose = TRUE,
        style = "color: #7f8c8d; font-weight: bold;"
      ))
      return(NULL)  # Stop further execution if no data is uploaded
    }
    
    # Generate predictions using the pre-fitted workflow
    prediction_results <- predict(fit_workflow, uploaded_data())
    
    # Exponentiate the predicted log salaries
    predicted_salary <- exp(prediction_results$.pred)
    
    # Combine predictions with the original uploaded data
    predicted_data <- uploaded_data() %>%
      mutate(Predicted_Salary = round(predicted_salary, digits=0)) 
    
    return(predicted_data)
  })
  
  # Prediction gt table
  output$predictionTable <- render_gt({
    req(predictions())  
    
    # Create gt table
    gt_table <- predictions() %>%
      select(Player, Predicted_Salary) %>%  
      gt() %>%
      fmt_currency(
        columns = Predicted_Salary,
        currency = "USD",
        decimals = 0
      ) %>%
      cols_label(
        Player = "Player",                     
        Predicted_Salary = "Salary" 
      ) %>%
      tab_header(title = "Predicted Salaries",
                 subtitle = "Generated using the uploaded data") %>% 
      tab_options(
        heading.title.font.size = px(30), 
        table.font.size = px(24)        
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightgray"),
          cell_text(weight = "bold")
        ),
        locations = cells_column_labels(everything()) 
      )
    
    return(gt_table)
  })
  
  
  # Variable Importance Plot
  output$importancePlot <- renderPlot({
    var.imp <- nbasalary.imp.xgb$results %>%
      select(feature, importance) %>%
      head(10) %>%
      mutate(feature = recode(feature, 
                              "years_played" = "Years Played",
                              "season" = "Season")) %>%
      mutate(feature = ifelse(feature == "X2P.", "2P%", feature))
    
    # Create the horizontal bar chart
    ggplot(var.imp, aes(x = reorder(feature, importance), y = importance)) +
      geom_bar(stat = "identity", fill = "#3498db") +
      labs(x = "Feature",
           y = "Importance") +
      theme_minimal() +
      coord_flip() +
      theme(
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16)  
      )
  })
  
  # Download handler for template
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      "Template.csv"
    },
    content = function(file) {
      download.file("https://github.com/summeryyang/SportsProject/raw/main/Template.csv", file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
