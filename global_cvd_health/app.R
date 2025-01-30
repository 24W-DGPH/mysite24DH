
# Load packages -------------------------------------------------------------------
pacman ::p_load(
  rio,           # import/export of many data  
  here,          # file paths relative to R project root folder
  tidyverse,     # data wrangling and presentation
  lubridate,     # to work with dates
  janitor,       # tables and data cleaning
  gtsummary,     # making descriptive and statistical tables
  linelist,      # cleaning line  lists
  naniar,        # assessing missing data
  skimr,         # preview data frames 
  matchmaker,    # dictionary-based cleaning
  epikit,        # age_categories() function
  dplyr,
  magrittr,
  stringr,       # for string searches, can be used in "rolling-up" values
  styler,        #source code formatting
  lintr,         #detects bad code patterns
  renv,          # to manage versions of packages when working in collaborative groups
  ggthemes,      # For additional themes
  patchwork,     # For combining plots
  plotly         #
) 

#setting the working directory
setwd("C:/Users/User/OneDrive/Documents/mysite24DH")

# Import Data ---------------------------------------------------------------------
linelist_raw <- import("cvd_disease_data.csv", setclass = "tibble")
skimr::skim(linelist_raw)

# Cleaning and Tidying up ---------------------------------------------------------
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(mi_risk       = heart_attack_risk)



library(dplyr)
library(tidyr)

# Remove unnecessary columns
linelist_col <- linelist %>%
  select(country,
         age,
         bmi,
         everything()) %>%
  select(-c(medication_use, 
            physical_activity_days_per_week,
            continent,
            hemisphere,
            patient_id)) %>%
  
  # Separate the blood_pressure column
  separate(blood_pressure, into = c("systolic_bp", "diastolic_bp"), sep = "/") %>%
  
  # Convert to numeric
  mutate(
    systolic_bp = as.numeric(systolic_bp),
    diastolic_bp = as.numeric(diastolic_bp),
    
    # Round up values to 2 decimal places
    exercise_hours_per_week = round(exercise_hours_per_week, 2),   
    sedentary_hours_per_day = round(sedentary_hours_per_day, 2),
    bmi = round(bmi, 2)
  ) 

# convert sedentary hours and sleep hours per day to week for comparison 
linelist_col$sedentary_hours_per_day <- linelist_col$sedentary_hours_per_day * 7
linelist_col$sleep_hours_per_day <- linelist_col$sleep_hours_per_day * 7

# rename sedentary hours column
linelist_col <- linelist_col %>%
  rename(sedentary_hours_per_week = sedentary_hours_per_day)

linelist_col <- linelist_col %>%
  rename(sleep_hours_per_week = sleep_hours_per_day)






# Creating frequency tables -------------

# Cholesterol

# Categorize cholesterol levels
linelist_col <- linelist_col %>%
  mutate(cholesterol_category = case_when(
    cholesterol > 240  ~ "Normal",
    cholesterol >= 200 & cholesterol <= 239 ~ "At Risk",
    cholesterol < 200  ~ "Heart Healthy",
    TRUE ~ NA_character_  # Handle missing values
  ))

# Create a frequency table
cholesterol_freq <- table(linelist_col$cholesterol_category)

# Print the table
print(cholesterol_freq)


# Diet frequency table

linelist_col %>%
  count(diet) %>%
  arrange(desc(n))  # Arrange in descending order







# Shiny application --------------------------------------------------------------------------
# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = textOutput("page_title")),  # Dynamic title
  dashboardSidebar(
    selectInput("selected_country", "Select Country:",
                choices = unique(linelist_col$country),
                selected = unique(linelist_col$country)[1],
                multiple = FALSE)  # Only one country can be selected
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("risk_percent", width = 3),
      box(title = "Health Statistics", width = 4, verbatimTextOutput("health_stats")),
      box(title = "Income Statistics", width = 4, tableOutput("income_summary"))
    ),
    fluidRow(
      box(title = "BMI Distribution by Sex", width = 6, plotlyOutput("bmi_plot")),
      box(title = "Age Distribution by Sex", width = 6, plotlyOutput("age_dist"))
    ),
    fluidRow(
      box(title = "Cholesterol Frequency", width = 5, plotlyOutput("cholesterol_hist")),
      box(title = "Physical Activity and Sleep vs Age", width = 7, plotlyOutput("scatter_plot"))
    ),
    fluidRow(
      box(title = "Population with Previous CVD", width = 5,
          plotlyOutput("heart_problems_pie"))
    )
    
  )
)

# Define Server
server <- function(input, output) {
  # Reactive dataset filtered by country
  filtered_data <- reactive({
    linelist_col %>% filter(country == input$selected_country)
  })
  
  # Dynamic Page Title
  output$page_title <- renderText({
    paste("Health Dashboard -", input$selected_country)
  })
  
  # Percentage of patients at risk
  output$risk_percent <- renderValueBox({
    risk_percentage <- mean(filtered_data()$mi_risk, na.rm = TRUE) * 100
    valueBox(paste0(round(risk_percentage, 1), "%"), "Patients at Risk", color = "red")
  })
  
  # Health Statistics
  output$health_stats <- renderText({
    data <- filtered_data()
    alcohol_perc <- mean(data$alcohol_consumption, na.rm = TRUE) * 100
    smoking_perc <- mean(data$smoking, na.rm = TRUE) * 100
    obesity_perc <- mean(data$obesity, na.rm = TRUE) * 100
    
    paste(
      "Alcohol Consumption:", round(alcohol_perc, 1), "%\n",
      "Smoking:", round(smoking_perc, 1), "%\n",
      "Obesity:", round(obesity_perc, 1), "%"
    )
  })
  
  # Cholesterol Histogram
  output$cholesterol_hist <- renderPlotly({
    data <- filtered_data() %>%
      mutate(cholesterol_category = factor(case_when(
        cholesterol > 240  ~ "Dangerous",
        cholesterol >= 200 & cholesterol <= 239 ~ "At Risk",
        cholesterol < 200  ~ "Heart Healthy"
      ), levels = c("Dangerous", "At Risk", "Heart Healthy"))) %>%  # Set factor levels
      drop_na(cholesterol_category)  # Remove missing values
    
    if (nrow(data) == 0) return(NULL)
    
    cholesterol_data <- table(data$cholesterol_category)
    
    plot_ly(
      x = factor(names(cholesterol_data), levels = c("Dangerous", "At Risk", "Heart Healthy")),  # Ensure order
      y = as.numeric(cholesterol_data),
      type = "bar",
      marker = list(color = c("#ff5757", "#ff9757", "lightgreen"))
    ) %>%
      layout(
        title = "Cholesterol Level of the Population",
        xaxis = list(title = "Cholesterol Levels"),
        yaxis = list(title = "Frequency"),
        showlegend = FALSE
      )
  })
  
  
  
  # BMI Distribution by Sex
  output$bmi_plot <- renderPlotly({
    ggplotly(
      ggplot(filtered_data(), aes(x = bmi, fill = sex)) +
        geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
        geom_vline(xintercept = c(18.5, 25, 30), linetype = "dashed") +
        labs( x = "BMI", y = "Density") +
        theme_minimal()
    )
  })
  
  # Age Distribution by Sex
  output$age_dist <- renderPlotly({
    ggplotly(
      ggplot(filtered_data(), aes(x = age, fill = sex)) +
        geom_histogram(bins = 10, alpha = 0.7, position = "identity") +
        labs( x = "Age", y = "Count") +
        theme_minimal()
    )
  })
  
  # Income Summary Table
  output$income_summary <- renderTable({
    data <- filtered_data()
    data.frame(
      Statistic = c("Mean", "Min", "Max"),
      Income = c(mean(data$income, na.rm = TRUE),
                 min(data$income, na.rm = TRUE),
                 max(data$income, na.rm = TRUE))
    )
  })
  
  
  # Scatter Plot (Exercise, Sedentary & Sleep Hours vs. Age)
  output$scatter_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = age)) +
      geom_point(aes(y = exercise_hours_per_week, color = "Exercise"), alpha = 0.7) +
      geom_point(aes(y = sedentary_hours_per_week, color = "Sedentary"), alpha = 0.7) +
      geom_point(aes(y = sleep_hours_per_week, color = "Sleep"), alpha = 0.7) +  # Added sleep hours
      scale_color_manual(values = c("Exercise" = "lightblue", "Sedentary" = "pink", "Sleep" = "purple")) +  
      labs(
        x = "Age",
        y = "Hours per Week",
        color = "Activity Type") +  
      theme_minimal()
    
    ggplotly(p) %>% layout(legend = list(title = list(text = "Activity Type")))
  })
  
  
  # Previous Heart Problems Pie Chart
  output$heart_problems_pie <- renderPlotly({
    data <- filtered_data()
    
    # Create a summary table
    heart_problem_counts <- table(data$previous_heart_problems)
    
    # Ensure there is data before plotting
    if (length(heart_problem_counts) == 0) {
      return(NULL)
    }
    
    # Convert to a dataframe for plotting
    heart_df <- as.data.frame(heart_problem_counts)
    colnames(heart_df) <- c("Heart Problem", "Count")
    
    # Label mapping for clarity
    heart_df$`Heart Problem` <- factor(heart_df$`Heart Problem`,
                                       levels = c(0, 1),
                                       labels = c("No Heart Problem", "Had Heart Problem"))
    
    # Create Pie Chart
    plot_ly(
      heart_df,
      labels = ~`Heart Problem`,
      values = ~Count,
      type = "pie",
      textinfo = "label+percent",
      insidetextfont = list(color = "#FFFFFF"),
      marker = list(colors = c("#e886f7", "#8ff783")) 
    ) %>%
      layout()
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
