

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
  rename(mi_risk       = heart_attack_risk )



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


# Descriptive statistics and distribution ---------------------------------------------------------

# Basic summary statistics
summary_stats <- linelist_col %>%
  summarise(across(where(is.numeric), 
                   list(mean = ~mean(., na.rm = TRUE),
                        sd = ~sd(., na.rm = TRUE),
                        median = ~median(., na.rm = TRUE),
                        q1 = ~quantile(., 0.25, na.rm = TRUE),
                        q3 = ~quantile(., 0.75, na.rm = TRUE)
                   )))

# BMI distribution

# Load the required libraries
library(tidyverse)
library(ggthemes)
library(viridis)

ggplot(linelist_col, aes(x = bmi)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50,
                 fill = "steelblue",
                 alpha = 0.7,
                 color = "white") +
  
  geom_density(color = "#FF5733",
               linewidth = 1.2,
               alpha = 0.2) +
  
  # Add WHO classification lines
  geom_vline(xintercept = c(18.5, 25, 30),
             color = "#2C3E50",
             linetype = "dashed",
             linewidth = 0.8) +
  
  # Modified way to add labels
  geom_text(data = data.frame(
    x = c(16.5, 21.75, 27.5, 33),
    y = rep(0.1, 4),  # You might need to adjust this value
    label = c("Underweight", "Normal", "Overweight", "Obese")
  ),
  aes(x = x, y = y, label = label),
  color = "#2C3E50",
  fontface = "bold",
  size = 4) +
  
  # Facet by sex
  facet_wrap(~sex, scales = "free_y") +
  
  # Theme and labels remain the same
  labs(title = "Distribution of BMI by Sex",
       subtitle = "With WHO Classification Guidelines",
       x = "Body Mass Index (BMI)",
       y = "Density",
       caption = "Dashed lines represent WHO BMI classification boundaries") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#666666", hjust = 0.5),
    plot.caption = element_text(size = 10, color = "#666666"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    strip.text = element_text(size = 12, face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
# Create a table of people with cardiovascular risk in different countries

# Create frequency table of mi_risk by country
mi_risk_freq <- linelist_col %>%
  group_by(country, mi_risk) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

# Print the table
print(mi_risk_freq)


# Logistic Regression Analysis -----
# First, prepare the data by standardizing continuous variables
model_data <- linelist_col %>%
  mutate(across(c(cholesterol, systolic_bp, heart_rate, 
                  exercise_hours_per_week, sleep_hours_per_day),
                ~scale(.) %>% as.vector))

# Fit the logistic regression model
risk_model <- glm(mi_risk ~ cholesterol + systolic_bp + heart_rate + 
                    family_history + smoking + obesity + alcohol_consumption +
                    exercise_hours_per_week + sleep_hours_per_day,
                  family = binomial(link = "logit"),
                  data = model_data)

# Create a summary table of results with odds ratios
summary_table <- broom::tidy(risk_model) %>%
  mutate(odds_ratio = exp(estimate),
         conf_low = exp(estimate - 1.96 * std.error),
         conf_high = exp(estimate + 1.96 * std.error))


# Create frequency table of mi_risk by country----
mi_risk_freq <- linelist_col %>%
  group_by(country, mi_risk) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

# Shiny application --------------------------

# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(igraph)
library(broom)
library(DT)

# Create the Shiny application
shinyApp(
  # UI Definition ----
  ui = dashboardPage(
    # Create the header with a clear title
    dashboardHeader(title = "Cardiovascular Risk Analysis"),
    
    # Sidebar definition with filters and navigation
    dashboardSidebar(
      # Create the main navigation menu
      sidebarMenu(
        menuItem("Overview", tabName = "overview", 
                 icon = icon("dashboard")),
        menuItem("BMI Analysis", tabName = "bmi", 
                 icon = icon("weight")),
        menuItem("Physical Activity", tabName = "phyact", 
                 icon = icon("dumbbell")),
        menuItem("Exploring risk factors", tabName = "rf", 
                 icon = icon("asterisk"))
      ),
      
      # Add interactive filters that will affect all visualizations
      # These help users explore specific subsets of the data
      selectInput("sex", "Select Sex:",
                  choices = c("All", "Male", "Female"),
                  selected = "All"),
      
      # Age range slider with reasonable defaults
      sliderInput("age_range", "Age Range:",
                  min = 0, max = 100,
                  value = c(20, 80)),
      
      # Risk factor selection using checkboxes
      checkboxGroupInput("risk_factors", "Risk Factors:",
                         choices = c("Smoking", "Diabetes", "Obesity"),
                         selected = "Obesity")
    ),
    
    # Main content area with different tabs
    dashboardBody(
      tabItems(
        # Overview Tab Content ----
        tabItem(tabName = "overview",
                fluidRow(
                  # Summary statistics displayed as value boxes
                  valueBoxOutput("total_patients"),
                  valueBoxOutput("avg_age"),
                  valueBoxOutput("high_risk_percent")
                ),
                fluidRow(
                  # Main visualizations for overview
                  box(plotlyOutput("age_dist"), 
                      title = "Age Distribution",
                      width = 6),
                  box(plotlyOutput("mi_risk_hist"),
                      title = "Risk of Heart Attack in Different Countries", 
                      width = 6)
                )
        ),
        
        # BMI Analysis Tab Content ----
        tabItem(tabName = "bmi",
                fluidRow(
                  # BMI distribution plot with statistics
                  box(plotlyOutput("bmi_hist"),
                      title = "BMI Distribution",
                      width = 8),
                  box(
                    title = "BMI Statistics",
                    width = 4,
                    tableOutput("bmi_stats")
                  )
                ),
                fluidRow(
                  # Additional BMI analysis
                  box(plotlyOutput("bmi_by_age"),
                      title = "BMI by Age",
                      width = 12)
                )
        ),
        
        # Physical Activity Tab Content ----
        tabItem(tabName = "phyact",
                fluidRow(
                  # Sedentary hours and BMI scatterplot
                  box(plotlyOutput("sedentary_bmi_scatter"),
                      title = "Sedentary hours per day vs. BMI",
                      width = 8)
                ),
                fluidRow(
                  # Additional BMI analysis
                  box(plotlyOutput("bmi_by_age"),
                      title = "BMI by Age",
                      width = 12)
                )
        ),
        
        # Exploring risk factors Tab Content ----
        tabItem(tabName = "rf",
                fluidRow(
                  # Forrest plot of risk factors of CVD
                  box(plotlyOutput("sedentary_bmi_scatter"),
                      title = "Sedentary hours per day vs. BMI",
                      width = 8)
                ),
                fluidRow(
                  # Additional BMI analysis
                  box(plotlyOutput("bmi_by_age"),
                      title = "BMI by Age",
                      width = 12)
                )
        
      )
    )
  ),
  
  # Server Logic ----
  server = function(input, output, session) {
    # Create reactive filtered dataset
    # This updates whenever the user changes any filter
    filtered_data <- reactive({
      # Start with the complete dataset
      data <- linelist_col
      
      # Apply sex filter if not "All"
      if (input$sex != "All") {
        data <- data %>% filter(sex == input$sex)
      }
      
      # Apply age range filter
      data <- data %>% 
        filter(age >= input$age_range[1],
               age <= input$age_range[2])
      
      # Apply risk factor filters
      if ("Smoking" %in% input$risk_factors) {
        data <- data %>% filter(smoking == TRUE)
      }
      if ("Diabetes" %in% input$risk_factors) {
        data <- data %>% filter(diabetes == TRUE)
      }
      if ("Obesity" %in% input$risk_factors) {
        data <- data %>% filter(bmi >= 30)
      }
      
      return(data)
    })
    
    # Render value boxes for overview tab
    output$total_patients <- renderValueBox({
      valueBox(
        nrow(filtered_data()),
        "Total Patients",
        icon = icon("users"),
        color = "blue"
      )
    })
    
    output$avg_age <- renderValueBox({
      valueBox(
        round(mean(filtered_data()$age), 1),
        "Average Age",
        icon = icon("calendar"),
        color = "green"
      )
    })
    
    output$high_risk_percent <- renderValueBox({
      valueBox(
        paste0(round(mean(filtered_data()$mi_risk) * 100, 1), "%"),
        "High Risk Patients",
        icon = icon("heart"),
        color = "red"
      )
    })
    
    # Render BMI histogram with WHO classification lines
    output$bmi_hist <- renderPlotly({
      plot_ly(filtered_data(), x = ~bmi, type = "histogram",
              nbinsx = 50,
              marker = list(color = "steelblue",
                            line = list(color = "white", width = 1))) %>%
        layout(title = "BMI Distribution",
               xaxis = list(title = "Body Mass Index"),
               yaxis = list(title = "Count"),
               # Add WHO classification lines
               shapes = list(
                 list(type = "line", x0 = 18.5, x1 = 18.5,
                      y0 = 0, y1 = 1000, 
                      line = list(color = "red", dash = "dash")),
                 list(type = "line", x0 = 25, x1 = 25,
                      y0 = 0, y1 = 1000, 
                      line = list(color = "red", dash = "dash")),
                 list(type = "line", x0 = 30, x1 = 30,
                      y0 = 0, y1 = 1000, 
                      line = list(color = "red", dash = "dash"))
               ))
    })
    
    # Render BMI statistics table
    output$bmi_stats <- renderTable({
      filtered_data() %>%
        summarise(
          "Average BMI" = mean(bmi),
          "Median BMI" = median(bmi),
          "Std Dev" = sd(bmi),
          "% Obese" = mean(bmi >= 30) * 100
        ) %>%
        round(2)
    })
    
    # Render histogram of the frequency table (Risk of Heart Attack in Different Countries)
    output$mi_risk_hist <- renderPlotly({
      plot_ly(mi_risk_freq, x = ~country, y = ~frequency, type = 'bar',
              marker = list(color = 'steelblue', line = list(color = 'white', width = 1))) %>%
        layout(title = "Risk of Heart Attack in Different Countries",
               xaxis = list(title = "Country"),
               yaxis = list(title = "Frequency"))
    })
    
    
    # Render age distribution plot
    output$age_dist <- renderPlotly({
      plot_ly(filtered_data(), x = ~age, type = "histogram",
              nbinsx = 30,
              color = ~sex) %>%
        layout(title = "Age Distribution by Sex",
               xaxis = list(title = "Age"),
               yaxis = list(title = "Count"),
               barmode = "overlay")
    })
    
    # Render BMI by age scatter plot
    output$bmi_by_age <- renderPlotly({
      plot_ly(filtered_data(), 
              x = ~age, 
              y = ~bmi,
              color = ~sex,
              type = "scatter",
              mode = "markers") %>%
        layout(title = "BMI vs Age",
               xaxis = list(title = "Age"),
               yaxis = list(title = "BMI"))
    })
    
    # Render scatter plot of Sedentary Hours vs BMI
    output$sedentary_bmi_scatter <- renderPlotly({
      plot_ly(filtered_data(), 
              x = ~sedentary_hours_per_day, 
              y = ~bmi,
              type = "scatter",
              mode = "markers",
              marker = list(color = 'blue', size = 6, opacity = 0.7)) %>%
        layout(title = "Sedentary Hours per Day vs BMI",
               xaxis = list(title = "Sedentary Hours per Day"),
               yaxis = list(title = "BMI"))
    })
  }
)
