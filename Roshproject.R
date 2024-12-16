
install.packages("pacman")

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
  
  renv,          # to manage versions of packages when working in collaborative groups
  
) 

# Import Data ---------------------------------------------------------------------
linelist_raw <- import("cvd_disease_data.csv")
skimr::skim(linelist_raw)

# Cleaning and Tidying up ---------------------------------------------------------
linelist <- linelist_raw %>%
janitor::clean_names()           #standardize column name syntax

# linelist dataset is piped through select command
linelist %>%
  select(age, sex, cholesterol, blood_pressure, diabetes, smoking, bmi,obesity, alcohol_consumption, exercise_hours_per_week, sleep_hours_per_day, heart_attack_risk)%>%
  names()                        # display column names


