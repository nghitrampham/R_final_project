# UI for the Stat Salary Shiny App
library(shiny)

# Set working directory to the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Get the team-salaries table
salary_data = read.csv(file = "../../data/cleandata/eff-stats-salary.csv")
# Merge with the roster-salary-stats table so we have access to the team 
# and position variables
total_data = read.csv(file = "../../data/cleandata/roster-salary-stats.csv")
merge_data = merge(salary_data, total_data)

fluidPage(
  # Title of page
  headerPanel('Statistic Comparison for Players'),
  sidebarPanel(
    # X Variable dropdown, choosing only quantitative variables
    selectInput(
        'x_var', 
        'X Variable', 
        names(merge_data)[c(2:9, 11:13, 17, 18, 21, 23:29, 36:43)], 
        selected = names(merge_data)[13]),
    # Y Variable dropdown, choosing only quantitative variables
    selectInput(
        'y_var', 
        'Y Variable', 
        names(merge_data)[c(2:9, 11:13, 17, 18, 21, 23:29, 36:43)], 
        selected = names(merge_data)[9]),
    # Radio button to choose to color by team or position
    radioButtons("color_by", label = "Color", 
                 choices = c("Team" = "team", "Position" = "position"),
                 selected = "team"),
    # Checkbox to choose to display regression line
    checkboxInput("regression_by", 
                  label = "Show Regression Line", 
                  value = FALSE),
    textOutput("correlation")
  ),
  # Display the plot
  mainPanel(
    plotOutput("plot1")
  )
)