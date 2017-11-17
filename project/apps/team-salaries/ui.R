# UI For the Team Salary Shiny app
library(shiny)

# Set working directory to the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Get the team-salaries table
salary_data = read.csv(file = "../../data/cleandata/team-salaries.csv")

fluidPage(
  # Title of the page
  headerPanel('Salary Statistics by Team'),
  sidebarPanel(
    # Create input dropdown for the variable to plot with Team
    selectInput('var', 
                'Variable', 
                names(salary_data)[2:10], 
                selected = names(salary_data)[2]),
    # Radio button to choose between ascending and descending orderings
    radioButtons("order", label = "Order", 
                 choices = c("Ascending" = "asc", "Descending" = "desc"),
                 selected = "asc")
  ),
  # Display the plot
  mainPanel(
    plotOutput('plot1')
  )
)