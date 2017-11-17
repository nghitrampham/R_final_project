# Server for the Team Salary Shiny App
library(shiny)
library(ggplot2)

# Set working directory to the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Get the team-salaries table
salary_data = read.csv(file = "../../data/cleandata/team-salaries.csv")

# Creates the graph objects that will be displayed in ui
# @param input, values chosen in the ui to be used in the creation of the
# graph
# @param output, variable to save the graph and correlation values to
# @return NULL
function(input, output) {
  
  # Get the order variable so we know how to order the bars
  selectedOrder <- reactive({
    input$order
  })
  
  # Create the plot
  plotObj <- reactive({
    
    # Create ascending plot
    if (selectedOrder() == "asc") {
      p = ggplot(
            salary_data, 
            aes(
                reorder(
                    x = Team, 
                    -salary_data[,input$var]), 
                y = salary_data[,input$var], 
                fill=Team)) +
        geom_bar(stat='identity') + coord_flip() +
        labs(
            x = "Team", 
            y = paste(input$var, "(US Dollars)"), 
            title = paste("Salary", input$var, "by Team"))
    } else {
      # Create descending plot
      p = ggplot(
            salary_data, 
            aes(
                reorder(
                    x = Team, 
                    salary_data[,input$var]), 
                y = salary_data[,input$var], 
                fill=Team)) +
        geom_bar(stat='identity') + coord_flip() +
        labs(
            x = "Team", 
            y = paste(input$var, "(US Dollars)"), 
            title = paste("Salary", input$var, "by Team"))
    }
  })
  
  
  # Output the plot
  output$plot1 <- renderPlot({
    print(plotObj())
  })
  
}