# Server for the Stat Salary shiny app
library(shiny)
library(ggplot2)

# Set working directory to the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Get the team-salaries table
salary_data = read.csv(file = "../../data/cleandata/eff-stats-salary.csv")
# Merge with the roster-salary-stats table so we have access to the team 
# and position variables
total_data = read.csv(file = "../../data/cleandata/roster-salary-stats.csv")
merge_data = merge(salary_data, total_data)

# Creates the graph and correlation objects that will be displayed in ui
# @param input, values chosen in the ui to be used in the creation of the
# graph
# @param output, variable to save the graph and correlation values to
# @return NULL
function(input, output) {
  
  # Get the color_by variable so we know to color by team or position
  selectedColor <- reactive({
    input$color_by
  })
  
  # Get the regression_by variable so we know if we should display the 
  # regression lines
  showRegression <- reactive({
    input$regression_by
  })
  
  # Generate the scatterplot
  plotObj <- reactive({
    
    # Generate plot colored by team
    if (selectedColor() == "team") {
      p = ggplot(merge_data, aes(x = merge_data[,input$x_var], 
                                 y = merge_data[,input$y_var], 
                                 colour = Team)) + geom_point()
        
    } else {
      # Generate plot by position
      p = ggplot(merge_data, aes(x = merge_data[,input$x_var], 
                                 y = merge_data[,input$y_var], 
                                 colour = Position)) + geom_point()
    }
    # Add regression line if box is checked
    if (showRegression() == TRUE) {
      p = p + geom_smooth(method = "lm", se = FALSE)
    }
    # Add labels to plot
    p = p + labs(x = input$x_var, y = input$y_var, 
                 title = paste(input$x_var, "vs", input$y_var))
  })
  
  # Render the plot
  output$plot1 <- renderPlot({
    print(plotObj())
  })
  
  # Render the correlation between the variables
  output$correlation <- renderText({
    # Correlation only valid if both variables are numeric
    if (is.numeric(merge_data[,input$x_var]) && is.numeric(merge_data[,input$y_var])) {
      paste("Correlation:", 
            cor(merge_data[,input$x_var], merge_data[,input$y_var], 
                use = 'pairwise.complete.obs'))
    }
  })
  
}