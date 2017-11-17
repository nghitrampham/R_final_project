# Create the team salaries table of aggregated salary grouped by team
library(dplyr)
library(rstudioapi)

# Set working directory to the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Get the roster salary data
team_data <- read.csv(file = "../../data/cleandata/roster-salary-stats.csv"
                      , sep = ",")

# Get the total, minimum, maximum, first quartile, median, third quartile
# average, interquartile range, and standard deviation of salaries, grouped
# by team
salary_aggregates <- team_data %>% 
                     group_by(Team) %>% 
                     select(Salary, Team) %>% 
  summarise(total = sum(Salary, na.rm = TRUE), 
            minimum = min(Salary, na.rm = TRUE),
            maximum = max(Salary, na.rm = TRUE), 
            first_quartile = quantile(Salary, .25, na.rm = TRUE), 
            median = median(Salary, na.rm = TRUE), 
            third_quartile = quantile(Salary, .75, na.rm = TRUE), 
            average = mean(Salary, na.rm = TRUE),
            interquartile_range = IQR(Salary, na.rm = TRUE), 
            standard_deviation = sd(Salary, na.rm = TRUE))

# Write out the aggregated data to the team-salaries.csv
write.csv(salary_aggregates, file = "../../data/cleandata/team-salaries.csv", 
          row.names = FALSE)
