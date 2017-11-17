# This script caluculates eff of each player using PCA and writes a new data 
# table, "eff-stats-salary.csv". This csv file includes variables, Player,
# Points, Total_Rebounds, Assists, Steals, Blocks, Missed_Field_Goals,
# Missed_Free_Throws, Turnovers, Games, EFF, and Salary.

library(dplyr)

# Load data file, "roster-salary-stats.csv".
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
team_data <- read.csv(file = "../../data/cleandata/roster-salary-stats.csv",
                      sep = ",")

# Statistics for efficiency
stats <- c('Points',
           'Total_Rebounds',
           'Assists',
           'Steals',
           'Blocks',
           'Missed_Field_Goals',
           'Missed_Free_Throws',
           'Turnovers')

#Use for loops to subset the data according to positions
team_data_modified = NULL
for (position in c('C', 'PF', 'PG', 'SF', 'SG')) {
  
  # Subset data according to players' positions
  # and add columns, "Missed_Field_Goals", "Missed_Free_Throws", and
  # "Turnovers" (these variables have negative sign)
  position_data = team_data %>%
    filter(Position == position) %>%
    mutate(Missed_Free_Throws = Free_Throws - Free_Throw_Attempts) %>%
    mutate(Missed_Field_Goals = Field_Goals - Field_Goal_Attempts) %>%
    mutate(Turnovers = -1 * Turnovers)
  
  # All variables divided by number of games
  data_by_game = as.matrix(position_data[ ,stats] / position_data$Games)
  
  # PCA with prcomp()
  pca <- prcomp(data_by_game, center = TRUE, scale. = TRUE)
  weights <- abs(pca$rotation[,1])
  
  # Std deviations for each columns
  sigmas <- apply(data_by_game, 2, sd)
  
  # Modified efficiency
  eff <- data_by_game %*% (weights / sigmas)
  position_data$EFF <- eff
  
  # Stack the subsets using "rbind()"
  team_data_modified = rbind(team_data_modified, position_data)
}

#==============================================================================
# Create data set "eff-stats-salary.csv" 
#==============================================================================

# Create a new table by selecting columns from team_data_modified
eff_stats_salary <- team_data_modified %>% 
  select (Player,
          Points,
          Total_Rebounds,
          Assists, Steals,
          Blocks,
          Missed_Field_Goals,
          Missed_Free_Throws,
          Turnovers,
          Games,
          EFF,
          Salary)

# Take off negative signs from columns, 
# "Missed Field Goals", "Missed Free Throws", and "Turnovers".
eff_stats_salary$Missed_Field_Goals = eff_stats_salary$Missed_Field_Goals * - 1
eff_stats_salary$Missed_Free_Throws = eff_stats_salary$Missed_Free_Throws * - 1
eff_stats_salary$Turnovers = eff_stats_salary$Turnovers * - 1

# Write data set, "eff-stats-salary.csv" 
write.csv(eff_stats_salary, file = "../../data/cleandata/eff-stats-salary.csv")
