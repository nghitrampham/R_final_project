# This file retrieves the raw data files from the specified source
# This file MUST be run in RStudio to work correctly
# Helper functions are imported from raw-data-helpers.R

# Set current working directory to the one containing download-data-script.R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Source helper functions
source("../functions/raw-data-helpers.R")

# Setup and execution of code to get player data

# Constants
url_base <- "http://www.basketball-reference.com"
url_get_teams <- "leagues/NBA_2016.html"
url_teams <- "teams"
url_file <- "2016.html"
dst_names <- c(roster = "roster-data",
               totals = "stat-data",
               salaries = "salary-data")

# Retrieves and stores player tables in csv files
# @param tables, the tables to fetch and save (subset of names(dst_names))
# @return NULL
create_player_csvs <- function(tables = names(dst_names)) {
  # Get team abbreviations
  team_retrieval <- paste(url_base, url_get_teams, sep = "/")
  teams <- get_team_names(team_retrieval)
  for (team in teams) {
    team_url <- paste(url_base, 
                      url_teams,
                      team, 
                      url_file, 
                      sep = "/")
    print(paste("accessing", team_url))
    html_lines <- readLines(con = team_url)
    for (table_name in tables) {
      # Which table are we on
      print(table_name)
      player_frame <- get_player_table(html_lines, table_name)
      
      # Where to write
      file_name <- paste0("../../data/rawdata/", 
                          unname(dst_names[table_name]), 
                          "/", 
                          team, 
                          ".csv")
      
      # Write to csv in rawdata directory
      write.csv(player_frame, file_name, row.names = FALSE)
    }
  }
}

# Get player data and store in csv files
create_player_csvs(names(dst_names))
