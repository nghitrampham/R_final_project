# This file generates all of our EDA analysis. We import functions from 2
# files: generate-statistics.R and generate-plots.R, each generates one half
# of the EDA output.

# Set current working directory to the one containing download-data-script.R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Source helper functions
source("../functions/generate-statistics.R") #Brandon's
source("../functions/generate-plots.R") #Trams's

# List of fields to treat as pure text fields
text_fields <- c("Player",
                 "Team",
                 "Position",
                 "Country",
                 "College")
text_fields2 <- text_fields[-1]
# Get the full player table

t_location <- "../../data/cleandata/roster-salary-stats.csv"

full_player_table <- read.csv(t_location,
                              stringsAsFactors = FALSE)

full_player_table$Birth_Date <-
    as.Date(full_player_table$Birth_Date)

# Generate sink()'d data
result <- create_summary_file(full_player_table, text_fields)
if (!result) {
    print("Failure in generating summary statistic file.")
}

# Generate Plots
bar_graphs <- create_plot_graphs(full_player_table, text_fields2)
hist_graphs <- create_box_histogram(full_player_table, text_fields)
