# This lean-data-script is used to clean all the raw data and merge 
# them in a big data frame called roster-salary-stats data frame. 
# It will return a clean roster-salary-stats in .csv format
library(stringr)
library(dplyr)
library(ggplot2)

# Set current working directory to the one containing 
# clean-data-scripts.R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# source for function helpers
source("../functions/clean-roster-helpers.R")
source("../functions/clean-stats-helpers.R")
source("../functions/clean-salary-helpers.R")

# changing working directory 
setwd("../../data/rawdata/roster-data")
folder <- getwd()
# get the path to read .csv file
file_list <-
    list.files(path = paste0(folder, "/"), pattern = "*.csv")
f_name = c()

# get the list of name for all teams
for (id in 1:length(file_list)) {
    name <- str_split(file_list[id], pattern = '')
    name <- unlist(name)
    name <- name[1:which(name == '.') - 1]
    f_name[id] <- paste0(name, collapse = '')
}

# initializing variables
roster_salary_stats = data.frame()
removed = 0

# loop through each team, it will clean roster, stats, salary for 
# each team and use merge() to merge them in one data frame, then loop 
# through the second team and use merge() again, then use cbind() to 
# combine the data frames of the first and second team, and the process 
# is repeated for all 30 teams
for (k in 1:length(file_list)) {
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    setwd("../../data/rawdata/roster-data")
    # setwd("../rawdata/roster-data")
    folder <- getwd()
    
    #read roster_file
    roster_file1 <- read.csv(
        paste(paste0(folder, "/"), file_list[k], sep = ''),
        as.is = TRUE,
        row.names = NULL,
        header = TRUE
    )
    
    # call function helper to clean the roster file
    roster_file <- clean_roster_helper(roster_file1)
    # add team column to roster file
    roster_file$Team <- rep(f_name[k], time = nrow(roster_file))
    
    
    # clean stats-data
    setwd("../stat-data")
    folder <- getwd()
    
    stat_file1 = read.csv(paste(paste0(folder, "/"), file_list[k], 
                          sep = ''),
                          as.is = TRUE)
    
    # call function helper to clean the stats file
    
    stat_file <- clean_stats_helpers(stat_file1)
    # add team colunm to stats file
    stat_file$Team <- rep(f_name[k], time = nrow(stat_file))
    
    
    # clean salary-data
    setwd("../salary-data")
    folder <- getwd()
    salary_file1 = read.csv(paste(paste0(folder, "/"), 
                            file_list[k], sep = ''),
                            as.is = TRUE)
    
    # call function helper to clean the salary file
    salary_file <- clean_salary_helpers(salary_file1)
    # add team column to salary file
    salary_file$Team <- rep(f_name[k], time = nrow(salary_file))
    
    # merging data frame
    temp = data.frame()
    temp = merge(
        x = roster_file,
        y = stat_file,
        by = c("Player", "Team"),
        all = TRUE
    )
    temp = merge(
        x = temp,
        y = salary_file,
        by = c("Player", "Team"),
        all = TRUE
    )
    roster_salary_stats = rbind(roster_salary_stats, temp)
    
}

colnames(roster_salary_stats)[11] <- "Rank_Totals"
colnames(roster_salary_stats)[38] <- "Rank_Salary"

# remove duplicated players
dup_filter = !duplicated(roster_salary_stats$Player)
roster_salary_stats = roster_salary_stats[dup_filter,]

# write the .csv file that contains all teams
write.csv(
    roster_salary_stats,
    file = paste0('../../cleandata/', "roster-salary-stats" , ".csv"),
    row.names = FALSE
)