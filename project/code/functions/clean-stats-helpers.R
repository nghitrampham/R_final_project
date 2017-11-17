# # This function is used to help cleaning the stats data
# the function takes in the a data frame of raw stats data of each team
# and return clean stats data in .csv format
# @param file_name, a data frame of raw stats data
# @return, file_name, .csv files of clean stats data

clean_stats_helpers <- function(file_name) {
    
    # change column names
    col_names <- c(
        "Rank",
        "Player",
        "Age",
        "Games",
        "Games_Started",
        "Minutes_Played",
        "Field_Goals",
        "Field_Goal_Attempts",
        "Field_Goal_Percentage",
        "3-Point_Field_Goals",
        "3-Point_Field_Goal_Attempts",
        "3-Point_Field_Goal_Percentage",
        "2-Point_Field_Goals",
        "2-point_Field_Goal_Attempts",
        "2-Point_Field_Goal_Percentage",
        "Effective_Field_Goal_Percentage",
        "Free_Throws",
        "Free_Throw_Attempts",
        "Free_Throw_Percentage",
        "Offensive_Rebounds",
        "Defensive_Rebounds",
        "Total_Rebounds",
        "Assists",
        "Steals",
        "Blocks",
        "Turnovers",
        "Personal_Fouls",
        "Points"
    )
    colnames(file_name) <- col_names
    
    #remove the one whose position is not one of the 5 required positions
    if (length(removed) >= 1 &&  removed != 0) {
        file_name = file_name[-removed, ]
    }
    
    # convert the first column to numeric 
    file_name[, 1] <- as.numeric(file_name[, 1])
    
    for (i in 3:ncol(file_name)) {
        index = which(is.na(file_name[, i]))
        if (length(index) >= 1) {
            file_name[, i][which(is.na(file_name[, i]))] <-  NA
        }
        file_name[, i] = as.numeric(file_name[, i])
    }

    
    return(file_name)
}