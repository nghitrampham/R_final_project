# This function is used to help cleaning the salary data
# the function takes in the a data frame of raw salary data
# and return clean salary data in .csv format
# @param file_name, a data frame of raw salary data
# @return file_name, .csv files of clean salary data

clean_salary_helpers <- function(file_name){
 # remove the one that does not satisfy the position's requirement   
    if (length(removed) >= 1 && removed != 0) {
        file_name = file_name[-removed,]
    }
    # changing colunms names
    colnames(file_name)[1] <- "Rank"
    colnames(file_name)[2] <- "Player"
    colnames(file_name)[3] <- "Salary"
    # changing the class of columns 
    file_name[, 1] <- as.numeric(file_name[, 1])
    file_name[, 3] <-
        sapply(str_split(file_name[, 3], "[$]"), "[[", 2)
    file_name[, 3] <- gsub(",", "", file_name[, 3])
    file_name[, 3] <- as.numeric(file_name[, 3])
    return(file_name)
}