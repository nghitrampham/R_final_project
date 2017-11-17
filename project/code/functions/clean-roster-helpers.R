# This function is used to help cleaning the roster data
# the function takes in the a data frame of raw roster data
# and return clean roster data in .csv format
# @param file_name, a data frame of raw roster data
# @return file_name, .csv files of clean roster data

clean_roster_helper <- function(file_name) {
    
    # change the variable name/col name
    colnames(file_name)[7] <- "roster.Country"
    colnames(file_name)[6] <- "roster.Birth_Date"
    # clean the name of each columns, the original format is 
    # roster.name, we will only get the name
    col_names <-
        sapply(str_split(colnames(file_name), "[.]"), "[[", 2)
    colnames(file_name) <- col_names
    # changing the col names
    index = c(1, 3, 4, 5, 8)
    name = c("Number", "Position", "Height", "Weight", "Experience")
    for (i in 1:length(index)) {
        names(file_name)[index[i]] <- paste(name[i])
    }
    
    # remove position
    position_ls <- c("C", "PF", "SF", "SG", "PG")
    removed =  which(!file_name$Position %in% position_ls)
    if (length(removed) >= 1) {
        file_name = file_name[-removed, ]
    }
    file_name$Position = factor(file_name$Position)
    
    # change Number to numeric
    file_name[, 1] <- as.numeric(file_name[, 1])
    
    # Clean Country column ( change to upper case)
    file_name[, 7] <- toupper(file_name[, 7])
    
    # Clean the Experience Column
    file_name[, 8][which(file_name$Experience == "R")] <- "0"
    file_name[, 8] <- as.numeric(file_name[, 8])
    
    # Clean the Height Column
    ft = sapply(str_split(file_name$Height, "-"), "[[", 1)
    inches = sapply(str_split(file_name$Height, "-"), "[[", 2)
    file_name$Height = round(as.numeric(paste0(ft, ".", inches)) * 0.3048,
                             digits = 2)
    
    # Clean the weight column
    file_name[, 5] = round(as.numeric(file_name[, 5] * 0.453592),
                           digits = 2)
    # Clean the Birth Date column
    year <- sapply(str_split(file_name$"Birth_Date", ","), "[[", 2)
    temp <- sapply(str_split(file_name$"Birth_Date", ","), "[[", 1)
    month <- str_trim(str_extract(temp, "\\D+"))
    day <- str_trim(str_extract(temp, "\\d+"))
    
    # this function helps to convert the month in words to number, 
    # @ param x, month in words
    # @ month_new, months in number (numeric)
    
    num2Month <- function(x) {
        months <- c(
            january = 1,
            february = 2,
            march = 3,
            april = 4,
            may = 5,
            june = 6,
            july = 7,
            august = 8,
            september = 9,
            october = 10,
            november = 11,
            december = 12
        )
        x <- tolower(x)
        month_new = lapply(x, function(x)
            months[x])
        month_new = as.numeric(unlist(month_new))
    }
    
    month = as.character(num2Month(month))
    file_name$"Birth_Date" = as.Date(paste(year, month, day, sep = "-"),
                                     format = "%Y-%m-%d")
    
    # Clean the College Column
    file_name[, 9][which(file_name$College == "")] <- NA
    
    
    return(file_name)
}
