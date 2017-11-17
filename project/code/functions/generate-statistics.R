# This file contains the function that sink()'s the summary statistics into
# the text file 'eda-output.txt'

library(dplyr)

# This function takes in a data.frame with all player information and creates
# a file called 'eda-output.txt' with all summary statistics surrounding all
# variables.
# @param player_data, a data frame with all player data
# @param text_fields, a character vector with all text fields to compute
#                     frequencies for
# @return NULL
create_summary_file <- function(player_data, text_fields) {
    if (class(full_player_table) != "data.frame" ||
        class(text_fields) != "character") {
        stop("Please check input types to 'create_summary_file'.")
    }
    sink(file = "../../data/cleandata/eda-output.txt")
    options(max.print = 10000000)
    
    for (field in text_fields) {
        print(field)
        freq <- player_data %>%
                dplyr::select_(field) %>%
                dplyr::group_by_(field) %>%
                dplyr::count() %>%
                dplyr::arrange()
        print.data.frame(freq, row.names = FALSE)
    }
    
    number_cols <- names(player_data[, !names(player_data) %in% text_fields])
    for (field in number_cols) {
        print(field)
        stats_summary <- summary(player_data[, field])
        data_range <- unname(stats_summary["Max."] - stats_summary["Min."])
        names(data_range) <- "Range"
        if (field == "Birth.Date") {
            stats_summary <- as.character(stats_summary)
        }
        stats_summary <- append(stats_summary, data_range)
        print(stats_summary)
    }
    
    options(max.print = 99999)
    sink()
    return(TRUE)
}
