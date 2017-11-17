# This script (contains 2 main functions)is used to help
# generating graph histograms and boxplots
# for quantitative variables (e.g. salary, games played, free throws, etc)
# and graph barcharts of their frequencies for qualitative variables
# (e.g. position, team)

library(ggplot2)
library(stringr)
library(dplyr)

# This function is used to help producing bar chart for qualitative
# variables
# @param data, big data frame that contains all information of all teams
# @param text_fields, character vector that contains column names of
#                qualitative varibales.
# @return NULL 

create_plot_graphs <- function(data, text_fields) {
    # checking if the inputs are in correct formats
    if (class(full_player_table) != "data.frame" ||
        class(text_fields) != "character") {
        stop("Please check input types to 'create_summary_file'.")
    }
    
    # In qualitative variables, NA is converted to character string
    # and is considered as a posibility
    col_number = c(1, 2, 4, 8, 10)
    for (idx in col_number) {
        data[, idx][which(is.na(data[, idx]))] <-  "NA"
    }
    
    # use switch to generate aes in ggplot according to each variable
    for (field in text_fields) {
        switch(
            field,
            Player = {
                aes = aes(x = Player)
            },
            Team = {
                aes = aes(x = Team)
            },
            Position = {
                aes = aes(x = Position)
            },
            Country = {
                aes = aes(x = Country)
            }
        )
        
        # handling the special case for college, we will only plot those
        if (field == "College") {
            player_data_copy = data
        # calculating the frequency for each college 
            freq <- data %>%
                dplyr::select_(field) %>%
                dplyr::group_by_(field) %>%
                dplyr::count() %>%
                dplyr::arrange()
        # only consider the college that has frequency above the average
            freq = as.data.frame(freq)
            idx = which(freq[, 2] <= mean(freq$n))
            freq = freq[-c(idx),]
            player_data_copy = data[which(data$College %in% freq$College),]
        # abbreviate the college's name 
            player_data_copy$College = abbreviate(
                player_data_copy$College,
                minlength = 4,
                use.classes = TRUE,
                dot = FALSE,
                strict = FALSE,
                method = c("left.kept")
            )
            # making the name of college short enough to plot 
            for (i in 1:length(player_data_copy$College)) {
                if (nchar(player_data_copy$College[i]) >= 10) {
                    temp = lapply(str_split(player_data_copy$College[i],
                                            "f"),
                                  "[[",
                                  2)
                    player_data_copy$College[i] = paste0("Uo", temp)
                } else {
                    player_data_copy$College[i] = 
                        player_data_copy$College[i]
                }
                
            }
            # generating the plot (bar chart) of qualitative variables
            p <- ggplot(player_data_copy, aes(x = College))
        } else {
            p <- ggplot(data, aes)
            
        }
        
        
        # label the y axis
        p <- p + ylab("Frequency")
        # change the color of the bar
        p <- p + geom_bar(fill = "#00BFC4", colour = "black")
        # modify the title and label of x and y axis in terms of font,
        #  size, etc
        p <- p + theme(axis.text.x = element_text(face = "bold",
                                                  size = rel(1.1)))
        p <- p + theme(axis.text.y = element_text(face = "bold",
                                                  size = rel(1.1)))
        p <- p + theme(axis.title.x = element_text(size = 12,
                                                   face = "bold"))
        p <-
            p + theme(axis.title.y = element_text(size = 12, face = "bold"))
        p <- p + ggtitle(paste("Frequency of", noquote(field))) +
            theme(plot.title = element_text(size = rel(1.2), face = "bold"))
        
        # save plot in png format to file images
        png(
            filename = paste0("../../images/bar-", noquote(field), ".png"),
            width = 800,
            height = 500
        )
        plot(p)
        dev.off()
    }
    
}

# generate the quantitative variable
# This function is used to help producing bar chart for qualitative
# variables
# @param data, big data frame that contains all information of all teams
# @param text_fields, character vector that contains column names of
#                qualitative varibales.
# @return NULL
create_box_histogram <- function(player_data, text_fields) {
    
    # checking inputs
    if (class(full_player_table) != "data.frame" ||
        class(text_fields) != "character") {
        stop("Please check input types to 'create_summary_file'.")
    }
    # modify the Birth_Date columns
    player_data$Birth_Date = sapply(str_split(player_data$Birth_Date, "-"),
                                    "[[",
                                    1)
    # convert class of Birth_Date from Date to numeric 
    player_data$Birth_Date = as.numeric(player_data$Birth_Date)

    number_cols <-
        names(player_data[, !names(player_data) %in% text_fields])

    # loop through all quantitative variable in roster-salary-stats 
    # data frame
    for (field in number_cols) {
        print(field)
        freq <- player_data %>%
            dplyr::select_(field) %>%
            dplyr::group_by_(field) %>%
            dplyr::count() %>%
            dplyr::arrange()
        
        freq = as.data.frame(freq)
        freq = freq[-nrow(freq), ]
        # boxplot for quantitative variables
        switch(
            field,
            Number = {
                aes = aes(x = Number , y = n)
                aes1 = aes(x = Number)
                
            },
            Weight = {
                aes = aes(x = Weight , y = n)
                aes1 = aes(x = Weight)
                
            },
            Experience = {
                aes = aes(x = Experience , y = n)
                aes1 = aes(x = Experience)
                
            },
            Age = {
                aes = aes(x = Age , y = n)
                aes1 = aes(x = Age)
            },
            Games_Started = {
                aes = aes(x = Games_Started , y = n)
                aes1 = aes(x = Games_Started)
                
            },
            Field_Goals =  {
                aes = aes(x = Field_Goals , y = n)
                aes1 = aes(x = Field_Goals)
                
            },
            Field_Goal_Percentage = {
                aes = aes(x = Field_Goal_Percentage , y = n)
                aes1 = aes(x = Field_Goal_Percentage)
                
            },
            X3.Point_Field_Goal_Attempts = {
                aes = aes(x = X3.Point_Field_Goal_Attempts , y = n)
                aes1 = aes(x = X3.Point_Field_Goal_Attempts)
                
            },
            X2.Point_Field_Goals = {
                aes = aes(x = X2.Point_Field_Goals , y = n)
                aes1 = aes(x = X2.Point_Field_Goals)
                
            },
            X2.Point_Field_Goal_Percentage = {
                aes = aes(x = X2.Point_Field_Goal_Percentage , y = n)
                aes1 = aes(x = X2.Point_Field_Goal_Percentage)
                
            },
            Free_Throws = {
                aes = aes(x = Free_Throws , y = n)
                aes1 = aes(x = Free_Throws)
                
            },
            Free_Throw_Percentage = {
                aes = aes(x = Free_Throw_Percentage , y = n)
                aes1 = aes(x = Free_Throw_Percentage)
                
            },
            Defensive_Rebounds = {
                aes = aes(x = Defensive_Rebounds , y = n)
                aes1 = aes(x = Defensive_Rebounds)
                
            },
            Assists = {
                aes = aes(x = Assists , y = n)
                aes1 = aes(x = Assists)
                
            },
            Blocks = {
                aes = aes(x = Blocks , y = n)
                aes1 = aes(x = Blocks)
                
            },
            Personal_Fouls = {
                aes = aes(x = Personal_Fouls , y = n)
                aes1 = aes(x = Personal_Fouls)
                
            },
            Rank_Salary = {
                aes = aes(x = Rank_Salary , y = n)
                aes1 = aes(x = Rank_Salary)
                
            },
            Height = {
                aes = aes(x = Height , y = n)
                aes1 = aes(x = Height)
                
            },
            Birth_Date = {
                aes = aes(x = Birth_Date , y = n)
                aes1 = aes(x = Birth_Date)
                
            },
            Rank_Totals = {
                aes = aes(x = Rank_Totals , y = n)
                aes1 = aes(x = Rank_Totals)
                
            },
            Games = {
                aes = aes(x = Games , y = n)
                aes1 = aes(x = Games)
                
            },
            Minutes_Played = {
                aes = aes(x = Minutes_Played , y = n)
                aes1 = aes(x = Minutes_Played)
                
            },
            Field_Goal_Attempts = {
                aes = aes(x = Field_Goal_Attempts , y = n)
                aes1 = aes(x = Field_Goal_Attempts)
                
            },
            X3.Point_Field_Goals = {
                aes = aes(x = X3.Point_Field_Goals , y = n)
                aes1 = aes(x = X3.Point_Field_Goals)
                
            },
            X3.Point_Field_Goal_Percentage = {
                aes = aes(x = X3.Point_Field_Goal_Percentage , y = n)
                aes1 = aes(x = X3.Point_Field_Goal_Percentage)
                
            },
            X2.point_Field_Goal_Attempts = {
                aes = aes(x = X2.point_Field_Goal_Attempts , y = n)
                aes1 = aes(x = X2.point_Field_Goal_Attempts)
                
            },
            Effective_Field_Goal_Percentage = {
                aes = aes(x = Effective_Field_Goal_Percentage , y = n)
                aes1 = aes(x = Effective_Field_Goal_Percentage)
                
            },
            Free_Throw_Attempts = {
                aes = aes(x = Free_Throw_Attempts , y = n)
                aes1 = aes(x = Free_Throw_Attempts)
                
            },
            Offensive_Rebounds = {
                aes = aes(x = Offensive_Rebounds , y = n)
                aes1 = aes(x = Offensive_Rebounds)
                
            },
            Total_Rebounds = {
                aes = aes(x = Total_Rebounds , y = n)
                aes1 = aes(x = Total_Rebounds)
                
            },
            Steals = {
                aes = aes(x = Steals , y = n)
                aes1 = aes(x = Steals)
                
            },
            Turnovers = {
                aes = aes(x = Turnovers , y = n)
                aes1 = aes(x = Turnovers)
                
            },
            Points = {
                aes = aes(x = Points , y = n)
                aes1 = aes(x = Points)
                
            },
            Salary = {
                aes = aes(x = Salary , y = n)
                aes1 = aes(x = Salary)
                
            }
        )
        
        # plotting box plot
        
        p <- ggplot(freq, aes) + geom_boxplot()
        p <- p + ylab("Frequency")
        p <- p + theme(axis.text.x = element_text(face = "bold",
                                                  size = rel(1.1)))
        p <- p + theme(axis.text.y = element_text(face = "bold",
                                                  size = rel(1.1)))
        p <-
            p + theme(axis.title.x = element_text(size = 12, face = "bold"))
        p <-
            p + theme(axis.title.y = element_text(size = 12, face = "bold"))
        p <- p + ggtitle(paste("Frequency of", noquote(field))) +
            theme(plot.title = element_text(size = rel(1.2), face = "bold"))
        # saving the boxplot
        png(
            filename = paste0(paste(
                "../../images/box-", noquote(field), sep = ""
            ), ".png"),
            width = 800,
            height = 500
        )
        plot(p)
        dev.off()
        
        
        # start plotting histogram
        p_his <- ggplot(player_data, aes1) +
            geom_histogram(fill = "#00BFC4",
                           colour = "black",
                           size = .2)
        p_his <- p_his + ylab("Frequency")
        p_his <-
            p_his + theme(axis.text.x = element_text(face = "bold",
                                                     size = rel(1.1)))
        p_his <-
            p_his + theme(axis.text.y = element_text(face = "bold",
                                                     size = rel(1.1)))
        p_his <-
            p_his + theme(axis.title.x = element_text(size = 12,
                                                      face = "bold"))
        p_his <-
            p_his + theme(axis.title.y = element_text(size = 12,
                                                      face = "bold"))
        p_his <-
            p_his + ggtitle(paste("Frequency of", noquote(field))) +
            theme(plot.title = element_text(size = rel(1.2), face = "bold"))
        
        # saving the histogram plots
        png(
            filename = paste0(paste(
                "../../images/hist-", noquote(field), sep = ""
            ), ".png"),
            width = 800,
            height = 500
        )
        # save plot in png format to file images
        plot(p_his)
        
        dev.off()
    }
    
}