# This script lists out the 20 best and the 20 worst players in the NBA
# in terms of their Values, calculated by Value=(EFF)/(Salary).

library(dplyr)

# load data file, "eff-stats-salary.csv".
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
eff_stats_salary <- read.csv(
					  file = "../../data/cleandata/eff-stats-salary.csv",
                      sep = ",")

#Entire List of players in order of their values
rank_value <- eff_stats_salary %>%
  mutate (Value = EFF/Salary) %>%
  arrange (Value) %>%
  select (Player)

#Extracting only the top 20 and bottom 20 players
best <- head(rank_value, 20)
worst <- tail(rank_value, 20)
best_and_worst <- rbind (best, worst)

#Creating txt of the list of top 20 and bottom 20 players
write.table(
	best_and_worst,
	"../../data/cleandata/best-worst-value-players.txt",
	sep=",")

