# This file contains helper functions to be used by get-raw-data.R

library(XML)

# Gets the list of teams to use later
# @param url, the url of where to find the list of teams
# @return vector of class "character"
get_team_names <- function(url) {
  if (url == "") {
    stop("Please provide url")
  }
  team_html <- XML::htmlParse(url)
  
  # Retrieve team rows
  team_rows <- XML::getNodeSet(team_html, "//th[@scope='row']/a")
  team_urls <- XML::xmlSApply(team_rows, XML::xmlAttrs)

  # Only want abbreviations
  teams <- substr(team_urls, 8, 10)
  return(teams)
}

# Given the table id to look for, returns a data frame defining the player
# table
# @param html_lines, vector of character lines defining html document
# @param location, a character vector defining the table id to retrieve
# @return a data.frame containing player data
get_player_table <- function(html_lines, location) {
  if (class(html_lines) != "character" || location == "") {
    stop("Bad inputs. Please make sure xml_data is an xml_document and location
         is a valid non-empty string")
  }
  # Find bounds of table and create data frame
  id_location <- paste0('id="', location, '"')
  begin_table <- grep(id_location, html_lines)
  line_counter <- begin_table
  while (!grepl("</table>", html_lines[line_counter])) {
    line_counter <- line_counter + 1
  }
  player_frame <- readHTMLTable(html_lines[begin_table:line_counter],
                                header = TRUE,
                                stringsAsFactors = FALSE)
  return(player_frame)
}
