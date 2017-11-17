# Set current working directory to the one containing download-data-script.R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rsconnect)

rsconnect::setAccountInfo(name='stat133',
                          token='D7DB0EAC608366366CE7F6B558B0903B',
                          secret='Z4WkijKeP2hDofaJRbWO7MNK1lULhXInZlSb5rcu')

# For these two lines to work properly, comment out the automatic directory
# changing command, move necessary files into the directory indicated below,
# then remove the path before the file names.

rsconnect::deployApp("project/apps/stat-salaries")

rsconnect::deployApp("project/apps/team-salaries")
