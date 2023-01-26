library(readxl)
library(readr)
library(tools)

#report period 
report_number <- "07"

#get working directory
x <- getwd()

#extract user string
slash_pos <- gregexpr("/", x, fixed=TRUE)
user <- substring(x, sapply(slash_pos, "[", 2) + 1, sapply(spots, "[", 3) - 1)

#create wd raw data folder path and sharepoint raw data folder path
sharepoint_folder <- paste0("C:/Users/", user, "/IS/Gazetteer Statistics - Automated Reports/Health Report ", report_number,"/Data/Raw Data (Input)")
raw_data_folder <- paste0(x, "/Cleaning Raw Data/Raw Data/")

#copy all data from the sharepoint folder to the wd
files <- list.files(path = sharepoint_folder)
for (file in files) {
  file.copy(from = paste0(sharepoint_folder, "/", file),
            to = paste0(raw_data_folder, file))
}
